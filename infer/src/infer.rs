use crate::{
    ast::{Ast, Identifier, Pattern, PatternElement},
    types::{PolarVar, SimpleType, Type, Var},
    Context, InferError, InferResult, SpannedError, SpannedInferResult, TypeName,
};
use parser::ast::BinOp;
use std::collections::{HashMap, HashSet};

fn constrain(context: &mut Context, lhs: &Type, rhs: &Type) -> InferResult<()> {
    constrain_inner(context, lhs, rhs, &mut HashSet::new())
}

fn constrain_inner(context: &mut Context, lhs: &Type, rhs: &Type, cache: &mut HashSet<(&Type, &Type)>) -> InferResult<()> {
    if lhs == rhs {
        return Ok(());
    }

    let lhs_rhs = (lhs, rhs);

    match (lhs, rhs) {
        (Type::Var(..), _) | (_, Type::Var(..)) => {
            if cache.contains(&lhs_rhs) {
                return Ok(());
            }
            cache.insert(lhs_rhs);
        }
        _ => (),
    }

    match (lhs, rhs) {
        (Type::Function(l0, r0), Type::Function(l1, r1)) => {
            constrain_inner(context, l1, l0, cache)?;
            constrain_inner(context,r0, r1, cache)?;
        }
        (Type::Record(fs0), Type::Record(fs1)) => {
            for (n1, t1) in fs1 {
                match fs0.iter().find(|(n0, _)| *n0 == n1) {
                    Some((_, t0)) => constrain_inner(context,t0, t1, cache)?,
                    None => return Err(InferError::MissingRecordField { field: n1.clone() }),
                }
            }
        }
        (Type::Var(lhs), rhs) if rhs.level() <= lhs.level => {
            lhs.upper_bounds.push(rhs.clone());
            for lower_bound in &lhs.lower_bounds {
                constrain_inner(context,lower_bound, rhs, cache)?;
            }
        }
        (lhs, Type::Var(rhs)) if lhs.level() <= rhs.level => {
            rhs.lower_bounds.push(lhs.clone());
            for upper_bound in &rhs.upper_bounds {
                constrain_inner(context,lhs, upper_bound, cache)?;
            }
        }
        (Type::Var(_), rhs) => {
            let rhs_extruded = extrude(context, rhs, false, lhs.level(), &mut HashMap::new());
            constrain_inner(context,lhs, &rhs_extruded, cache)?;
        }
        (lhs, Type::Var(_)) => {
            let lhs_extruded = extrude(context, lhs, true, rhs.level(), &mut HashMap::new());
            constrain_inner(context,&lhs_extruded, rhs, cache)?;
        }

        // TODO: complete types
        _ => {
            return Err(InferError::CannotConstrain {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            })
        }
    }

    Ok(())
}


fn constrain_bools(
    context: &mut Context, 
    lhs: Type,
    rhs: Type,
) -> InferResult<()> {
    if lhs != Type::Bool && matches!(lhs, Type::Var(_)) {
        return Err(InferError::TypeMismatch {
            expected: TypeName::Number,
            found: lhs.get_name(),
        });
    }

    if rhs != Type::Bool && matches!(rhs, Type::Var(_)) {
        return Err(InferError::TypeMismatch {
            expected: TypeName::Number,
            found: rhs.get_name(),
        });
    }

    // TODO: right side for constraint?
    constrain(context,&lhs, &Type::Bool)?;
    constrain(context,&rhs, &Type::Bool)
}

struct Inferrer(usize);

fn extrude(context: &mut Context, ty: &Type, pol: bool, lvl: usize, c: &mut HashMap<Var, Var>) -> Type {
    if ty.level() <= lvl {
        return ty.clone();
    }

    match ty {
        t @ Type::Number
        | t @ Type::Bool
        | t @ Type::String
        | t @ Type::Path
        | t @ Type::Null
        | t @ Type::Undefined => ty.clone(),
        Type::Function(l, r) => Type::Function(
            Box::new(extrude(context, l, !pol, lvl, c)),
            Box::new(extrude(context, r, pol, lvl, c)),
        ),
        Type::Record(fs) => Type::Record(
            fs.iter()
                .map(|(name, t)| (name.clone(), extrude(context, t, pol, lvl, c)))
                .collect(),
        ),
        Type::Var(vs) => {
            if let Some(nvs) = c.get(vs) {
                Type::Var(nvs.clone())
            } else {
                let nvs = context.fresh_var(lvl);
                c.insert(vs.clone(), nvs.clone());

                if pol {
                    // Logic for adjusting upper bounds
                    // Placeholder: the actual manipulation depends on how bounds are represented and modified
                } else {
                    // Logic for adjusting lower bounds
                    // Placeholder: the actual manipulation depends on how bounds are represented and modified
                }

                Type::Var(nvs)
            }
        }
        Type::List(ls) => Type::List(ls.iter().map(|t| extrude(context, t, pol, lvl, c)).collect()),
        Type::Optional(ty) => Type::Optional(Box::new(extrude(context, ty, pol, lvl, c))),
        Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) | Type::Recursive(..) => {
            panic!("Not a simple type")
        }
    }
}

fn freshen_above(context: &mut Context, ty: &Type, lim: usize, lvl: usize) -> Type {
    let mut freshened = HashMap::new();
    freshen(context, &ty, lim, lvl, &mut freshened)

}

fn freshen(context: &mut Context, ty: &Type, lim: usize, lvl: usize, freshened: &mut HashMap<Var, Var>) -> Type {
    if ty.level() <= lim {
        return ty.clone()
    } 

    match ty {
        Type::Var(var) => {
            Type::Var(freshened.get(&var).cloned().unwrap_or_else(|| {
                let new_v = Var {
                    level: lvl,
                    id: context.count,
                    lower_bounds: var.lower_bounds.iter().rev().map(|ty| freshen(context, ty, lim, lvl, freshened)).rev().collect(),
                    upper_bounds: var.upper_bounds.iter().rev().map(|ty| freshen(context, ty, lim, lvl, freshened)).rev().collect(),
                };
                context.count += 1;
                freshened.insert(var.clone(), new_v.clone());
                new_v
            }))
        },
        Type::Number |
        Type::Bool |
        Type::String |
        Type::Path |
        Type::Null |
        Type::Undefined => ty.clone(),
        Type::Function(ty1, ty2) => Type::Function(
            Box::new(freshen(context, ty1, lim, lvl, freshened)),
            Box::new(freshen(context, ty2, lim, lvl, freshened)),
        ),
        Type::List(list) => Type::List(list.iter().map(|t| freshen(context, t, lim, lvl, freshened)).collect()),
        Type::Record(rc) => Type::Record(
            rc.iter().map(|(name, ty)| (name.clone(), freshen(context, ty, lim, lvl, freshened))).collect()
        ),
        Type::Optional(opt) => Type::Optional(Box::new(freshen(context, opt, lim, lvl, freshened))),
        
        
        Type::Top |
        Type::Bottom |
        Type::Union(_, _) |
        Type::Inter(_, _)  | Type::Recursive(..) => unreachable!(),
    }
}


/// Infer the type of an expression.
fn type_term<'a>(ctx: &mut Context<'a>, term: &'a Ast, lvl: usize) -> Result<Type, SpannedError> {
    use Type::*;
    match term {
        Ast::Identifier(super::ast::Identifier { name, span, .. }) => ctx
            .lookup(name)
            .map(|val| Type::Var(val))
            .ok_or(InferError::UnknownIdentifier.span(span)),

        Ast::UnaryOp { rhs, .. } => type_term(ctx, rhs, lvl),

        Ast::BinaryOp { op, lhs, rhs, span } => {
            let ty1 = type_term(ctx, lhs, lvl)?;
            let ty2 = type_term(ctx, rhs, lvl)?;

            match op {
                // Application etc.
                BinOp::Application => {
                    let res = Type::Var(ctx.fresh_var(lvl));
                    constrain(
                        ctx,
                        &ty1,
                        &Type::Function(Box::new(ty2), Box::new(res.clone())),
                    );
                    Ok(res)
                }
                BinOp::AttributeSelection => {
                    let res = Type::Var(ctx.fresh_var(lvl));

                    constrain(
                        ctx,
                        &ty1,
                        &Type::Record(
                            [(
                                rhs.as_identifier_str()
                                    .map_err(|e| e.span(lhs.get_span()))?,
                                res.clone(),
                            )]
                            .into_iter()
                            .collect(),
                        ),
                    );
                    Ok(res)
                }

                // Object modifications
                BinOp::ListConcat => {
                    let lhs = ty1
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rhs = ty2
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    
                    Ok(Type::List([lhs, rhs].concat()))
                }

                BinOp::Update => {
                    let rc1 = ty1
                        .into_record()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rc2 = ty2
                        .into_record()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    
                    // overwrite first record with fields from second one
                    rc1.extend(rc2);

                    Ok(Type::Record(rc1))
                },

                // Primitives
                BinOp::Mul | BinOp::Div | BinOp::Sub => {
                    constrain(ctx, &ty1, &Type::Number).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Type::Number).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Type::Number)
                }
                BinOp::Add => {
                    if ty1 == Type::Number {
                        match ty2 {
                            Type::Number => Ok(Type::Number),
                            Type::Var(var) => {constrain(ctx, &ty2, &ty1); Ok(Number)},
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::Number,
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    } else if ty1 == Type::String {
                        match ty2 {
                            Type::String | Type::Path => Ok(Type::String),
                            Type::Var(var) => {constrain(ctx, &ty2, &ty1); Ok(String)},
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::String, // TODO: improve
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    } 
                    else if ty1 == Type::Path {
                        match ty2 {
                            Type::String => Ok(Type::String),
                            Type::Var(var) => {constrain(ctx, &ty2, &ty1); Ok(Path)},
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::Path, // TODO: improve
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    }else {
                        Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Number, // TODO: improve
                                found: ty1.get_name(),
                            },
                            span: span.clone(),
                        })
                    }
                },

                // Misc
                BinOp::HasAttribute => Ok(Bool),
                BinOp::AttributeFallback => {
                    // maybe typeunion?
                    todo!()
                },

                // Comparisons
                BinOp::LessThan
                | BinOp::LessThanEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanEqual
                | BinOp::Equal
                | BinOp::NotEqual => {
                    constrain(ctx, &ty1, &Type::Bool).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Type::Bool).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Bool)
                },

                // Logical oprators
                BinOp::And | BinOp::Or | BinOp::Implication => todo!(),
            }
        }

        // Language constructs
        Ast::AttrSet {
            attrs,
            is_recursive, // TODO: handle recursiveness
            span,
        } => {
            /* if is_recursive {
            } else {
            } */
            let mut items: HashMap<_, _> = attrs
                .iter()
                .map(|(name, expr)| (name.name.to_string(), type_term(ctx, expr, lvl).unwrap())) // TODO: remove unwrap
                .collect();

            /* let (ok, err): (Vec<_>, Vec<_>) = inherit
                .iter()
                .map(|Inherit { name, items }| {
                    Result::<_, SpannedError>::Ok((
                        name.to_string(),
                        ctx.lookup_by_name(name)
                            .ok_or(InferError::UnknownIdentifier.span(range))?
                            .get_type() // TODO: does this have to copied too?
                            .unwrap_or_default(),
                    ))
                })
                .partition_map(|r| match r {
                    Ok(v) => Either::Left(v),
                    Err(v) => Either::Right(v),
                });

            if !err.is_empty() {
                return Err(InferError::MultipleErrors(err).span(span));
            }
            items.extend(ok.into_iter());*/
            Ok(Record(items))
        }
        Ast::LetBinding {
            bindings,
            body,
            span,
        } => {
            //let mut inherits = lookup_inherits(ctx, inherit).map_err(|e| e.span(span))?;
            //inherits.extend(bindings.iter().map(|(ident, _)| ident));
            let names: Vec<&Identifier> = bindings.iter().map(|(name, _)| name).collect();
            let expressions = bindings.iter().map(|(_, expr)| expr);
            ctx.with_scope(names, |context| {
                let types = expressions.map(|ast| type_term(context, ast, lvl));

                for (name, ty) in names.iter().zip(types) {
                    let ty = ty?;
                    constrain(ctx, &ty, &Type::Var(names))
                        .map_err(|e| e.span(&name.span))?
                }

                type_term(context, body, lvl)
            })
        }
        Ast::Lambda {
            pattern,
            body,
            arg_binding: _,
            span,
        } => {
            let mut added = vec![];
            let ty = match pattern {
                crate::ast::Pattern::Record {
                    patterns,
                    is_wildcard,
                    name,
                } => {
                    let mut item = vec![];

                    for pattern in patterns {
                        match pattern {
                            PatternElement::Identifier(ident) => {
                                item.push((ident, Type::Undefined));
                                added.push(ident);
                            }
                            PatternElement::DefaultIdentifier(name, expr) => {
                                let ty = type_term(ctx, expr, lvl)?;
                                constrain(ctx, &Type::Var(name), &ty);
                                item.push((name, ty));
                                added.push(name);
                            }
                        }
                    }

                    let ty = Type::Record(item.collect())
                    if let Some(Name) = name {
                        let var = ctx.fresh_var(lvl);
                        constrain(ctx, var, &ty);
                        added.push(var);
                    }
                    ty
                }
                Pattern::Identifier(ident) => Type::Var(ident),
            };

            let ret = ctx.with_scope(added, |context| type_term(context, body, lvl))?;
            Ok(Function(Box::new(ty), Box::new(ret)))
        }
        Ast::Conditional {
            condition,
            expr1,
            expr2,
            span,
        } => {
            let ty = type_term(ctx, condition, lvl)?;
            if ty != Type::Bool {
                return Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Bool,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                });
            }
            let ty1 = type_term(ctx, expr1, lvl)?;
            let ty2 = type_term(ctx, expr2, lvl)?;
            if ty1 != ty2 {
                Ok(Union(Box::new(ty1), Box::new(ty2)))
            } else {
                Ok(ty1)
            }
        }
        Ast::Assertion {
            condition,
            expr,
            span,
        } => {
            let ty = type_term(ctx, condition, lvl)?;
            if ty != Type::Bool || !matches!(ty, Type::Var(_)) {
                return Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Bool,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                });
            }
            type_term(ctx, expr, lvl)
        }

        Ast::With {
            box set,
            body,
            span,
        } => {
            // TODO: handle
            let ty = type_term(ctx, body, lvl);
            ty
        }

        Ast::List { exprs, span: _ } => Ok(Type::List(
            exprs
                .iter()
                .flat_map(|ast| type_term(ctx, ast, lvl))
                .collect(),
        )),

        // Primitives
        Ast::NixString(_) => Ok(String),
        Ast::NixPath(_) => Ok(Path),
        Ast::Null(_) => Ok(Null),
        Ast::Bool { .. } => Ok(Bool),
        Ast::Int { .. } | Ast::Float { .. } => Ok(Number),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    }
}

fn coalsce_type(ty: &Type) -> Type {
    let mut map = HashMap::new();
    let mut processing = HashSet::new();
    coalsce_type_inner(ty, true, &mut map, &mut processing)
}


fn coalsce_type_inner(ty: &Type, polarity: bool, rec: &mut HashMap<PolarVar, Var>, processing: &mut HashSet<PolarVar>) -> Type {
    match ty {
        Type::Number |
        Type::Bool |
        Type::String |
        Type::Path |
        Type::Null |
        Type::Undefined => ty.clone(),
        tyvar @ Type::Var(var) => {
            let pol_var = (var, polarity);
            if processing.contains(&pol_var) {
                return tyvar.clone()
            } else {
                processing.insert(pol_var);
                let bounds = if polarity {
                    &var.lower_bounds
                } else {
                    &var.upper_bounds
                };
                let bound_types = bounds.iter().map(|t| {
                    processing.insert(pol_var);
                    let t = coalsce_type_inner(t, polarity, rec, processing);
                    processing.remove(&pol_var);
                    t
                });
                let res = if polarity {
                    bound_types.reduce( |a, b| Type::Union(Box::new(a), Box::new(b)))
                } else {
                    bound_types.reduce( |a, b| Type::Inter(Box::new(a), Box::new(b)))
                };
                rec.get(&pol_var).map(|var| Type::Recursive(var.clone(), Box::new(res.unwrap())));
                todo!()
            }
        },
        Type::Function(l, r) => Type::Function(Box::new(coalsce_type_inner(ty, !polarity, rec, processing)), Box::new(coalsce_type_inner(ty, polarity, rec, processing))),
        Type::List(l) => Type::List(l.iter().map(|t| coalsce_type_inner(t, polarity, rec, processing)).collect()),
        Type::Record(r) => Type::Record(r.iter().map(|(n, t)| (n.clone(), coalsce_type_inner(t, polarity, rec, processing))).collect()),
        Type::Optional(o) => Type::Optional(Box::new(coalsce_type_inner(o, polarity, rec, processing))),
        _ => unreachable!(),
    }
}

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    type_term(&mut context, expr, 0)
}
