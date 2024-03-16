use crate::{
    ast::{Ast, Identifier, Inherit, Pattern, PatternElement},
    spanned_infer_error,
    types::{SimpleType, Type, Var},
    Context, InferError, InferResult, SpannedError, SpannedInferResult, TypeName,
};
use itertools::{Either, Itertools};
use logos::Span;
use parser::ast::BinOp;
use std::collections::{HashMap, HashSet};

fn constrain(lhs: &Type, rhs: &Type) -> InferResult<()> {
    constrain_inner(lhs, rhs, &mut HashSet::new())
}


fn constrain_inner(lhs: &Type, rhs: &Type, cache: &mut HashSet<(&Type, &Type)>) -> InferResult<()> {
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
            constrain_inner(l1, l0, cache)?;
            constrain_inner(r0, r1, cache)?;
        }
        (Type::Record(fs0), Type::Record(fs1)) => {
            for (n1, t1) in fs1 {
                match fs0.iter().find(|(n0, _)| *n0 == n1) {
                    Some((_, t0)) => constrain_inner(t0, t1, cache)?,
                    None => return Err(InferError::MissingRecordField { field: n1.clone() }),
                }
            }
        }
        (Type::Var(lhs), rhs) if rhs.level() <= lhs.level => {
            lhs.upper_bounds.push(rhs.clone());
            for lower_bound in &lhs.lower_bounds {
                constrain_inner(lower_bound, rhs, cache)?;
            }
        }
        (lhs, Type::Var(rhs)) if lhs.level() <= rhs.level => {
            rhs.lower_bounds.push(lhs.clone());
            for upper_bound in &rhs.upper_bounds {
                constrain_inner(lhs, upper_bound, cache)?;
            }
        }
        (Type::Var(_), rhs) => {
            let rhs_extruded = extrude(rhs, false, lhs.level(), &mut HashMap::new());
            constrain_inner(lhs, &rhs_extruded, cache)?;
        }
        (lhs, Type::Var(_)) => {
            let lhs_extruded = extrude(lhs, true, rhs.level(), &mut HashMap::new());
            constrain_inner(&lhs_extruded, rhs, cache)?;
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

fn constrain_numerals(
    lhs: Type,
    rhs: Type,
) -> InferResult<()> {
    if lhs != Type::Number && matches!(lhs, Type::Var(_)) {
        return Err(InferError::TypeMismatch {
            expected: TypeName::Number,
            found: lhs.get_name(),
        });
    }

    if rhs != Type::Number && matches!(rhs, Type::Var(_)) {
        return Err(InferError::TypeMismatch {
            expected: TypeName::Number,
            found: rhs.get_name(),
        });
    }

    // TODO: right side for constraint?
    constrain(&lhs, &Type::Number)?;
    constrain(&rhs, &Type::Number)
}

struct Inferrer(usize);

fn extrude(ty: &Type, pol: bool, lvl: usize, c: &mut HashMap<Var, Var>) -> Type {
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
            Box::new(extrude(l, !pol, lvl, c)),
            Box::new(extrude(r, pol, lvl, c)),
        ),
        Type::Record(fs) => Type::Record(
            fs.iter()
                .map(|(name, t)| (name.clone(), extrude(t, pol, lvl, c)))
                .collect(),
        ),
        Type::Var(vs) => {
            if let Some(nvs) = c.get(vs) {
                Type::Var(nvs.clone())
            } else {
                let nvs = fresh_var();
                c.insert(vs.clone(), nvs.get_var().unwrap().clone());

                if pol {
                    // Logic for adjusting upper bounds
                    // Placeholder: the actual manipulation depends on how bounds are represented and modified
                } else {
                    // Logic for adjusting lower bounds
                    // Placeholder: the actual manipulation depends on how bounds are represented and modified
                }

                nvs
            }
        }
        Type::List(ls) => Type::List(ls.iter().map(|t| extrude(t, pol, lvl, c)).collect()),
        Type::Optional(ty) => Type::Optional(Box::new(extrude(ty, pol, lvl, c))),
        Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) => {
            panic!("Not a simple type")
        }
    }
}

// TODO: don't reuse level
fn fresh_var() -> Type {
    Type::Var(Var {
        lower_bounds: vec![],
        upper_bounds: vec![],
        debrujin: 0,
        level: 0,
    })
}

fn is_var_and(tup: (&Type, &Type), ty: TypeName) -> bool {
    match tup {
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        _ => false,
    }
}

/// Infer the type of an expression.
fn type_term<'a>(ctx: &mut Context<'a>, term: &'a Ast, lvl: usize) -> Result<Type, SpannedError> {
    use Type::*;
    match term {
        Ast::Identifier(super::ast::Identifier { debrujin, span, .. }) => ctx
            .lookup_type(*debrujin)
            .ok_or(InferError::UnknownIdentifier.span(span)),

        Ast::UnaryOp { rhs, .. } => type_term(ctx, rhs, lvl),

        Ast::BinaryOp { op, lhs, rhs, span } => {
            let ty1 = type_term(ctx, lhs, lvl)?;
            let ty2 = type_term(ctx, rhs, lvl)?;

            match op {
                // Application etc.
                BinOp::Application => {
                    let res = fresh_var();
                    constrain(
                        &ty1,
                        &Type::Function(Box::new(ty2), Box::new(res.clone())),
                    );
                    Ok(res)
                }
                BinOp::AttributeSelection => {
                    let res = fresh_var();
                    constrain(
                        &ty1,
                        &Type::Record(
                            [(
                                lhs.as_identifier_str()
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
                    if is_var_and((&ty1, &ty2), TypeName::List) {
                        constrain(&ty1, &ty2);
                    }
                    let lhs = ty1
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rhs = ty2
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;

                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Update => todo!(),

                // Primitives
                BinOp::Mul | BinOp::Div | BinOp::Sub => {
                    constrain_numerals(ty1, ty2).map_err(|err| err.span(term.get_span()))?;
                    Ok(Type::Number)
                }
                BinOp::Add => todo!(),

                // Misc
                BinOp::HasAttribute => todo!(),
                BinOp::AttributeFallback => todo!(),

                // Comparisons
                BinOp::LessThan
                | BinOp::LessThanEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanEqual
                | BinOp::Equal
                | BinOp::NotEqual => todo!(),

                // Logical oprators
                BinOp::And | BinOp::Or | BinOp::Implication => todo!(),
            }
        }

        // Language constructs
        Ast::AttrSet {
            attrs,
            is_recursive, // TODO: handle recursiveness
            span,
            inherit,
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
            inherit,
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
                    constrain(&ty, &Type::Var(names))
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
                                constrain(&Type::Var(name), &ty);
                                item.push((name, ty));
                                added.push(name);
                            }
                        }
                    }

                    let ty = Type::Record(item.collect())
                    if let Some(Name) = name {
                        let var = new_var();
                        constrain(var, &ty)
                        added.push(var)
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
            if ty != Type::Bool {
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
        Ast::Bool { val: _, span: _ } => Ok(Bool),
        Ast::Int { val: _, span: _ } => Ok(Number),
        Ast::Float { val: _, span: _ } => Ok(Number),
        Ast::Null(_) => Ok(Null),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    }
}

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    type_term(&mut context, expr, 0)
}
