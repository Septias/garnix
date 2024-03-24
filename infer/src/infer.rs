use crate::{
    ast::{Ast, Inherit, PatternElement},
    types::{PolarVar, PolymorphicType, Type, Var},
    Context, ContextType, InferError, InferResult, SpannedError, SpannedInferResult, TypeName,
};
use itertools::{Either, Itertools};
use logos::Span;
use nom::error::context;
use parser::ast::BinOp;
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

fn constrain(context: &Context, lhs: &Type, rhs: &Type) -> InferResult<()> {
    constrain_inner(context, lhs, rhs, HashSet::new())
}

fn constrain_inner<'a>(
    context: &Context,
    lhs: &'a Type,
    rhs: &'a Type,
    mut cache: HashSet<(&'a Type, &'a Type)>,
) -> InferResult<()> {
    if lhs == rhs {
        return Ok(());
    }

    let lhs_rhs = (lhs, rhs);

    match (lhs, rhs) {
        (Type::Var(..), _) | (_, Type::Var(..)) => {
            if cache.contains(&lhs_rhs) {
                return Ok(());
            }
            cache.insert(lhs_rhs.clone());
        }
        _ => (),
    }

    match (lhs, rhs) {
        (Type::Function(l0, r0), Type::Function(l1, r1)) => {
            constrain_inner(context, l1, l0, cache.clone())?;
            constrain_inner(context, r0, r1, cache)?;
        }

        (Type::Record(fs0), Type::Record(fs1)) => {
            for (n1, t1) in fs1 {
                match fs0.iter().find(|(n0, _)| *n0 == n1) {
                    Some((_, t0)) => constrain_inner(context, t0, t1, cache.clone())?,
                    None => return Err(InferError::MissingRecordField { field: n1.clone() }),
                }
            }
        }

        (Type::Optional(o1), Type::Optional(o0)) => {
            constrain_inner(context, o0, o1, cache)?;
        }

        (Type::Bool, Type::Bool)
        | (Type::Number, Type::Number)
        | (Type::String, Type::String)
        | (Type::Path, Type::Path)
        | (Type::Null, Type::Null)
        | (Type::List(..), Type::List(..))
        | (Type::Undefined, Type::Undefined) => (),

        // application?
        // function constraints
        // selection
        (Type::Var(lhs), rhs) if rhs.level() <= lhs.level => {
            lhs.upper_bounds.borrow_mut().push(rhs.clone());
            for lower_bound in lhs.lower_bounds.borrow().iter() {
                constrain_inner(context, lower_bound, rhs, cache.clone())?;
            }
        }

        // let-binding
        // record typing
        (lhs, Type::Var(rhs)) if lhs.level() <= rhs.level => {
            rhs.lower_bounds.borrow_mut().push(lhs.clone());
            for upper_bound in rhs.upper_bounds.borrow().iter() {
                constrain_inner(context, lhs, upper_bound, cache.clone())?;
            }
        }
        (Type::Var(_), rhs) => {
            let rhs_extruded = extrude(context, rhs, false, lhs.level(), HashMap::new());
            constrain_inner(context, lhs, &rhs_extruded, cache)?;
        }
        (lhs, Type::Var(_)) => {
            let lhs_extruded = extrude(context, lhs, true, rhs.level(), HashMap::new());
            constrain_inner(context, &lhs_extruded, rhs, cache)?;
        }

        _ => {
            return Err(InferError::CannotConstrain {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            })
        }
    }

    Ok(())
}

fn extrude<'a>(
    context: &Context,
    ty: &'a Type,
    pol: bool,
    lvl: usize,
    mut c: HashMap<PolarVar, Var>,
) -> Type {
    if ty.level() <= lvl {
        return ty.clone();
    }

    match ty {
        Type::Number
        | Type::Bool
        | Type::String
        | Type::Path
        | Type::Null
        | Type::Pattern(..)
        | Type::Undefined => ty.clone(),

        Type::Var(vs) => {
            let pol_var = (vs.clone(), pol);
            if let Some(nvs) = c.get(&pol_var) {
                Type::Var(nvs.clone())
            } else {
                let nvs = context.fresh_var(lvl);
                c.insert(pol_var, nvs.clone()); // check

                if pol {
                    vs.upper_bounds.borrow_mut().push(Type::Var(nvs.clone()));
                    *nvs.lower_bounds.borrow_mut() = vs
                        .lower_bounds
                        .borrow()
                        .iter()
                        .map(|t| extrude(context, t, pol, lvl, c.clone()))
                        .collect();
                } else {
                    vs.lower_bounds.borrow_mut().push(Type::Var(nvs.clone()));
                    *nvs.upper_bounds.borrow_mut() = vs
                        .upper_bounds
                        .borrow()
                        .iter()
                        .map(|t| extrude(context, t, pol, lvl, c.clone()))
                        .collect();
                }

                Type::Var(nvs)
            }
        }

        Type::Function(l, r) => Type::Function(
            Box::new(extrude(context, l, !pol, lvl, c.clone())),
            Box::new(extrude(context, r, pol, lvl, c)),
        ),
        Type::Record(fs) => Type::Record(
            fs.iter()
                .map(|(name, t)| (name.clone(), extrude(context, t, pol, lvl, c.clone())))
                .collect(),
        ),

        Type::List(ls) => Type::List(
            ls.iter()
                .map(|t| extrude(context, t, pol, lvl, c.clone()))
                .collect(),
        ),
        Type::Optional(ty) => Type::Optional(Box::new(extrude(context, ty, pol, lvl, c))),
        Type::Top | Type::Bottom | Type::Union(..) | Type::Inter(..) | Type::Recursive(..) => {
            panic!("Not a simple type")
        }
    }
}

pub(crate) fn freshen_above(context: &Context, ty: &Type, lim: usize, lvl: usize) -> Type {
    freshen(context, &ty, lim, lvl, HashMap::new())
}

fn freshen<'a>(
    context: &Context,
    ty: &'a Type,
    lim: usize,
    lvl: usize,
    mut freshened: HashMap<&'a Var, Var>,
) -> Type {
    if ty.level() <= lim {
        return ty.clone();
    }

    match ty {
        Type::Var(var) => Type::Var(freshened.get(var).cloned().unwrap_or_else(|| {
            let new_v = context.fresh_var(lvl);
            let lower = var
                .lower_bounds
                .borrow()
                .iter()
                .map(|ty| freshen(context, ty, lim, lvl, freshened.clone()))
                .collect();
            let upper = var
                .upper_bounds
                .borrow()
                .iter()
                .map(|ty| freshen(context, ty, lim, lvl, freshened.clone()))
                .collect();
            *new_v.lower_bounds.borrow_mut() = lower;
            *new_v.upper_bounds.borrow_mut() = upper;
            freshened.insert(var, new_v.clone());
            new_v
        })),
        Type::Number
        | Type::Bool
        | Type::String
        | Type::Path
        | Type::Null
        | Type::Undefined
        | Type::Pattern(..) => ty.clone(),
        Type::Function(ty1, ty2) => Type::Function(
            Box::new(freshen(context, ty1, lim, lvl, freshened.clone())),
            Box::new(freshen(context, ty2, lim, lvl, freshened)),
        ),
        Type::List(list) => Type::List(
            list.iter()
                .map(|t| freshen(context, t, lim, lvl, freshened.clone()))
                .collect(),
        ),
        Type::Record(rc) => Type::Record(
            rc.iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        freshen(context, ty, lim, lvl, freshened.clone()),
                    )
                })
                .collect(),
        ),
        Type::Optional(opt) => Type::Optional(Box::new(freshen(context, opt, lim, lvl, freshened))),

        Type::Top | Type::Bottom | Type::Union(_, _) | Type::Inter(_, _) | Type::Recursive(..) => {
            unreachable!()
        }
    }
}

/// Infer the type of an expression.
fn type_term<'a>(ctx: &mut Context<'a>, term: &'a Ast, lvl: usize) -> Result<Type, SpannedError> {
    use Type::*;
    match term {
        Ast::Identifier(super::ast::Identifier { name, span, .. }) => ctx
            .lookup(name)
            .map(|val| val.instantiate(ctx, lvl))
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
                    )
                    .map_err(|e| e.span(lhs.get_span()))?;
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
                    )
                    .map_err(|e| e.span(lhs.get_span()))?;
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
                    let mut rc1 = ty1
                        .into_record()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rc2 = ty2
                        .into_record()
                        .map_err(|err| SpannedError::from((span, err)))?;

                    // overwrite first record with fields from second one
                    rc1.extend(rc2);

                    Ok(Type::Record(rc1))
                }

                // Primitives
                BinOp::Mul | BinOp::Div | BinOp::Sub => {
                    constrain(ctx, &ty1, &Type::Number).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Type::Number).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Type::Number)
                }
                BinOp::Add => {
                    if ty1 == Type::Number {
                        match &ty2 {
                            Type::Number => Ok(Type::Number),
                            Type::Var(_) => {
                                constrain(ctx, &ty2, &ty1).map_err(|e| e.span(rhs.get_span()))?;
                                Ok(Number)
                            }
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::Number,
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    } else if ty1 == Type::String {
                        match &ty2 {
                            Type::String | Type::Path => Ok(Type::String),
                            Type::Var(_) => {
                                constrain(ctx, &ty2, &ty1).map_err(|e| e.span(rhs.get_span()))?;
                                Ok(String)
                            }
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::String, // TODO: improve
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    } else if ty1 == Type::Path {
                        match &ty2 {
                            Type::String => Ok(Type::String),
                            Type::Var(_) => {
                                constrain(ctx, &ty2, &ty1).map_err(|e| e.span(rhs.get_span()))?;
                                Ok(Path)
                            }
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::Path, // TODO: improve
                                    found: ty2.get_name(),
                                },
                                span: span.clone(),
                            }),
                        }
                    } else if matches!(ty1, Type::Var(_)) {
                        todo!()
                    } else {
                        Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Number, // TODO: improve
                                found: ty1.get_name(),
                            },
                            span: span.clone(),
                        })
                    }
                }

                // Misc
                BinOp::HasAttribute => Ok(Bool),
                BinOp::AttributeFallback => {
                    // maybe typeunion?
                    todo!()
                }

                // Comparisons
                BinOp::LessThan
                | BinOp::LessThanEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanEqual
                | BinOp::Equal
                | BinOp::NotEqual => {
                    // TODO: type this as function f: α -> α -> Bool
                    Ok(Bool)
                }

                // Logical oprators
                BinOp::And | BinOp::Or | BinOp::Implication => {
                    constrain(ctx, &ty1, &Type::Bool).map_err(|e| e.span(lhs.get_span()))?;
                    constrain(ctx, &ty2, &Type::Bool).map_err(|e| e.span(rhs.get_span()))?;
                    Ok(Bool)
                }
            }
        }

        // Language constructs
        Ast::AttrSet {
            attrs,
            inherit,
            is_recursive,
            span,
        } => {
            if *is_recursive {
                let mut vars: Vec<_> = attrs
                    .iter()
                    .map(|(ident, expr)| {
                        (
                            ident.name.to_string(),
                            ContextType::Type(Type::Var(ctx.fresh_var(lvl))),
                        )
                    })
                    .collect();

                let ok = load_inherit(ctx, span.clone(), lvl, inherit)?;
                vars.extend(ok);

                ctx.with_scope(vars, |ctx| {
                    let (items, errs): (Vec<_>, Vec<_>) = attrs
                        .iter()
                        .map(|(ident, expr)| {
                            Ok::<_, SpannedError>((
                                ident.name.to_string(),
                                type_term(ctx, expr, lvl)?,
                            ))
                        })
                        .partition_map(|r| match r {
                            Ok(v) => Either::Left(v),
                            Err(v) => Either::Right(v),
                        });
                    if errs.is_empty() {
                        Ok(Type::Record(items.into_iter().collect()))
                    } else {
                        Err(SpannedError {
                            error: InferError::MultipleErrors(errs),
                            span: span.clone(),
                        })
                    }
                })
            } else {
                let mut vars: HashMap<_, _> = attrs
                    .iter()
                    .map(|(ident, expr)| {
                        (ident.name.to_string(), type_term(ctx, expr, lvl).unwrap())
                    })
                    .collect();
                let ok = load_inherit(ctx, span.clone(), lvl, inherit);
                vars.extend(
                    ok?.into_iter()
                        .map(|(name, ty)| (name.to_string(), ty.instantiate(ctx, lvl))),
                );
                Ok(Record(vars))
            }
        }

        Ast::LetBinding {
            bindings,
            inherit,
            body,
            span,
        } => {
            let binds = bindings
                .iter()
                .map(|(name, _)| {
                    (
                        name.name.to_string(),
                        ContextType::Type(Type::Var(ctx.fresh_var(lvl))),
                    )
                })
                .collect();

            let (ok, err): (Vec<_>, Vec<_>) = ctx
                .with_scope(binds, |ctx| {
                    let names = bindings.iter().map(|(name, _)| name.name.as_str());
                    let expressions = bindings.iter().map(|(_, expr)| expr);

                    names
                        .into_iter()
                        .zip(expressions)
                        .map(move |(name, rhs)| {
                            let e_ty = ctx.fresh_var(lvl + 1);
                            let ty = type_term(ctx, rhs, lvl + 1)?;
                            constrain(ctx, &ty, &Type::Var(e_ty.clone()))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ok((
                                name.to_string(),
                                ContextType::PolymorhicType(PolymorphicType::new(
                                    Type::Var(e_ty),
                                    lvl,
                                )),
                            ))
                        })
                        .collect_vec()
                })
                .into_iter()
                .partition_map(|r| match r {
                    Ok(v) => Either::Left(v),
                    Err(v) => Either::Right(v),
                });

            if !err.is_empty() {
                return Err(SpannedError {
                    error: InferError::MultipleErrors(err),
                    span: span.clone(),
                });
            }

            let ret = ctx.with_scope(ok, |ctx| type_term(ctx, body, lvl))?;
            Ok(ret)
        }

        Ast::Lambda {
            pattern,
            body,
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
                                item.push((ident.name.clone(), Type::Undefined));
                                added.push((ident.name.to_string(), ctx.fresh_context_var(lvl)));
                            }
                            PatternElement::DefaultIdentifier(name, expr) => {
                                let ty = type_term(ctx, expr, lvl)?;
                                let var = ctx.fresh_var(lvl);
                                constrain(ctx, &Type::Var(var.clone()), &ty)
                                    .map_err(|e| e.span(span))?;
                                item.push((name.name.clone(), ty));
                                added.push((name.name.to_string(), ContextType::Type(Type::Var(var))));
                            }
                        }
                    }

                    let ty = Type::Pattern(item.into_iter().collect(), *is_wildcard);
                    if let Some(name) = name {
                        if *is_wildcard {
                            todo!()
                        }
                        let var = ctx.fresh_var(lvl);
                        constrain(ctx, &Type::Var(var.clone()), &ty).map_err(|e| e.span(span))?;
                        added.push((name.to_string(), ContextType::Type(Type::Var(var))));
                    }
                    ty
                }
                crate::ast::Pattern::Identifier(_) => Type::Var(ctx.fresh_var(lvl)),
            };

            let ret = ctx.with_scope(added, |context| type_term(context, body, lvl))?;
            Ok(Function(Box::new(ty), Box::new(ret)))
        }

        Ast::With { set, body, span } => {
            let ty = type_term(ctx, set, lvl)?;
            match ty {
                var @ Type::Var(_) => {
                    ctx.set_with(var);
                    let ret = type_term(ctx, body, lvl);
                    ctx.remove_with();
                    ret
                }
                Type::Record(rc) => ctx.with_scope(
                    rc.iter()
                        .map(|(name, ty)| (name.to_string(), ContextType::Type(ty.clone())))
                        .collect(),
                    |ctx| type_term(ctx, body, lvl),
                ),
                _ => Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Record,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                }),
            }
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

fn load_inherit<'a>(
    ctx: &mut Context,
    span: Span,
    lvl: usize,
    inherit: &'a Vec<Inherit>,
) -> Result<Vec<(String, ContextType)>, SpannedError> {
    let (ok, err): (Vec<_>, Vec<_>) = inherit
        .into_iter()
        .map(|Inherit { name, items }| {
            if let Some(expr) = name {
                // load path
                let ty = type_term(ctx, expr, lvl)?;

                match &ty {
                    //
                    ty @ Type::Var(_) => {
                        let vars = items
                            .iter()
                            .map(|(_span, name)| (name.to_string(), Type::Var(ctx.fresh_var(lvl))));
                        let record = Type::Record(
                            vars.clone()
                                .map(|(name, ty)| (name, ty))
                                .collect(),
                        );
                        constrain(ctx, &ty, &record);
                        Ok(vars
                            .map(|(name, ty)| (name, ContextType::Type(ty)))
                            .collect())
                    }
                    Type::Record(rc_items) => {
                        let (ok, err): (Vec<_>, Vec<_>) = items
                            .iter()
                            .map(|(range, name)| {
                                Ok((
                                    name.to_string(),
                                    ContextType::Type(
                                        rc_items
                                            .get(name)
                                            .ok_or(
                                                InferError::MissingRecordField {
                                                    field: name.clone(),
                                                }
                                                .span(range),
                                            )?
                                            .clone(),
                                    ),
                                ))
                            })
                            .partition_map(|r| match r {
                                Ok(ty) => Either::Left(ty),
                                Err(e) => Either::Right(e),
                            });
                        if err.is_empty() {
                            Ok(ok)
                        } else {
                            Err(SpannedError {
                                error: InferError::MultipleErrors(err),
                                span: span.clone(),
                            })
                        }
                    }
                    _ => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Record,
                            found: ty.get_name(),
                        },
                        span: span.clone(),
                    }),
                }
            } else {
                let (ok, err): (Vec<_>, Vec<_>) = items
                    .iter()
                    .map(|(range, name)| {
                        Ok((
                            name.to_string(),
                            ctx.lookup(name)
                                .ok_or(InferError::UnknownIdentifier.span(range))?
                                .clone(),
                        ))
                    })
                    .partition_map(|r| match r {
                        Ok(v) => Either::Left(v),
                        Err(v) => Either::Right(v),
                    });

                if err.is_empty() {
                    Ok(ok)
                } else {
                    Err(SpannedError {
                        error: InferError::MultipleErrors(err),
                        span: span.clone(),
                    })
                }
            }
        })
        .partition_map(
            |r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            },
        );

    if !err.is_empty() {
        Err(SpannedError {
            error: InferError::MultipleErrors(err),
            span: span.clone(),
        })
    } else {
        Ok(ok.into_iter().flatten().collect())
    }
}

fn coalsce_type(ty: &Type) -> Type {
    coalsce_type_inner(ty, true, HashMap::new(), HashSet::new())
}

fn coalsce_type_inner(
    ty: &Type,
    polarity: bool,
    mut rec: HashMap<PolarVar, Var>,
    mut processing: HashSet<PolarVar>,
) -> Type {
    match ty {
        Type::Number | Type::Bool | Type::String | Type::Path | Type::Null | Type::Undefined => {
            ty.clone()
        }
        tyvar @ Type::Var(var) => {
            let pol_var = (var.clone(), polarity);
            if processing.contains(&pol_var) {
                return tyvar.clone();
            } else {
                processing.insert(pol_var.clone());
                let bounds = if polarity {
                    &var.lower_bounds
                } else {
                    &var.upper_bounds
                };
                let bound_types = bounds
                    .borrow()
                    .iter()
                    .map(|t| {
                        let mut processing = processing.clone();
                        processing.insert(pol_var.clone());

                        let t = coalsce_type_inner(t, polarity, rec.clone(), processing);
                        t
                    })
                    .collect_vec();
                let res = if polarity {
                    bound_types
                        .into_iter()
                        .reduce(|a, b| Type::Union(Box::new(a), Box::new(b)))
                } else {
                    bound_types
                        .into_iter()
                        .reduce(|a, b| Type::Inter(Box::new(a), Box::new(b)))
                };
                rec.get(&pol_var)
                    .map(|var| Type::Recursive(var.clone(), Box::new(res.unwrap())));
                todo!()
            }
        }
        Type::Function(l, r) => Type::Function(
            Box::new(coalsce_type_inner(
                l,
                !polarity,
                rec.clone(),
                processing.clone(),
            )),
            Box::new(coalsce_type_inner(r, polarity, rec, processing)),
        ),
        Type::List(l) => Type::List(
            l.iter()
                .map(|t| coalsce_type_inner(t, polarity, rec.clone(), processing.clone()))
                .collect(),
        ),
        Type::Record(r) => Type::Record(
            r.iter()
                .map(|(n, t)| {
                    (
                        n.clone(),
                        coalsce_type_inner(t, polarity, rec.clone(), processing.clone()),
                    )
                })
                .collect(),
        ),
        Type::Optional(o) => {
            Type::Optional(Box::new(coalsce_type_inner(o, polarity, rec, processing)))
        }
        _ => unreachable!(),
    }
}

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    type_term(&mut context, expr, 0)
}
