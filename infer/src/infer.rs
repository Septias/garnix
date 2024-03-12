use crate::{
    ast::{Ast, Identifier, PatternElement},
    spanned_infer_error,
    types::{SimpleType, Type, Var},
    Context, InferError, InferResult, SpannedError, SpannedInferResult, TypeName,
};
use itertools::{Either, Itertools};
use logos::Span;
use parser::ast::BinOp;
use std::collections::{HashMap, HashSet};

/// Lookup all bindings that are part of a `with`-expression and add them to the context.
/// This creates a new scope.
fn introduce_set_bindigs<'a>(context: &mut Context<'a>, bindings: &'a Ast) -> InferResult<()> {
    let (bindings, inherit) = bindings.as_attr_set()?;
    context.insert(bindings.keys().collect());
    context.insert(lookup_inherits(context, inherit)?);
    Ok(())
}

/// Lookup inherits from the context and return the ones that existed.
fn lookup_inherits<'a>(
    context: &Context<'a>,
    inherit: &[(String, Span)],
) -> InferResult<Vec<&'a Identifier>> {
    let (ok, err): (Vec<_>, Vec<_>) = inherit
        .iter()
        .map(|(inherit, span)| {
            context
                .lookup_by_name(inherit)
                .ok_or(InferError::UnknownInherit.span(span))
        })
        .partition_map(|r| match r {
            Ok(v) => Either::Left(v),
            Err(v) => Either::Right(v),
        });
    if !err.is_empty() {
        return Err(InferError::MultipleErrors(err));
    }
    Ok(ok)
}

fn constrain(lhs: &Type, rhs: &Type, cache: &mut HashSet<(&Type, &Type)>) -> InferResult<()> {
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
            constrain(l1, l0, cache)?;
            constrain(r0, r1, cache)?;
        }
        (Type::Record(fs0), Type::Record(fs1)) => {
            for (n1, t1) in fs1 {
                match fs0.iter().find(|(n0, _)| *n0 == n1) {
                    Some((_, t0)) => constrain(t0, t1, cache)?,
                    None => return Err(InferError::MissingRecordField { field: n1.clone() }),
                }
            }
        }
        (Type::Var(lhs), rhs) if rhs.level() <= lhs.level => {
            lhs.upper_bounds.push(rhs.clone());
            for lower_bound in &lhs.lower_bounds {
                constrain(lower_bound, rhs, cache)?;
            }
        }
        (lhs, Type::Var(rhs)) if lhs.level() <= rhs.level => {
            rhs.lower_bounds.push(lhs.clone());
            for upper_bound in &rhs.upper_bounds {
                constrain(lhs, upper_bound, cache)?;
            }
        }
        (Type::Var(_), rhs) => {
            let rhs_extruded = extrude(rhs, false, lhs.level(), &mut HashMap::new());
            constrain(lhs, &rhs_extruded, cache)?;
        }
        (lhs, Type::Var(_)) => {
            let lhs_extruded = extrude(lhs, true, rhs.level(), &mut HashMap::new());
            constrain(&lhs_extruded, rhs, cache)?;
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

struct Inferrer(usize);

fn extrude(ty: &Type, pol: bool, lvl: usize, c: &mut HashMap<Var, Var>) -> Type {
    if ty.level() <= lvl {
        return ty.clone();
    }

    match ty {
        Type::Primitive(_) => ty.clone(),
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
    let cache = &mut HashSet::new();
    use Type::*;
    match term {
        Ast::UnaryOp { rhs, .. } => type_term(ctx, rhs, lvl),
        Ast::BinaryOp { op, lhs, rhs, span } => {
            let ty1 = type_term(ctx, lhs, lvl)?;
            let ty2 = type_term(ctx, rhs, lvl)?;

            match op {
                BinOp::Application => {
                    let res = fresh_var();
                    constrain(
                        &ty1,
                        &Type::Function(Box::new(ty2), Box::new(res.clone())),
                        cache,
                    );
                    Ok(res)
                }
                BinOp::ListConcat => {
                    if is_var_and((&ty1, &ty2), TypeName::List) {
                        constrain(&ty1, &ty2, cache);
                    }

                    let lhs = ty1
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rhs = ty2
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;

                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Mul => constrain(lhs, rhs, cache),
                BinOp::Div => expect_numerals(ty1, ty2, span),
                BinOp::Sub => expect_numerals(ty1, ty2, span),
                BinOp::Add => expect_numerals(ty1, ty2, span),
                BinOp::Update => constrain_update(),
                BinOp::HasAttribute => {
                    if let Record(bindings) = ty1 {
                        if let Var(ident) = ty2 {
                            if bindings.get(&ident.name.to_string()).is_some() {
                                Ok(Bool)
                            } else {
                                Ok(Null)
                            }
                        } else {
                            spanned_infer_error(TypeName::Identifier, ty2.get_name(), span)
                        }
                    } else {
                        spanned_infer_error(TypeName::Set, ty1.get_name(), span)
                    }
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
                        cache,
                    );
                    Ok(res)
                }
                BinOp::AttributeFallback => {
                    if ty1 == Null {
                        Ok(ty2)
                    } else {
                        Ok(ty1)
                    }
                }
                BinOp::LessThan => expect_numerals(ty1, ty2, span),
                BinOp::LessThanEqual => expect_numerals(ty1, ty2, span),
                BinOp::GreaterThan => expect_numerals(ty1, ty2, span),
                BinOp::GreaterThanEqual => expect_numerals(ty1, ty2, span),
                BinOp::Equal => expect_numerals(ty1, ty2, span),
                BinOp::NotEqual => expect_bools(ty1, ty2, span),
                BinOp::And => expect_bools(ty1, ty2, span),
                BinOp::Or => expect_bools(ty1, ty2, span),
                BinOp::Implication => expect_bools(ty1, ty2, span),
            }
        }
        Ast::AttrSet {
            attrs,
            is_recursive: _, // TODO: handle recursiveness
            span,
            inherit,
        } => {
            let mut items: HashMap<_, _> = attrs
                .iter()
                .map(|(name, expr)| (name.name.to_string(), type_term(ctx, expr).unwrap())) // TODO: remove unwrap
                .collect();

            let (ok, err): (Vec<_>, Vec<_>) = inherit
                .iter()
                .map(|(name, range)| {
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
            items.extend(ok.into_iter());
            Ok(Record(items))
        }
        Ast::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            let mut inherits = lookup_inherits(ctx, inherit).map_err(|e| e.span(span))?;
            inherits.extend(bindings.iter().map(|(ident, _)| ident));
            ctx.with_scope(inherits, |context| {
                for (ident, expr) in bindings {
                    let ty = type_term(context, expr)?;
                    ident.set_type(ty);
                }
                type_term(context, body)
            })
        }
        Ast::Lambda {
            arguments,
            body,
            arg_binding: _,
            span,
        } => {
            let mut items = vec![];
            for patt in arguments {
                for patt in &patt.patterns {
                    items.push(match patt {
                        PatternElement::Identifier(ident) => ident,
                        PatternElement::DefaultIdentifier(ident, default) => {
                            let ty2 = type_term(ctx, default)?;
                            ident.add_ub(ty2.clone());
                            ident
                        }
                    })
                }
            }

            let ty = ctx.with_scope(items.clone(), |context| type_term(context, body))?;
            let first = items
                .pop()
                .and_then(|i| i.get_type().clone())
                .ok_or(InferError::TooFewArguments.span(span))?;
            Ok(items
                .into_iter()
                .fold(Function(Box::new(first), Box::new(ty)), |acc, elem| {
                    Function(Box::new(elem.get_type().unwrap().clone()), Box::new(acc))
                }))
        }
        Ast::Conditional {
            condition,
            expr1,
            expr2,
            span,
        } => {
            let ty = type_term(ctx, condition)?;
            if ty != Bool {
                return Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Bool,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                });
            }
            let ty1 = type_term(ctx, expr1)?;
            let ty2 = type_term(ctx, expr2)?;
            if ty1 != ty2 {
                Ok(Union(Box::new(ty1), Box::new(ty2)))
            } else {
                Ok(ty1)
            }
        }
        Ast::Assertion {
            condition: _,
            span: _,
            expr: _,
        } => todo!(),

        Ast::With {
            box set,
            body,
            span,
        } => {
            ctx.push_scope(vec![]);
            introduce_set_bindigs(ctx, set).map_err(|err| SpannedError::from((span, err)))?;
            let ty = type_term(ctx, body);
            ctx.pop_scope();
            ty
        }
        Ast::Identifier(super::ast::Identifier { debrujin, span, .. }) => ctx
            .lookup_type(*debrujin)
            .ok_or(InferError::UnknownIdentifier.span(span)),
        Ast::List { exprs, span: _ } => Ok(Type::List(
            exprs.iter().flat_map(|ast| type_term(ctx, ast)).collect(),
        )),
        Ast::NixString(_) => Ok(String),
        Ast::NixPath(_) => Ok(Path),
        Ast::Bool { val: _, span: _ } => Ok(Bool),
        Ast::Int { val: _, span: _ } => Ok(Int),
        Ast::Float { val: _, span: _ } => Ok(Float),
        Ast::Null(_) => Ok(Null),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    }
}

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    type_term(&mut context, expr)
}
