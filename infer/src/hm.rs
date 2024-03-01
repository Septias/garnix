use crate::{
    ast::{Ast, Identifier, PatternElement},
    spanned_infer_error, Context, InferError, InferResult, SpannedError, SpannedInferResult, Type,
    TypeName,
};
use itertools::{Either, Itertools};
use logos::Span;
use parser::ast::BinOp;
use std::collections::HashMap;

/// Lookup all bindings that are part of a `with`-expression and add them to the context.
fn introduce_set_bindigs<'a>(
    context: &Context<'a>,
    bindings: &'a Ast,
) -> InferResult<Vec<&'a Identifier>> {
    let binds = match bindings {
        Ast::AttrSet { attrs, inherit, .. } => {
            let mut binds = lookup_inherits(context, inherit)?;
            binds.extend(attrs.keys());
            binds
        }
        Ast::Identifier(ident) => {
            // TODO: actually insert bindings from this set
            let ident = context
                .lookup(ident.debrujin)
                .ok_or(InferError::UnknownIdentifier)?;
            vec![ident]
        }
        _ => panic!(),
    };
    Ok(binds)
}

/// Arguments:
/// - the function type
/// - the arguments that should be supplied to the function in form of application(lhs, application(lhs, ..))
fn reduce_function<'a>(
    context: &mut Context<'a>,
    function: Type,
    arguments: &'a Ast,
) -> SpannedInferResult<Type> {
    let (from, to) = function.into_function().expect("can't unpack function");

    // find out if this is a stacked function
    if matches!(to, Type::Function(_, _)) {
        // extract the last argument from the chain
        if let Ok((arg, next)) = arguments.as_application() {
            let ty = hm(context, arg)?;
            if from != ty {
                return Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: from.get_name(),
                        found: ty.get_name(),
                    },
                    span: arg.get_span().clone(),
                });
            }

            // further reduce the function
            let ret = reduce_function(context, to, next)?;

            // if it is an identifier we can formulate a constraint
            if let Ok(debrujin) = arg.as_debrujin() {
                let identi = context.lookup(debrujin).ok_or(SpannedError {
                    error: InferError::UnknownIdentifier,
                    span: arg.get_span().clone(),
                })?;
                identi.add_constraint(from.clone())
            }
            Ok(ret)
        } else {
            // If there is no argument to apply, just return a partial function
            Ok(from)
        }
    } else {
        let ty = hm(context, arguments)?;

        if from != ty {
            return Err(SpannedError {
                error: InferError::TypeMismatch {
                    expected: from.get_name(),
                    found: ty.get_name(),
                },
                span: arguments.get_span().clone(),
            });
        }

        if let Ok(debrujin) = arguments.as_debrujin() {
            let identi = context.lookup(debrujin).ok_or(SpannedError {
                error: InferError::UnknownIdentifier,
                span: arguments.get_span().clone(),
            })?;
            identi.add_constraint(from.clone())
        }
        Ok(to)
    }
}

/// Expect two types to be numerals.
fn expect_numerals(ty1: Type, ty2: Type, span1: &Span, span2: &Span) -> SpannedInferResult<Type> {
    use Type::*;
    if ty1 == Int {
        if ty2 == Int {
            Ok(Int)
        } else if ty2 == Float {
            Ok(Float)
        } else {
            spanned_infer_error(Number.get_name(), ty2.get_name(), span2)
        }
    } else if ty1 == Float {
        if ty2 == Int || ty2 == Float {
            Ok(Float)
        } else {
            spanned_infer_error(TypeName::Number, ty2.get_name(), span2)
        }
    } else {
        spanned_infer_error(Number.get_name(), ty1.get_name(), span1)
    }
}

/// Expect two types to be booleans.
fn expect_bools(ty1: Type, ty2: Type, span: &Span) -> SpannedInferResult<Type> {
    use Type::*;
    if ty1 == Bool && ty2 == Bool {
        Ok(Bool)
    } else if ty1 != Bool {
        spanned_infer_error(TypeName::Bool, ty1.get_name(), span)
    } else {
        spanned_infer_error(TypeName::Bool, ty2.get_name(), span)
    }
}

/// Infer the type of an expression.
fn hm<'a>(context: &mut Context<'a>, expr: &'a Ast) -> Result<Type, SpannedError> {
    use Type::*;
    match expr {
        Ast::UnaryOp { rhs, .. } => hm(context, rhs),
        Ast::BinaryOp {
            op,
            lhs,
            rhs,
            span,
        } => {
            let ty1 = hm(context, lhs)?;

            if op == &BinOp::HasAttribute {
                if let Set(bindings) = ty1 {
                    if let Ok(ident) = rhs.get_identifier() {
                        return Ok(if bindings.get(&ident.name).is_some() {
                            Bool
                        } else {
                            Null
                        });
                    } else {
                        return spanned_infer_error(
                            TypeName::Identifier,
                            hm(context, rhs)?.get_name(),
                            span,
                        );
                    }
                } else {
                    return spanned_infer_error(TypeName::Set, ty1.get_name(), span);
                }
            }

            let ty2 = hm(context, rhs)?;

            match op {
                BinOp::Application => {
                    let fun = ty1
                        .into_debrujin()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let fun_type = context
                        .lookup_type(fun)
                        .ok_or(SpannedError::from((span, InferError::UnknownFunction)))?;
                    let typ = reduce_function(context, fun_type, rhs)?;
                    Ok(typ.clone())
                }
                BinOp::ListConcat => {
                    let lhs = ty1
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rhs = ty2
                        .into_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Mul => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::Div => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::Sub => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::Add => {
                    if ty1 == String && ty2 == String {
                        Ok(String)
                    } else if ty1 == Path && ty2 == String {
                        Ok(Path)
                    } else if ty1 == String && ty2 == Path {
                        Ok(Path)
                    } else if ty1 == Path && ty2 == Path {
                        Ok(Path)
                    } else {
                        expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span())
                    }
                }
                BinOp::Update => {
                    if let Set(mut bindings) = ty1 {
                        if let Set(new_bindings) = ty2 {
                            bindings.extend(new_bindings);
                            Ok(Set(bindings))
                        } else {
                            spanned_infer_error(TypeName::Set, ty2.get_name(), span)
                        }
                    } else {
                        spanned_infer_error(TypeName::Set, ty1.get_name(), span)
                    }
                }
                BinOp::HasAttribute => panic!(),
                BinOp::AttributeSelection => {
                    if let Set(bindings) = ty1 {
                        if let Identifier(ident) = ty2 {
                            if let Some(ty) = bindings.get(&ident.name.to_string()) {
                                Ok(ty.clone())
                            } else {
                                Ok(Undefined)
                            }
                        } else {
                            spanned_infer_error(TypeName::Identifier, ty2.get_name(), span)
                        }
                    } else {
                        spanned_infer_error(TypeName::Set, ty1.get_name(), span)
                    }
                }
                BinOp::AttributeFallback => {
                    if ty1 == Null {
                        Ok(ty2)
                    } else {
                        Ok(ty1)
                    }
                }
                BinOp::LessThan => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::LessThanEqual => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::GreaterThan => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
                BinOp::GreaterThanEqual => {
                    expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span())
                }
                BinOp::Equal => expect_numerals(ty1, ty2, lhs.get_span(), rhs.get_span()),
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
            let mut attrs: HashMap<_, _> = attrs
                .iter()
                .map(|(name, expr)| (name.name.to_string(), hm(context, expr).unwrap()))
                .collect();

            let (ok, err): (Vec<_>, Vec<_>) = inherit
                .iter()
                .map(|(name, range)| {
                    Result::<_, SpannedError>::Ok((
                        name.to_string(),
                        context
                            .lookup_by_name(name)
                            .ok_or(InferError::UnknownIdentifier.span(range))?
                            .get_type()
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
            attrs.extend(ok.into_iter());
            Ok(Set(attrs))
        }
        Ast::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            let mut inherits = lookup_inherits(context, inherit).map_err(|e| e.span(span))?;
            inherits.extend(bindings.iter().map(|(ident, _)| ident));
            context.with_scope(inherits, |context| {
                for (ident, expr) in bindings {
                    let ty = hm(context, expr)?;
                    ident.set_type(ty);
                }
                hm(context, body)
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
                            let ty2 = hm(context, default)?;
                            ident.add_constraint(ty2.clone());
                            ident
                        }
                    })
                }
            }

            let ty = context.with_scope(items.clone(), |context| hm(context, body))?;
            let first = items
                .pop()
                .ok_or(InferError::TooFewArguments.span(span))?
                .get_type()
                .unwrap_or_default()
                .clone();
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
            let ty = hm(context, condition)?;
            if ty != Bool {
                return Err(SpannedError {
                    error: InferError::TypeMismatch {
                        expected: TypeName::Bool,
                        found: ty.get_name(),
                    },
                    span: span.clone(),
                });
            }
            let ty1 = hm(context, expr1)?;
            let ty2 = hm(context, expr2)?;
            if ty1 != ty2 {
                Ok(Union(Box::new(ty1), Box::new(ty2)))
            } else {
                Ok(ty1)
            }
        }
        Ast::Assertion {
            condition,
            span: _,
            expr,
        } => {
            hm(context, &condition)?;
            hm(context, expr)
        }
        Ast::With {
            set,
            body,
            span,
        } => {
            let ty = context.with_scope(
                introduce_set_bindigs(context, set)
                    .map_err(|err| SpannedError::from((span, err)))?,
                |context| hm(context, body),
            );
            ty
        }
        Ast::Identifier(super::ast::Identifier { debrujin, .. }) => {
            Ok(context.lookup_type(*debrujin).unwrap_or(Undefined))
        }
        Ast::List { exprs, span: _ } => Ok(Type::List(
            exprs.iter().flat_map(|ast| hm(context, ast)).collect(),
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

/// Infer the type of an expression.
/// Insert constraints for all [Identifier]s on the way.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    hm(&mut context, expr)
}
