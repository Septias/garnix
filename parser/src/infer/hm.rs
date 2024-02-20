use super::{
    ast::Ast, Constraint, Context, InferError, InferResult, SpannedError, SpannedInferResult, Type,
    TypeName,
};
use crate::{
    ast::BinOp,
    infer::{ast::PatternElement, spanned_infer_error},
};
use anyhow::Context as _;
use logos::Span;
use std::collections::HashMap;

/// Lookup all bindings that are part of a `with`-expression and add them to the context.
/// This does not create a new scope
fn lookup_set_bindigs(context: &mut Context, bindings: &Ast) -> InferResult<()> {
    let attrs = bindings.as_attr_set()?;
    for (name, _expr) in attrs {
        let ty = context.lookup(name).ok_or(InferError::UnknownIdentifier)?;
        context.insert(*name, ty.clone());
    }
    Ok(())
}

/// Arguments:
/// - the function type
/// - the arguments that should be supplied to the function in form of application(lhs, application(lhs, ..))
fn reduce_function<'a>(
    function: &'a Type,
    arguments: &Ast,
    constraints: &mut Vec<Constraint>,
) -> Result<&'a Type, InferError> {
    let (from, to) = function.as_function().context("can't unpack function")?;

    // find out if this is a stacked function
    if matches!(to, Type::Function(_, _)) {
        // extract the last argument from the chain
        if let Ok((arg, _next)) = arguments.as_application() {
            // further reduce the function
            let ret = reduce_function(&to, arguments, constraints)?;

            // if it is an identifier we can formulate a constraint
            if let Ok(ident) = arg.as_ident() {
                constraints.push((ident, ret.clone()));
            }

            Ok(ret)
        } else {
            // If there is no argument to apply, just return a partial function
            Ok(function.as_function().context("can't unpack' function")?.1)
        }
    } else {
        if let Ok(ident) = arguments.as_ident() {
            constraints.push((ident, from.clone()));
        }
        Ok(to)
    }
}

/// Expect two types to be numerals.
fn expect_numerals(ty1: Type, ty2: Type, span: &Span) -> SpannedInferResult<Type> {
    use Type::*;
    if ty1 == Int {
        if ty2 == Int {
            Ok(Int)
        } else if ty2 == Float {
            Ok(Float)
        } else {
            spanned_infer_error(TypeName::Int, ty2.get_name(), span)
        }
    } else if ty1 == Float {
        if ty2 == Int || ty2 == Float {
            Ok(Float)
        } else {
            spanned_infer_error(TypeName::Float, ty2.get_name(), span)
        }
    } else {
        // TODO: this could be int or float
        spanned_infer_error(Int.get_name(), ty1.get_name(), span)
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
fn hm(context: &mut Context, expr: &Ast) -> Result<Type, SpannedError> {
    use Type::*;
    match expr {
        Ast::UnaryOp { rhs, .. } => hm(context, rhs),
        Ast::BinaryOp {
            op,
            lhs,
            box rhs,
            span,
        } => {
            let ty1 = hm(context, lhs)?;
            let ty2 = hm(context, rhs)?;

            match op {
                BinOp::Application => {
                    let fun = ty1
                        .as_ident()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let fun_type = context
                        .lookup(&fun.name)
                        .ok_or(SpannedError::from((span, InferError::UnknownFunction)))?;
                    if let Some(_path) = fun.path {
                        todo!()
                    }
                    let mut constraint = vec![];
                    let typ = reduce_function(fun_type, rhs, &mut constraint)
                        .map_err(|err| SpannedError::from((span, err)))?;
                    Ok(typ.clone())
                }
                BinOp::ListConcat => {
                    let lhs = ty1
                        .as_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    let rhs = ty2
                        .as_list()
                        .map_err(|err| SpannedError::from((span, err)))?;
                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Mul => expect_numerals(ty1, ty2, span),
                BinOp::Div => expect_numerals(ty1, ty2, span),
                BinOp::Sub => expect_numerals(ty1, ty2, span),
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
                        expect_numerals(ty1, ty2, span)
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
                BinOp::HasAttribute => {
                    if let Set(bindings) = ty1 {
                        if let Identifier(ident) = ty2 {
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
            is_recursive: _,
            span: _,
        } => Ok(Set(attrs
            .iter()
            .map(|(_name, expr)| ("".to_string(), hm(context, expr).unwrap()))
            .collect::<HashMap<_, _>>())),

        Ast::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            context.push_scope();
            for (name, expr) in bindings {
                let ty = hm(context, expr)?;
                context.insert(*name, ty);
            }
            if let Some(inherit) = inherit {
                for name in inherit {
                    let ty = context.lookup(name).ok_or(SpannedError {
                        error: InferError::UnknownIdentifier,
                        span: span.clone(),
                    })?; // Maybe don't make this a hard error
                    context.insert(*name, ty.clone());
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();
            Ok(ty)
        }
        Ast::Lambda {
            arguments,
            body,
            arg_binding: _,
            span,
        } => {
            context.push_scope();
            for patt in arguments {
                for patt in &patt.patterns {
                    match patt {
                        PatternElement::Identifier(name) => {
                            let ty = context.lookup(name).ok_or(SpannedError {
                                error: InferError::UnknownIdentifier,
                                span: span.clone(),
                            })?;
                            context.insert(*name, ty.clone());
                        }
                        PatternElement::DefaultIdentifier(name, default) => {
                            let ty1 = context.lookup(name).cloned();
                            let ty2 = hm(context, default)?;

                            if let Some(ty1) = ty1 {
                                if ty1 != ty2 {
                                    return Err(SpannedError {
                                        error: InferError::TypeMismatch {
                                            expected: ty2.get_name(),
                                            found: ty1.get_name(),
                                        },
                                        span: span.clone(),
                                    });
                                }
                            }

                            context.insert(*name, ty2);
                        }
                    }
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();

            // TODO: somehow create curry style functions here
            Ok(Function(Box::new(Undefined), Box::new(ty)))
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
            condition: _,
            span: _,
        } => todo!(),
        Ast::With { set, body, span } => {
            context.push_scope();
            lookup_set_bindigs(context, &set.as_ref().clone())
                .map_err(|err| SpannedError::from((span, err)))?;
            hm(context, body)
        }
        Ast::Identifier { debrujin, .. } => {
            Ok(context.lookup(debrujin).cloned().unwrap_or(Undefined))
        }
        Ast::List { items, span: _ } => Ok(Type::List(
            items.iter().flat_map(|ast| hm(context, ast)).collect(),
        )),
        Ast::NixString(_) => Ok(String),
        Ast::NixPath(_) => Ok(String),
        Ast::Bool { val: _, span: _ } => Ok(Bool),
        Ast::Int { val: _, span: _ } => Ok(Int),
        Ast::Float { val: _, span: _ } => Ok(Float),
        Ast::Null(_) => Ok(Null),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    }
}

/// Infer the type of an expression.
pub fn infer(expr: &Ast) -> SpannedInferResult<Type> {
    let mut context = Context::new();
    hm(&mut context, expr)
}