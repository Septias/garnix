use std::collections::HashMap;

use anyhow::Context as _;

use crate::{
    ast::BinOp,
    infer::{ast::PatternElement, infer_error, Ident},
};

use super::{ast::Ast, Constraint, Context, InferError, InferResult, Type};

/// Lookup all bindings that are part of a `with`-expression and add them to the context.
/// This does not create a new scope
fn lookup_set_bindigs(context: &mut Context, bindings: &Ast) -> InferResult<()> {
    let attrs = bindings.as_attr_set()?;
    for (name, _expr) in attrs {
        let ty = context
            .lookup(name)
            .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
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
) -> InferResult<&'a Type> {
    let (from, to) = function.as_function().context("can't unpack function")?;

    // find out if this is a stacked function
    if matches!(to, Type::Function(_, _)) {
        // extract the last argument from the chain
        if let Ok((arg, _next)) = arguments.as_application() {
            // further reduce the function
            let ret = reduce_function(to, arguments, constraints)?;

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

fn expect_numerals(ty1: Type, ty2: Type) -> InferResult<Type> {
    use Type::*;
    if ty1 == Int {
        if ty2 == Int {
            Ok(Int)
        } else if ty2 == Float {
            Ok(Float)
        } else {
            infer_error(Int, ty2.clone())
        }
    } else if ty1 == Float {
        if ty2 == Int || ty2 == Float {
            Ok(Float)
        } else {
            infer_error(Float, ty2.clone())
        }
    } else {
        // TODO: this could be int or float
        infer_error(Int, ty1.clone())
    }
}

fn expect_bools(ty1: Type, ty2: Type) -> InferResult<Type> {
    use Type::*;
    if ty1 == Bool && ty2 == Bool {
        Ok(Bool)
    } else if ty1 != Bool {
        infer_error(Bool, ty1)
    } else {
        infer_error(Bool, ty2)
    }
}

/// Infer the type of an expression.
fn hm(context: &mut Context, expr: &Ast) -> Result<Type, InferError> {
    use Type::*;
    match expr {
        Ast::UnaryOp { rhs, .. } => hm(context, rhs),
        Ast::BinaryOp {
            op,
            lhs,
            box rhs,
            span: _,
        } => {
            let ty1 = hm(context, lhs)?;
            let ty2 = hm(context, rhs)?;

            match op {
                BinOp::Application => {
                    let fun = ty1.as_ident()?;
                    let fun_type = context
                        .lookup(&fun.name)
                        .ok_or(InferError::UnknownFunction)?;
                    if let Some(_path) = fun.path {
                        todo!()
                    }
                    let mut constraint = vec![];
                    let typ = reduce_function(fun_type, rhs, &mut constraint)?;
                    Ok(typ.clone())
                }
                BinOp::ListConcat => {
                    let lhs = ty1.as_list()?;
                    let rhs = ty2.as_list()?;
                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Mul => expect_numerals(ty1, ty2),
                BinOp::Div => expect_numerals(ty1, ty2),
                BinOp::Sub => expect_numerals(ty1, ty2),
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
                        expect_numerals(ty1, ty2)
                    }
                }
                BinOp::Update => {
                    if let Set(mut bindings) = ty1 {
                        if let Set(new_bindings) = ty2 {
                            bindings.extend(new_bindings);
                            Ok(Set(bindings))
                        } else {
                            infer_error(Set(HashMap::new()), ty2)
                        }
                    } else {
                        infer_error(Set(HashMap::new()), ty1)
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
                            infer_error(Identifier(Ident::default()), ty2)
                        }
                    } else {
                        infer_error(Set(HashMap::new()), ty1)
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
                            infer_error(Identifier(Ident::default()), ty2)
                        }
                    } else {
                        infer_error(Set(HashMap::new()), ty1)
                    }
                }
                BinOp::AttributeFallback => {
                    if ty1 == Null {
                        Ok(ty2)
                    } else {
                        Ok(ty1)
                    }
                }
                BinOp::LessThan => expect_numerals(ty1, ty2),
                BinOp::LessThanEqual => expect_numerals(ty1, ty2),
                BinOp::GreaterThan => expect_numerals(ty1, ty2),
                BinOp::GreaterThanEqual => expect_numerals(ty1, ty2),
                BinOp::Equal => expect_numerals(ty1, ty2),
                BinOp::NotEqual => expect_bools(ty1, ty2),
                BinOp::And => expect_bools(ty1, ty2),
                BinOp::Or => expect_bools(ty1, ty2),
                BinOp::Implication => expect_bools(ty1, ty2),
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
            span: _,
        } => {
            context.push_scope();
            for (name, expr) in bindings {
                let ty = hm(context, expr)?;
                context.insert(*name, ty);
            }
            if let Some(inherit) = inherit {
                for name in inherit {
                    let ty = context
                        .lookup(name)
                        .ok_or(InferError::UnknownIdentifier("".to_string()))?; // Maybe don't make this a hard error
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
            span: _,
        } => {
            context.push_scope();
            for patt in arguments {
                for patt in &patt.patterns {
                    match patt {
                        PatternElement::Identifier(name) => {
                            let ty = context
                                .lookup(name)
                                .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
                            context.insert(*name, ty.clone());
                        }
                        PatternElement::DefaultIdentifier(name, default) => {
                            let ty1 = context.lookup(name).cloned();
                            let ty2 = hm(context, default)?;

                            if let Some(ty1) = ty1 {
                                if ty1 != ty2 {
                                    return Err(InferError::TypeMismatch {
                                        expected: ty2,
                                        found: ty1,
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
            span: _,
        } => {
            let ty = hm(context, condition)?;
            if ty != Bool {
                return Err(InferError::TypeMismatch {
                    expected: Bool,
                    found: ty,
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
        } => Err(InferError::UnexpectedComment),
        Ast::With { set, body, span: _ } => {
            context.push_scope();
            lookup_set_bindigs(context, &set.as_ref().clone())?;
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

pub fn infer(expr: &Ast) -> Result<Type, InferError> {
    let mut context = Context::new();
    hm(&mut context, expr)
}
