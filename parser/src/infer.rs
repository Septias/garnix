#![allow(unused)]
//! Type inference using HM type system.

use std::collections::HashMap;

use anyhow::Context as _;
use strum_macros::Display;
use thiserror::Error;

use crate::ast::{Ast, PatternElement};

#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(String),
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: Type, found: Type },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("Can't infer type of assert")]
    UnexpectedAssertion,
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Path,
    Null,
    Undefined,
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
    Union(Box<Type>, Box<Type>),
    Set(HashMap<String, Type>),
    Var(String),
}
pub(crate) struct Context(Vec<Vec<(String, Type)>>);

impl Context {
    pub(crate) fn new() -> Self {
        Self(vec![Vec::new()])
    }

    pub(crate) fn push_scope(&mut self) {
        self.0.push(Vec::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub(crate) fn insert(&mut self, name: String, ty: Type) {
        self.0.last_mut().unwrap().push((name, ty));
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.0.iter().rev() {
            for (n, ty) in scope.iter().rev() {
                if n == name {
                    return Some(ty);
                }
            }
        }
        None
    }
}

/// Lookup all bindings that are part of a `with`-expression
fn lookup_set_bindigs(context: &mut Context, bindings: Ast) -> Result<(), InferError> {
    match bindings {
        Ast::AttrSet {
            attrs,
            is_recursive,
        } => {
            for (name, expr) in attrs {
                let ty = context
                    .lookup("todo")
                    .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
                context.insert("todo".to_string(), ty.clone());
            }
        }
        Ast::Identifier(_) => {}
        _ => todo!(),
    }
    Ok(())
}

fn hm(context: &mut Context, expr: &Ast) -> Result<Type, InferError> {
    use Type::*;
    match expr {
        Ast::UnaryOp { op, rhs } => {
            let ty = hm(context, rhs)?;
            todo!()
        }
        Ast::BinaryOp { op, lhs, rhs } => {
            let ty1 = hm(context, lhs)?;
            let ty2 = hm(context, rhs)?;
            todo!()
        }
        Ast::AttrSet {
            attrs,
            is_recursive,
        } => Ok(Set(attrs
            .iter()
            .map(|(name, expr)| ("".to_string(), hm(context, expr).unwrap()))
            .collect::<HashMap<_, _>>())),
        Ast::LetBinding {
            bindings,
            body,
            inherit,
        } => {
            context.push_scope();
            for (name, expr) in bindings {
                let ty = hm(context, expr)?;
                context.insert("todo".to_string(), ty);
            }
            if let Some(inherit) = inherit {
                for name in inherit {
                    let ty = context
                        .lookup("")
                        .ok_or(InferError::UnknownIdentifier("".to_string()))?; // Maybe don't make this a hard error
                    context.insert("".to_string(), ty.clone());
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();
            Ok(ty)
        }
        Ast::Lambda {
            arguments,
            body,
            arg_binding,
        } => {
            context.push_scope();
            for patt in arguments {
                for patt in &patt.patterns {
                    match patt {
                        PatternElement::Identifier(_) => {
                            let ty = context
                                .lookup("todo")
                                .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
                            context.insert("todo".to_string(), ty.clone());
                        }
                        PatternElement::DefaultIdentifier(_, default) => {
                            let ty1 = context.lookup("todo").cloned();
                            let ty2 = hm(context, default)?;

                            if let Some(ty1) = ty1 {
                                if ty1 != ty2 {
                                    return Err(InferError::TypeMismatch {
                                        expected: ty2,
                                        found: ty1,
                                    });
                                }
                            }

                            context.insert("todo".to_string(), ty2);
                        }
                    }
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();

            /// TODO: somehow create curry style functions here
            Ok(Function(Box::new(Undefined), Box::new(ty)))
        }
        Ast::Conditional {
            condition,
            expr1,
            expr2,
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
        Ast::Assertion { condition, then } => Err(InferError::UnexpectedComment),
        Ast::With { set, body } => {
            context.push_scope();
            lookup_set_bindigs(context, set.as_ref().clone())?;
            hm(context, body)
        }
        Ast::Identifier(name) => Ok(context.lookup("").cloned().unwrap_or(Undefined)),
        Ast::NixString(_) => Ok(String),
        Ast::NixPath(_) => Ok(Path),
        Ast::Bool(_) => Ok(Bool),
        Ast::Int(_) => Ok(Int),
        Ast::Float(_) => Ok(Float),
        Ast::Null => Ok(Null),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => {
            Err(InferError::UnexpectedComment)
        }
    }
}

pub fn infer(expr: &Ast) -> Result<Type, InferError> {
    let mut context = Context::new();
    hm(&mut context, expr)
}
