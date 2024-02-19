use core::str;
use std::collections::HashMap;

use strum_macros::{AsRefStr, Display, EnumDiscriminants};
use thiserror::Error;

pub mod ast;
pub mod helpers;
pub mod hm;

#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(String),
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: Type, found: Type },
    #[error("Can't convert {from} to {to}")]
    ConversionError { from: String, to: &'static str },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("Can't infer type of assert")]
    UnexpectedAssertion,
    #[error("Unknown function call")]
    UnknownFunction,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type InferResult<T> = Result<T, InferError>;

pub(crate) fn infer_error<T>(expected: Type, found: Type) -> Result<T, InferError> {
    Err(InferError::TypeMismatch { expected, found })
}

#[derive(Debug, Clone, PartialEq, Display, Default, EnumDiscriminants)]
#[strum_discriminants(derive(AsRefStr, Display))]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Path,
    Identifier(Ident),
    Null,
    Undefined,
    List(Vec<Type>),
    Function(Box<Type>, Box<Type>),
    Union(Box<Type>, Box<Type>),
    Set(HashMap<String, Type>),
    Var(String),
    #[default]
    Default,
}

impl Type {
    fn as_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(Type::List(vec![]), t.clone()),
        }
    }

    fn as_ident(self) -> InferResult<Ident> {
        match self {
            Type::Identifier(ident) => Ok(ident),
            t => infer_error(Type::Identifier(Ident::default()), t),
        }
    }

    fn as_function(&self) -> InferResult<(&Type, &Type)> {
        match self {
            Type::Function(box lhs, box rhs) => Ok((lhs, rhs)),
            t => infer_error(Type::Function(Box::default(), Box::default()), t.clone()),
        }
    }

    fn to_discriminant(&self) -> TypeDiscriminants {
        match self {
            Type::Int => TypeDiscriminants::Int,
            Type::Float => TypeDiscriminants::Float,
            Type::Bool => TypeDiscriminants::Bool,
            Type::String => TypeDiscriminants::String,
            Type::Path => TypeDiscriminants::Path,
            Type::Identifier(_) => TypeDiscriminants::Identifier,
            Type::Null => TypeDiscriminants::Null,
            Type::Undefined => TypeDiscriminants::Undefined,
            Type::List(_) => TypeDiscriminants::List,
            Type::Function(_, _) => TypeDiscriminants::Function,
            Type::Union(_, _) => TypeDiscriminants::Union,
            Type::Set(_) => TypeDiscriminants::Set,
            Type::Var(_) => TypeDiscriminants::Var,
            Type::Default => TypeDiscriminants::Default,
        }
    }
}

/// A constraint for some identifier.
type Constraint = (Ident, Type);

/// Context to save variables and their types.
pub(crate) struct Context(Vec<Vec<(usize, Type)>>);

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

    pub(crate) fn insert(&mut self, name: usize, ty: Type) {
        self.0.last_mut().unwrap().push((name, ty));
    }

    pub(crate) fn lookup(&self, name: &usize) -> Option<&Type> {
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

/// A single identifier.
/// The name should be a debrujin index and the path is used for set-accesses.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Ident {
    name: usize,
    path: Option<Vec<String>>,
}
