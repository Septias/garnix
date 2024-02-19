use core::str;
use std::collections::HashMap;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};
use thiserror::Error;

pub mod ast;
pub mod helpers;
pub mod hm;

/// An error that occured during type inference.
#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(String),
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: TypeName, found: TypeName },
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

/// Create an infer error.
pub(crate) fn infer_error<T>(expected: TypeName, found: TypeName) -> Result<T, InferError> {
    Err(InferError::TypeMismatch { expected, found })
}

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Display, Default, EnumDiscriminants)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
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
    /// Try to convert this type to a list.
    fn as_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(TypeName::List, t.get_name()),
        }
    }

    /// Try to convert this type to an identifier.
    fn as_ident(self) -> InferResult<Ident> {
        match self {
            Type::Identifier(ident) => Ok(ident),
            t => infer_error(TypeName::Identifier, t.get_name()),
        }
    }

    /// Try to convert this type to a function.
    fn as_function(&self) -> InferResult<(&Type, &Type)> {
        match self {
            Type::Function(box lhs, box rhs) => Ok((lhs, rhs)),
            t => infer_error(TypeName::Function, t.get_name()),
        }
    }

    /// Returns the enum descriminant of this type.
    fn get_name(&self) -> TypeName {
        match self {
            Type::Int => TypeName::Int,
            Type::Float => TypeName::Float,
            Type::Bool => TypeName::Bool,
            Type::String => TypeName::String,
            Type::Path => TypeName::Path,
            Type::Identifier(_) => TypeName::Identifier,
            Type::Null => TypeName::Null,
            Type::Undefined => TypeName::Undefined,
            Type::List(_) => TypeName::List,
            Type::Function(_, _) => TypeName::Function,
            Type::Union(_, _) => TypeName::Union,
            Type::Set(_) => TypeName::Set,
            Type::Var(_) => TypeName::Var,
            Type::Default => TypeName::Default,
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
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Ident {
    name: usize,
    path: Option<Vec<String>>,
}
