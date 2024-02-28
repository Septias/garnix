#![feature(box_patterns)]
use anyhow::Context as _;
use ast::Identifier;
use core::str;
use logos::Span;
use std::collections::HashMap;
use strum::EnumTryAs;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};
use thiserror::Error;

pub mod ast;
pub mod helpers;
pub mod hm;

/// An error that occured during type inference.
#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier")]
    UnknownIdentifier,
    #[error("Unknown inherit")]
    UnknownInherit,
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

impl InferError {
    fn span(self, span: &Span) -> SpannedError {
        SpannedError {
            span: span.clone(),
            error: self,
        }
    }
}

/// An Error that also contains the span in the source.
#[derive(Debug)]
pub struct SpannedError {
    pub span: Span,
    pub error: InferError,
}

pub type SpannedInferResult<T> = Result<T, SpannedError>;
pub type InferResult<T> = Result<T, InferError>;

impl From<(Span, TypeName, TypeName)> for SpannedError {
    fn from((span, expected, found): (Span, TypeName, TypeName)) -> Self {
        Self {
            span,
            error: InferError::TypeMismatch { expected, found },
        }
    }
}

impl From<(String, &'static str)> for InferError {
    fn from((from, to): (String, &'static str)) -> Self {
        InferError::ConversionError { from, to }
    }
}

impl From<(&Span, InferError)> for SpannedError {
    fn from((span, error): (&Span, InferError)) -> Self {
        Self {
            span: span.clone(),
            error,
        }
    }
}

/// Create an infer error.
pub(crate) fn infer_error<T>(expected: TypeName, found: TypeName) -> Result<T, InferError> {
    Err(InferError::TypeMismatch { expected, found })
}

/// Create a spaned infer error.
pub(crate) fn spanned_infer_error<T>(
    expected: TypeName,
    found: TypeName,
    span: &Span,
) -> Result<T, SpannedError> {
    Err(SpannedError {
        error: InferError::TypeMismatch { expected, found },
        span: span.clone(),
    })
}

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Display, Default, EnumDiscriminants, EnumTryAs)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type {
    Int,
    Float,
    Number, // Type used only to represent both int and float
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
    fn into_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(TypeName::List, t.get_name()),
        }
    }

    /// Try to convert this type to an identifier.
    fn into_ident(self) -> InferResult<Ident> {
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
            Type::Number => TypeName::Number,
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

/// Context to save variables and their types.
pub(crate) struct Context {
    bindings: Vec<Vec<Identifier>>,
    depth: usize,
}

impl Context {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![Vec::new()],
            depth: 0,
        }
    }

    /// Create a new function scope with all it's bindings.
    pub(crate) fn push_scope(&mut self, bindings: Vec<&Identifier>) {
        self.depth += bindings.len();
        self.bindings.push(bindings);
    }

    /// Pop a function scope.
    pub(crate) fn pop_scope(&mut self) {
        let removed = self.bindings.pop();
        if let Some(removed) = removed {
            self.depth -= removed.len();
        }
    }

    pub(crate) fn insert(&mut self, ident: Vec<&Identifier>) {
        self.bindings.last_mut().unwrap().extend(ident);
    }

    pub(crate) fn reintroduce(&mut self, debrujin: usize) -> anyhow::Result<()> {
        let binding = self.lookup(debrujin).context("can't find")?;
        self.bindings.last_mut().unwrap().insert(0, binding);
        Ok(())
    }

    pub(crate) fn lookup_by_name(&self, name: &str) -> Option<&Identifier> {
        for scope in self.bindings.iter().rev() {
            for n in scope.iter().rev() {
                if n.name == name {
                    return Some(n);
                }
            }
        }
        None
    }

    pub(crate) fn lookup_type(&self, debrujin: usize) -> Option<&Type> {
        for scope in self.bindings.iter().rev() {
            for n in scope.iter().rev() {
                if n.debrujin == debrujin {
                    return Some(n.get_type());
                }
            }
        }
        None
    }

    pub(crate) fn lookup(&self, debrujin: usize) -> Option<&Identifier> {
        for scope in self.bindings.iter().rev() {
            for ident in scope.iter().rev() {
                if ident.debrujin == debrujin {
                    return Some(ident);
                }
            }
        }
        None
    }

    pub(crate) fn add_constraint(&mut self, _debrujin: usize, _ty: Type) {
        todo!()
    }
}

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Ident {
    name: String,
    debrujin: usize,
    path: Option<Vec<String>>,
}

impl Ident {
    fn new(name: String, debrujin: usize, path: Option<Vec<String>>) -> Self {
        Self {
            name,
            debrujin,
            path,
        }
    }
}