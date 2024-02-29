#![feature(box_patterns)]
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
pub use ast::Ast;

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
    #[error("Function has to accept at least one argument")]
    TooFewArguments,
    #[error("Multiple")]
    MultipleErrors(Vec<SpannedError>),
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

/// [InferResult] with a [Span] attached.
pub type SpannedInferResult<T> = Result<T, SpannedError>;

/// Type infercence result.
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

/// Create an infer error [InferResult].
pub(crate) fn infer_error<T>(expected: TypeName, found: TypeName) -> InferResult<T> {
    Err(InferError::TypeMismatch { expected, found })
}

/// Create a spanned [SpannedInferResult].
pub(crate) fn spanned_infer_error<T>(
    expected: TypeName,
    found: TypeName,
    span: &Span,
) -> SpannedInferResult<T> {
    Err(SpannedError {
        error: InferError::TypeMismatch { expected, found },
        span: span.clone(),
    })
}

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, Default, EnumDiscriminants, EnumTryAs)]
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
    fn into_debrujin(self) -> InferResult<usize> {
        match self {
            Type::Identifier(ident) => Ok(ident.debrujin),
            t => infer_error(TypeName::Identifier, t.get_name()),
        }
    }

    /// Try to convert this type to a function.
    fn into_function(self) -> InferResult<(Type, Type)> {
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
pub(crate) struct Context<'a> {
    bindings: Vec<Vec<&'a Identifier>>,
    depth: usize,
}

impl<'a> Context<'a> {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![Vec::new()],
            depth: 0,
        }
    }

    /// Create a new scope with the given bindings.
    pub(crate) fn push_scope(&mut self, bindings: Vec<&'a Identifier>) {
        self.depth += bindings.len();
        self.bindings.push(bindings);
    }

    /// Pop the list scope.
    pub(crate) fn pop_scope(&mut self) {
        let removed = self.bindings.pop();
        if let Some(removed) = removed {
            self.depth -= removed.len();
        }
    }

    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &mut self,
        scope: Vec<&'a Identifier>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.push_scope(scope);
        let t = f(self);
        self.pop_scope();
        t
    }

    /// Insert a list of [Identifier] into the current scope.
    pub(crate) fn insert(&mut self, ident: Vec<&'a Identifier>) {
        self.bindings.last_mut().unwrap().extend(ident);
    }

    /// Lookup an [Identifier] by it's name.
    pub(crate) fn lookup_by_name(&self, name: &str) -> Option<&'a Identifier> {
        for scope in self.bindings.iter().rev() {
            for n in scope.iter().rev() {
                if n.name == name {
                    return Some(n);
                }
            }
        }
        None
    }

    /// Lookup an [Identifier]s [Type] by it's debrujin index.
    pub(crate) fn lookup_type(&self, debrujin: usize) -> Option<Type> {
        for scope in self.bindings.iter().rev() {
            for n in scope.iter().rev() {
                if n.debrujin == debrujin {
                    return n.get_type();
                }
            }
        }
        None
    }

    /// Lookup an [Identifier] by it's debrujin index.
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
}

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Ident {
    name: String,
    debrujin: usize,
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
pub fn infer(source: &str) -> (Type, Ast) {
    let ast = parser::parse(source).unwrap();
    let ast = Ast::from_parser_ast(ast, &source);
    let ty = hm::infer(&ast).unwrap();
    (ty, ast)
}
