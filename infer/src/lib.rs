#![feature(box_patterns)]
use ast::Identifier;
use core::str;
use logos::Span;
use std::fmt;
use thiserror::Error;
use types::{Type, TypeName};

pub mod ast;
pub mod helpers;
pub mod infer;
pub use ast::Ast;
pub mod types;

#[cfg(test)]
mod tests;

/// An error that occured during type inference.
#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier")]
    UnknownIdentifier,
    #[error("Unknown inherit")]
    UnknownInherit,
    #[error("The record field {field} is missing")]
    MissingRecordField { field: String },
    #[error("cannot constrain {lhs} <: {rhs}")]
    CannotConstrain { lhs: Type, rhs: Type },
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: TypeName, found: TypeName },
    #[error("Can't convert {from} to {to}")]
    ConversionError { from: String, to: &'static str },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("Unknown function call")]
    UnknownFunction,
    #[error("Function has to accept at least one argument")]
    TooFewArguments,
    #[error("Multiple")]
    MultipleErrors(Vec<SpannedError>),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl PartialEq for InferError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::TypeMismatch {
                    expected: l_expected,
                    found: l_found,
                },
                Self::TypeMismatch {
                    expected: r_expected,
                    found: r_found,
                },
            ) => l_expected == r_expected && l_found == r_found,
            (
                Self::ConversionError {
                    from: l_from,
                    to: l_to,
                },
                Self::ConversionError {
                    from: r_from,
                    to: r_to,
                },
            ) => l_from == r_from && l_to == r_to,
            (Self::MultipleErrors(l0), Self::MultipleErrors(r0)) => l0 == r0,
            (Self::Other(l0), Self::Other(r0)) => l0.to_string() == r0.to_string(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
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
#[derive(Debug, Error, PartialEq)]
pub struct SpannedError {
    pub span: Span,
    pub error: InferError,
}

impl fmt::Display for SpannedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at [{}, {}]",
            self.error, self.span.start, self.span.end
        )
    }
}

/// Type infercence result.
pub type InferResult<T> = Result<T, InferError>;

/// Create an [InferResult].
pub(crate) fn infer_error<T>(expected: TypeName, found: TypeName) -> InferResult<T> {
    Err(InferError::TypeMismatch { expected, found })
}

/// [InferResult] with a [Span] attached.
pub type SpannedInferResult<T> = Result<T, SpannedError>;

/// Create a [SpannedInferResult].
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

/// Context to save variables and their types.
pub(crate) struct Context<'a> {
    bindings: Vec<Vec<&'a Identifier>>,
    with: Option<&'a Identifier>,
    errors: Vec<SpannedError>,
    depth: usize,
}

impl<'a> Context<'a> {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![Vec::new()],
            depth: 0,
            with: None,
            errors: Vec::new(),
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
    pub(crate) fn lookup_type(&self, mut debrujin: usize) -> Option<Type> {
        for scope in self.bindings.iter().rev() {
            for n in scope.iter().rev() {
                if n.debrujin == debrujin {
                    return;
                }
            }
        }
        None
    }

    /// Lookup an [Identifier] by it's debrujin index.
    pub(crate) fn lookup(&self, debrujin: usize) -> Option<&'a Identifier> {
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

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
pub fn infer(source: &str) -> SpannedInferResult<(Type, Ast)> {
    let ast = parser::parse(source).map_err(|e| InferError::from(e).span(&Span::default()))?;
    let ast = Ast::from_parser_ast(ast, &source);
    let ty = infer::infer(&ast)?;
    Ok((ty, ast))
}
