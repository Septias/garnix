use salsa::Accumulator;
use thiserror::Error;

use crate::types::{Ty, TypeName};

/// An error that occured during type inference.
#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier")]
    UnknownIdentifier,
    #[error("Unknown inherit")]
    UnknownInherit,
    #[error("The record field {field} is missing")]
    MissingRecordField { field: String },
    #[error("Cannot constrain {lhs} <: {rhs}")]
    CannotConstrain { lhs: Ty, rhs: Ty },
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: TypeName, found: TypeName },
    #[error("Can't convert {from} to {to}")]
    ConversionError { from: String, to: &'static str },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("The supplied argument has too many fields {field}")]
    TooManyField { field: String },
    #[error("Unknown function call")]
    UnknownFunction,
    #[error("Function has to accept at least one argument")]
    TooFewArguments,
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

/// An Error that also contains the span in the source.
#[salsa::accumulator]
pub struct Diagnostic {
    pub start: usize,
    pub end: usize,
    pub error: InferError,
}

#[salsa::tracked(debug)]
pub struct Span<'db> {
    #[tracked]
    pub start: usize,
    #[tracked]
    pub end: usize,
}

impl Diagnostic {
    pub fn new(start: usize, end: usize, error: InferError) -> Self {
        Diagnostic { start, end, error }
    }

    #[cfg(test)]
    pub fn render(&self, db: &dyn salsa::Database, src: SourceProgram) -> String {
        use annotate_snippets::*;
        let line_start = src.text(db)[..self.start].lines().count() + 1;
        Renderer::plain()
            .render(
                Level::Error.title(&self.message).snippet(
                    Snippet::source(src.text(db))
                        .line_start(line_start)
                        .origin("input")
                        .fold(true)
                        .annotation(Level::Error.span(self.start..self.end).label("here")),
                ),
            )
            .to_string()
    }
}

/// Type infercence result.
pub type InferResult<T> = Result<T, InferError>;

/// Create an [InferResult].
pub(crate) fn type_mismatch<T>(expected: TypeName, found: TypeName) -> InferResult<T> {
    Err(InferError::TypeMismatch { expected, found })
}

/// Add diagnostic to the accumulator.
pub(crate) fn add_diag(db: &dyn salsa::Database, span: Span, error: InferError) {
    Diagnostic::new(span.start(db), span.end(db), error).accumulate(db);
}

impl From<(String, &'static str)> for InferError {
    fn from((from, to): (String, &'static str)) -> Self {
        InferError::ConversionError { from, to }
    }
}

impl From<(&Span, InferError)> for Diagnostic {
    fn from((span, error): (&Span, InferError)) -> Self {
        Self {
            span: span.clone(),
            error,
        }
    }
}
