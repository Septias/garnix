use salsa::{Accumulator, Update};
use thiserror::Error;

use crate::{
    module::ExprId,
    types::{Ty, TypeName},
};

/// An Error that also contains location.
#[derive(Clone, Debug, Update, PartialEq)]
#[salsa::accumulator]
pub struct Diagnostic {
    pub expr: ExprId,
    pub error: InferError,
}

impl Diagnostic {
    pub fn new(expr: ExprId, error: InferError) -> Self {
        Diagnostic { expr, error }
    }
}

/// An error that occured during type inference.
#[derive(Debug, Clone, Error, Update)]
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
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

/// Type infercence result.
pub type InferResult<T> = Result<T, InferError>;

/// Create an [InferResult].
pub(crate) fn type_mismatch<T>(expected: TypeName, found: TypeName) -> InferResult<T> {
    Err(InferError::TypeMismatch { expected, found })
}

/// Add diagnostic to the accumulator.
pub(crate) fn add_diag(db: &dyn salsa::Database, expr: ExprId, error: InferError) {
    Diagnostic::new(expr, error).accumulate(db);
}

impl From<(String, &'static str)> for InferError {
    fn from((from, to): (String, &'static str)) -> Self {
        InferError::ConversionError { from, to }
    }
}
