#![feature(box_patterns)]
use ast::Identifier;
use core::str;
use logos::Span;
use std::{collections::HashMap, fmt};
use strum::EnumTryAs;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};
use thiserror::Error;

pub mod ast;
pub mod helpers;
pub mod hm;
pub use ast::Ast;

#[cfg(test)]
mod tests;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitives {
    Number,
    Bool,
    String,
    Path,
    Null,
    Undefined,
}

/// A nix language type.
#[derive(Debug, Clone, PartialEq, Eq, Display, Hash, Default, EnumDiscriminants, EnumTryAs)]
#[strum_discriminants(derive(AsRefStr, Display))]
#[strum_discriminants(name(TypeName))]
pub enum Type {
    Top,
    Bottom,
    Primitive(Primitives),
    Identifier(Ident),
    Function(Box<Type>, Box<Type>),
    List(Vec<Type>),
    Set(HashMap<String, Type>),
    Optional(Box<Type>),
    // Complexe types only created by simplification
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
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
            Type::Identifier(_) => TypeName::Identifier,
            Type::Primitive(_) => TypeName::Primitive,
            Type::List(_) => TypeName::List,
            Type::Function(_, _) => TypeName::Function,
            Type::Union(_, _) => TypeName::Union,
            Type::Set(_) => TypeName::Set,
            Type::Default => TypeName::Default,
            Type::Top => TypeName::Top,
            Type::Bottom => TypeName::Bottom,
            Type::Inter(_, _) => TypeName::Inter,
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

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Ident {
    lower_boundsd: Vec<Type>,
    upper_bounds: Vec<Type>,
    debrujin: usize,
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
pub fn infer(source: &str) -> SpannedInferResult<(Type, Ast)> {
    let ast = parser::parse(source).map_err(|e| InferError::from(e).span(&Span::default()))?;
    let ast = Ast::from_parser_ast(ast, &source);
    // println!("ast: {:?}", ast);
    let ty = hm::infer(&ast)?;
    Ok((ty, ast))
}
