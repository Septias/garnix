#![feature(box_patterns, iter_intersperse)]
use ast::Identifier;
use core::str;
use infer::freshen_above;
use logos::Span;
use std::cell::RefCell;
use types::{PolymorphicType, Type, TypeName, Var};

pub mod ast;
pub mod infer;
pub use ast::Ast;
pub mod error;
pub mod types;
pub use error::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub enum ContextType {
    Type(Type),
    PolymorhicType(PolymorphicType),
}

impl ContextType {
    fn instantiate(&self, context: &Context, lvl: usize) -> Type {
        match self {
            ContextType::Type(ty) => ty.instantiate(),
            ContextType::PolymorhicType(pty) => freshen_above(context, &pty.body, pty.level, lvl),
        }
    }

    fn into_type(self) -> Option<Type> {
        match self {
            ContextType::Type(ty) => Some(ty),
            ContextType::PolymorhicType(_) => None,
        }
    }
}

/// Context to save variables and their types.
pub struct Context {
    bindings: Vec<Vec<(String, ContextType)>>,
    with: Option<Type>,
    count: RefCell<usize>,
}

impl Context {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![vec![]],
            count: RefCell::new(0),
            with: None,
        }
    }

    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &mut self,
        scope: Vec<(String, ContextType)>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.bindings.push(scope);
        let t = f(self);
        self.bindings.pop();
        t
    }

    /// Lookup a [Var] by it's name.
    pub(crate) fn lookup(&self, name: &str) -> Option<ContextType> {
        for scope in self.bindings.iter().rev() {
            for (item_name, item) in scope.iter().rev() {
                if *item_name == name {
                    return Some(item.clone());
                }
            }
        }
        None
    }

    pub(crate) fn fresh_var(&self, lvl: usize) -> Var {
        let res = Var::new(lvl, *self.count.borrow());
        *self.count.borrow_mut() += 1;
        res
    }

    /// TODO: with can be stacked.
    pub(crate) fn set_with(&mut self, with: Type) {
        self.with = Some(with);
    }

    /// TODO: with can be stacked.
    pub(crate) fn remove_with(&mut self) {
        self.with = None;
    }
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
pub fn infer(source: &str) -> SpannedInferResult<(Type, Ast)> {
    let ast = parser::parse(source).map_err(|e| InferError::from(e).span(&Span::default()))?;
    let ast = Ast::from_parser_ast(ast, source);
    let ty = infer::infer(&ast)?;
    Ok((ty, ast))
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
pub fn coalesced(source: &str) -> SpannedInferResult<(Type, Ast)> {
    let ast = parser::parse(source).map_err(|e| InferError::from(e).span(&Span::default()))?;
    let ast = Ast::from_parser_ast(ast, source);
    let ty = infer::coalesced(&ast)?;
    Ok((ty, ast))
}
