#![feature(box_patterns, iter_intersperse)]
use ast::Identifier;
use core::str;
use logos::Span;
use types::{Type, TypeName, Var};

pub mod ast;
pub mod infer;
pub use ast::Ast;
pub mod error;
pub mod types;
pub use error::*;

#[cfg(test)]
mod tests;

/// Context to save variables and their types.
pub(crate) struct Context<'a> {
    bindings: Vec<Vec<&'a Identifier>>,
    with: Option<&'a Identifier>,
    errors: Vec<SpannedError>,
    count: usize,
}

impl<'a> Context<'a> {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![Vec::new()],
            count: 0,
            with: None,
            errors: Vec::new(),
        }
    }

    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &mut self,
        scope: Vec<&'a Identifier>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.bindings.push(scope);

        let t = f(self);
        let removed = self.bindings.pop();
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

    pub(crate) fn new_var(&mut self, lvl: usize) -> Var {
        let res = Var::new(lvl, self.count);
        self.count += 1;
        res
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
