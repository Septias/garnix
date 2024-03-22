#![feature(box_patterns, iter_intersperse)]
use ast::Identifier;
use core::str;
use infer::freshen_above;
use logos::Span;
use types::{PolymorhicType, Type, TypeName, Var};

pub mod ast;
pub mod infer;
pub use ast::Ast;
pub mod error;
pub mod types;
pub use error::*;

#[cfg(test)]
mod tests;

pub enum ContextType {
    Type(Type),
    PolymorhicType(PolymorhicType),
}

impl ContextType {
    fn level(&self) -> usize {
        match self {
            ContextType::Type(ty) => ty.level(),
            ContextType::PolymorhicType(pty) => pty.lvl,
        }
    }

    fn instantiate(&self, context: &Context, lvl: usize) -> Type {
        match self {
            ContextType::Type(ty) => ty.instantiate(),
            ContextType::PolymorhicType(pty) => freshen_above(context, &pty.body, pty.lvl, lvl),
        }
    }
}

/// Context to save variables and their types.
pub(crate) struct Context<'a> {
    bindings: Vec<Vec<(&'a str, ContextType)>>,
    with: Option<&'a Identifier>,
    count: usize,
}

impl<'a> Context<'a> {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![vec![]],
            count: 0,
            with: None,
        }
    }

    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &mut self,
        scope: Vec<(&'a str, ContextType)>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.bindings.push(scope);
        let t = f(self);
        self.bindings.pop();
        t
    }

    /// Lookup an [Var] by it's name.
    pub(crate) fn lookup(&self, name: &str) -> Option<&ContextType> {
        for scope in self.bindings.iter().rev() {
            for (item_name, item) in scope.iter().rev() {
                if *item_name == name {
                    return Some(item);
                }
            }
        }
        None
    }

    pub(crate) fn fresh_var(&self, lvl: usize) -> Var {
        let res = Var::new(lvl, self.count);
        // self.count += 1;
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
