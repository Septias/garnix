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

pub enum PackagedItem {
    Type(Type),
    PolymorhicType(PolymorhicType),
}

impl PackagedItem {
    fn level(&self) -> usize {
        match self {
            PackagedItem::Type(ty) => ty.level(),
            PackagedItem::PolymorhicType(pty) => pty.lvl,
        }
    }

    fn instantiate(&self, context: &mut Context, lvl: usize) -> Type {
        match self {
            PackagedItem::Type(ty) => ty.instantiate(context, lvl),
            PackagedItem::PolymorhicType(pty) => freshen_above(context, &pty.body, pty.lvl, lvl),
        }
    }
}

/// Context to save variables and their types.
pub(crate) struct Context<'a> {
    bindings: Vec<Vec<(&'a str, PackagedItem)>>,
    vars: Vec<Var>,
    with: Option<&'a Identifier>,
    count: usize,
}

impl<'a> Context<'a> {
    pub(crate) fn new() -> Self {
        Self {
            bindings: vec![vec![]],
            count: 0,
            with: None,
            vars: Vec::new(),
        }
    }

    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &mut self,
        scope: Vec<(&'a str, PackagedItem)>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.bindings.push(scope);

        let t = f(self);
        let removed = self.bindings.pop();
        t
    }

    /// Lookup an [Var] by it's name.
    pub(crate) fn lookup(&self, name: &str) -> Option<&Var> {
        for scope in self.bindings.iter().rev() {
            for (name, item) in scope.iter().rev() {
                if name == name {
                    todo!();
                }
            }
        }
        None
    }

    pub(crate) fn fresh_var(&mut self, lvl: usize) -> &Var {
        let res = Var::new(lvl, self.count);
        self.count += 1;
        self.vars.push(res);
        &res
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
