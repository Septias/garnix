#![feature(box_patterns, iter_intersperse)]
use core::str;
use infer::freshen_above;
use parser2::Parse;
use std::cell::RefCell;
use types::{PolymorphicType, Ty, TypeName, Var};

pub mod error;
pub mod infer;
pub mod module;
pub mod types;
pub use error::*;

use crate::module::{ExprData, ExprId};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub enum ContextType {
    Type(Ty),
    PolymorhicType(PolymorphicType),
}

impl ContextType {
    fn instantiate(&self, context: &Context, lvl: usize) -> Ty {
        match self {
            ContextType::Type(ty) => ty.instantiate(),
            ContextType::PolymorhicType(pty) => freshen_above(context, &pty.body, pty.level, lvl),
        }
    }

    fn into_type(self) -> Option<Ty> {
        match self {
            ContextType::Type(ty) => Some(ty),
            ContextType::PolymorhicType(_) => None,
        }
    }
}

/// Context to save variables and their types.
#[derive(Debug)]
#[salsa::tracked]
pub struct Context<'a> {
    bindings: Vec<Vec<(String, ContextType)>>,
}

pub type Scope = Vec<(String, ContextType)>;

impl<'a> Context<'a> {
    /// Create a new scope and run a function with it.
    pub(crate) fn with_scope<T>(
        &self,
        db: &dyn salsa::Database,
        scope: Vec<(String, ContextType)>,
        f: impl FnOnce(Scope) -> T,
    ) -> T {
        let new = self.bindings(db).clone();
        new.push(scope);
        f(new);
    }

    /// Lookup a [Var] by it's name.
    pub(crate) fn lookup(&self, db: &dyn salsa::Database, name: &str) -> Option<ContextType> {
        for scope in self.bindings(db).iter().rev() {
            for (item_name, item) in scope.iter().rev() {
                if *item_name == name {
                    return Some(item.clone());
                }
            }
        }
        None
    }
}

#[salsa::input]
struct SourceProgramm {
    #[returns(ref)]
    pub text: String,
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
#[salsa::tracked]
pub fn infer_program(db: &dyn salsa::Database, program: SourceProgramm) -> Vec<Diagnostic> {
    infer::infer(db, program.expr(db))
}

#[salsa::tracked]
fn parse_file(db: &dyn salsa::Database, file: SourceProgramm) -> ExprData {
    let contents: &str = file.text(db);
    let parser = parser2::parse_file(db, contents);
    parser.root.expr()
}
