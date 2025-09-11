#![feature(box_patterns, iter_intersperse)]
use core::str;
pub mod error;
pub mod infer;
pub mod module;
mod parreaux;
pub mod types;
use crate::module::{Expr, ExprData};
pub use error::*;

#[cfg(test)]
mod tests;

#[salsa::input]
struct SourceProgramm {
    #[returns(ref)]
    pub text: String,
}

/// Infer the type of an ast.
/// Returns the final type as well as the ast which has been annotated with types.
#[salsa::tracked]
pub fn infer_program<'a>(db: &'a dyn salsa::Database, program: Expr<'a>) -> Vec<Diagnostic> {
    infer::infer(db, program)
}

#[salsa::tracked]
fn parse_file(db: &dyn salsa::Database, file: SourceProgramm) -> Vec<Diagnostic> {
    let contents: &str = file.text(db);
    let parser = parser2::parse_file(contents);
    let parser_ast = parser.root().expr();
    let ast = lower(parser_ast.unwrap());
    infer_program(db, ast)
}

fn lower<'a>(_parser_ast: parser2::ast::Expr) -> Expr<'a> {
    todo!()
}
