use core::str;
mod context;
pub mod error;
pub mod infer;
mod lower;
mod module;
mod parreaux;
mod types;
use crate::module::Expr;
use error::*;

#[salsa::input]
pub(crate) struct File {
    pub content: String,
}

#[salsa::tracked]
pub fn infer_program<'a>(db: &'a dyn salsa::Database, program: Expr<'a>) -> Vec<Diagnostic> {
    infer::infer(db, program)
}

#[salsa::tracked]
fn parse_file(db: &dyn salsa::Database, file: File) -> Vec<Diagnostic> {
    let contents = file.content(db);
    let parser = parser2::parse_file(&contents);
    let ast = lower::lower(db, file, parser);
    infer_program(db, todo!())
}
