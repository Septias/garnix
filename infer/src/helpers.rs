use crate::{
    ast::{self, Ast},
    InferResult,
};
use anyhow::anyhow;
use parser::ast::BinOp;

/// Create the last debrujin index in an access chain.
pub(crate) fn fold_path(ast: &Ast) -> InferResult<usize> {
    match ast {
        Ast::BinaryOp {
            op,
            lhs,
            rhs,
            span: _,
        } => {
            if op == &BinOp::AttributeSelection {
                fold_path(rhs)
            } else {
                lhs.as_debrujin()
            }
        }
        Ast::Identifier(ast::Identifier { debrujin, .. }) => Ok(*debrujin),
        _ => Err(anyhow!("Path error").into()),
    }
}
