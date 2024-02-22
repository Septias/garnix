use super::InferError;
use crate::ast::Ast;
use parser::ast::BinOp;

/// Create the longest possible path access-path for this identifier.
pub(crate) fn fold_path(ast: &Ast, path: &mut Vec<String>) -> Result<(), InferError> {
    if let Ast::BinaryOp {
        op,
        lhs,
        rhs,
        span: _,
    } = ast
    {
        if op == &BinOp::AttributeSelection {
            fold_path(lhs, path)?;
            path.push(rhs.to_identifier_string()?);
        }
    }
    Ok(())
}
