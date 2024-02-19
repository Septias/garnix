use super::InferError;
use crate::{ast::BinOp, infer::ast::Ast};

/// Create the longest possible path access-path for this identifier.
pub(crate) fn fold_path(ast: &Ast, path: &mut Vec<String>) -> Result<(), InferError> {
    match ast {
        Ast::BinaryOp {
            op,
            lhs,
            rhs,
            span: _,
        } => match op {
            BinOp::AttributeSelection => {
                fold_path(lhs, path)?;
                path.push(rhs.to_identifier_string()?);
            }
            _ => (),
        },
        _ => (),
    }
    Ok(())
}
