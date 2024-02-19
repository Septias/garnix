use crate::{ast::BinOp, infer::ast::Ast};

use super::InferResult;

/// Create the longest possible path access-path for this identifier.
pub(crate) fn fold_path(ast: &Ast, path: &mut Vec<String>) -> InferResult<()> {
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
