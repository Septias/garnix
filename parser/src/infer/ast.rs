use core::str;
use std::collections::HashMap;

use logos::Span;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::ast::{Ast as ParserAst, BinOp, BinOpDiscriminants, UnOp};

use super::{helpers::fold_path, Ident, InferError, InferResult};

/// Part of a [Pattern].
#[derive(Debug, Clone, PartialEq)]
pub enum PatternElement {
    /// Pattern of the form `ident`
    Identifier(usize),
    /// Pattern of the form `ident ? <default>`
    DefaultIdentifier(usize, Ast),
}

/// A pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// A list of patterns
    pub patterns: Vec<PatternElement>,
    /// Is widcard
    pub is_wildcard: bool,
}

#[derive(Debug, Clone, PartialEq, AsRefStr, EnumDiscriminants)]
#[strum_discriminants(derive(Display, AsRefStr))]
/// Mirror of [ParserAst], but with identifiers replaced by DeBrujin indices.
pub enum Ast {
    /// ----------------- Operators -----------------

    /// Unary Operators
    UnaryOp {
        op: UnOp,
        rhs: Box<Ast>,
        span: Span,
    },

    /// Binary Operators
    BinaryOp {
        op: BinOp,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
        span: Span,
    },

    /// ----------------- Language Constructs -----------------

    /// Attribute set
    AttrSet {
        /// A set of attributes
        attrs: Vec<(usize, Ast)>,
        is_recursive: bool,
        span: Span,
    },

    /// Let expression
    LetBinding {
        /// A set of bindings
        bindings: Vec<(usize, Ast)>,
        /// The expression to evaluate
        body: Box<Ast>,
        /// A list of identifiers to inherit from the parent scope
        inherit: Option<Vec<usize>>,
        span: Span,
    },

    /// Function
    Lambda {
        arguments: Vec<Pattern>,
        body: Box<Ast>,
        arg_binding: Option<usize>,
        span: Span,
    },

    /// Conditional
    Conditional {
        /// The condition to evaluate
        condition: Box<Ast>,
        /// The expression to evaluate if the condition is true
        expr1: Box<Ast>,
        /// The expression to evaluate if the condition is false
        expr2: Box<Ast>,
        span: Span,
    },

    /// An assert statement.
    Assertion {
        /// The condition to evaluate
        condition: Box<Ast>,
        /// The expression to evaluate if the condition is true
        span: Span,
    },

    /// A with-statement.
    With {
        /// The set-identifier to add
        set: Box<Ast>,
        /// The expression to evaluate
        body: Box<Ast>,
        span: Span,
    },

    Identifier {
        debrujin: usize,
        name: String,
        span: Span,
    },
    List {
        items: Vec<Ast>,
        span: Span,
    },

    NixString(Span),
    NixPath(Span),
    Bool {
        val: bool,
        span: Span,
    },
    Int {
        val: i32,
        span: Span,
    },
    Float {
        val: f32,
        span: Span,
    },
    Null(Span),
    Comment(Span),
    DocComment(Span),
    LineComment(Span),
}

impl Ast {
    /// Convert a parser ast to an infer ast.
    pub fn from_parser_ast(value: ParserAst, source: String) -> Self {
        let mut vars = Cache::new();
        transform_ast(value, &mut vars, &source, 0)
    }

    /// Tries to convert the ast to an [Identifier] and adds as many path elements as possible.
    pub fn as_ident(&self) -> InferResult<Ident> {
        match self {
            Ast::Identifier { debrujin, .. } => Ok(Ident {
                name: *debrujin,
                path: None,
            }),
            Ast::BinaryOp {
                op: BinOp::AttributeSelection,
                lhs,
                rhs,
                span: _,
            } => {
                let mut path = vec![];
                fold_path(rhs, &mut path)?;
                Ok(Ident {
                    name: lhs.as_debrujin()?,
                    path: Some(path),
                })
            }
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to a list.
    pub fn as_list(&self) -> InferResult<&Vec<Ast>> {
        match self {
            Ast::List { items, span: _ } => Ok(items),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::List.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to a function and then return the arguments.
    pub fn as_application(&self) -> InferResult<(&Ast, &Ast)> {
        match self {
            Ast::BinaryOp {
                op,
                box lhs,
                box rhs,
                span: _,
            } => match op {
                BinOp::Application => Ok((lhs, rhs)),
                _ => Err(InferError::ConversionError {
                    from: op.as_ref().to_string(),
                    to: BinOpDiscriminants::Application.as_ref(),
                }),
            },
            _ => Err(InferError::ConversionError {
                from: self.as_ref().to_string(),
                to: AstDiscriminants::BinaryOp.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to an identifier and then return name as string.
    pub fn to_identifier_string(&self) -> InferResult<String> {
        match self {
            Ast::Identifier { name: text, .. } => Ok(text.to_string()),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to an identifier and then return the De Bruijn index.
    pub fn as_debrujin(&self) -> InferResult<usize> {
        match self {
            Ast::Identifier { debrujin, .. } => Ok(*debrujin),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    pub fn as_attr_set(&self) -> InferResult<&Vec<(usize, Ast)>> {
        match self {
            Ast::AttrSet { attrs, .. } => Ok(attrs),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::AttrSet.as_ref(),
            }),
        }
    }
}

// Convert spans to De' Brujin indices
// this should support path-wise set accesses
fn transform_ast<'a>(
    value: ParserAst,
    cache: &mut Cache<'a>,
    source: &'a str,
    mut fun_depth: usize,
) -> Ast {
    use Ast::*;
    match value {
        ParserAst::UnaryOp { op, box rhs, span } => UnaryOp {
            op,
            rhs: Box::new(transform_ast(rhs, cache, source, fun_depth)),
            span,
        },
        ParserAst::BinaryOp {
            op,
            box lhs,
            box rhs,
            span,
        } => BinaryOp {
            op,
            lhs: Box::new(transform_ast(lhs, cache, source, fun_depth)),
            rhs: Box::new(transform_ast(rhs, cache, source, fun_depth)),
            span,
        },
        ParserAst::AttrSet {
            attrs,
            is_recursive,
            span,
        } => {
            let attrs = attrs
                .into_iter()
                .map(|(name, expr)| {
                    (
                        cache.get(&source[name], &mut fun_depth),
                        transform_ast(expr, cache, source, fun_depth),
                    )
                })
                .collect();
            Ast::AttrSet {
                attrs,
                is_recursive,
                span,
            }
        }
        ParserAst::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            let bindings = bindings
                .into_iter()
                .map(|(name, expr)| {
                    (
                        cache.get(&source[name], &mut fun_depth),
                        transform_ast(expr, cache, source, fun_depth),
                    )
                })
                .collect();
            let inherit = inherit.map(|inherit| {
                inherit
                    .into_iter()
                    .map(|name| cache.get(&source[name], &mut fun_depth))
                    .collect()
            });
            LetBinding {
                bindings,
                body: Box::new(transform_ast(*body, cache, source, fun_depth)),
                inherit,
                span,
            }
        }
        ParserAst::Lambda {
            arguments: _,
            body: _,
            arg_binding: _,
            span: _,
        } => {
            todo!()
        }
        ParserAst::Conditional {
            box condition,
            box expr1,
            box expr2,
            span,
        } => Conditional {
            condition: Box::new(transform_ast(condition, cache, source, fun_depth)),
            expr1: Box::new(transform_ast(expr1, cache, source, fun_depth)),
            expr2: Box::new(transform_ast(expr2, cache, source, fun_depth)),
            span,
        },
        ParserAst::Assertion {
            box condition,
            span,
        } => Assertion {
            condition: Box::new(transform_ast(condition, cache, source, fun_depth)),
            span,
        },
        ParserAst::With {
            box set,
            box body,
            span,
        } => With {
            set: Box::new(transform_ast(set, cache, source, fun_depth)),
            body: Box::new(transform_ast(body, cache, source, fun_depth)),
            span,
        },
        ParserAst::Identifier(span) => Identifier {
            debrujin: cache.get(&source[span.clone()], &mut fun_depth),
            name: source[span.clone()].to_string(),
            span,
        },
        ParserAst::NixString(span) => NixString(span),
        ParserAst::NixPath(span) => NixPath(span),
        ParserAst::List { items, span } => List {
            items: items
                .into_iter()
                .map(|l| transform_ast(l, cache, source, fun_depth))
                .collect(),
            span,
        },
        ParserAst::Bool { val, span } => Bool { val, span },
        ParserAst::Int { val, span } => Int { val, span },
        ParserAst::Float { val, span } => Float { val, span },
        ParserAst::Null(span) => Null(span),
        ParserAst::Comment(_) | ParserAst::DocComment(_) | ParserAst::LineComment(_) => {
            unimplemented!()
        }
    }
}

/// A cache.
struct Cache<'a> {
    map: HashMap<&'a str, usize>,
}

impl<'a> Cache<'a> {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn get(&mut self, var: &'a str, count: &mut usize) -> usize {
        if let Some(number) = self.map.get(var) {
            *number
        } else {
            self.map.insert(var, *count);
            *count += 1;
            *count - 1
        }
    }
}
