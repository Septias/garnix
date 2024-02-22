use super::{helpers::fold_path, Ident, InferError, InferResult};
use crate::ast::{Ast as ParserAst, BinOp, BinOpDiscriminants, UnOp};
use core::str;
use logos::Span;
use std::collections::HashMap;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

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

#[derive(Debug, Clone, PartialEq)]
/// An Identifier.
pub struct Identifier {
    pub debrujin: usize,
    pub name: String,
    pub usize_name: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, AsRefStr, EnumDiscriminants)]
#[strum_discriminants(derive(Display, AsRefStr))]
/// Mirror of [ParserAst], but with identifiers replaced by DeBrujin indices.
pub enum Ast {
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

    /// Attribute set
    AttrSet {
        attrs: Vec<(usize, Ast)>,
        is_recursive: bool,
        span: Span,
    },

    /// Let expression
    LetBinding {
        bindings: Vec<(usize, Ast)>,
        body: Box<Ast>,
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
        condition: Box<Ast>,
        expr1: Box<Ast>,
        expr2: Box<Ast>,
        span: Span,
    },

    /// An assert statement.
    Assertion {
        condition: Box<Ast>,
        expr: Box<Ast>,
        span: Span,
    },

    /// A with-statement.
    With {
        set: Box<Ast>,
        body: Box<Ast>,
        span: Span,
    },

    /// Identifier
    Identifier(Identifier),

    /// List
    List {
        items: Vec<Ast>,
        span: Span,
    },

    /// ----- Primitives --------
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
        transform_ast(value, &mut vars, &source)
    }

    /// Tries to convert the ast to an [Identifier] and adds as many path elements as possible.
    pub fn as_ident(&self) -> Result<Ident, InferError> {
        match self {
            Ast::Identifier(Identifier {
                debrujin,
                usize_name,
                ..
            }) => Ok(Ident {
                name: *usize_name,
                debrujin: *debrujin,
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
                    name: lhs.to_identifier()?,
                    debrujin: lhs.as_debrujin()?,
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
            Ast::Identifier(Identifier { name, .. }) => Ok(name.to_string()),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    pub fn to_identifier(&self) -> InferResult<usize> {
        match self {
            Ast::Identifier(Identifier { usize_name, .. }) => Ok(*usize_name),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to an identifier and then return the De Bruijn index.
    pub fn as_debrujin(&self) -> InferResult<usize> {
        match self {
            Ast::Identifier(Identifier { debrujin, .. }) => Ok(*debrujin),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to a set.
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

/// Convert [ParserAst] to [Ast].
/// Replace every occurence of an identifier with a number.
fn transform_ast<'a>(value: ParserAst, cache: &mut Cache<'a>, source: &'a str) -> Ast {
    use Ast::*;
    match value {
        ParserAst::UnaryOp { op, box rhs, span } => UnaryOp {
            op,
            rhs: Box::new(transform_ast(rhs, cache, source)),
            span,
        },
        ParserAst::BinaryOp {
            op,
            box lhs,
            box rhs,
            span,
        } => BinaryOp {
            op,
            lhs: Box::new(transform_ast(lhs, cache, source)),
            rhs: Box::new(transform_ast(rhs, cache, source)),
            span,
        },
        ParserAst::AttrSet {
            attrs,
            is_recursive,
            span,
            inherit: _,
        } => {
            let attrs = attrs
                .into_iter()
                .map(|(name, expr)| (cache.get(&source[name]), transform_ast(expr, cache, source)))
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
                .map(|(name, expr)| (cache.get(&source[name]), transform_ast(expr, cache, source)))
                .collect();
            let inherit = inherit.map(|inherit| {
                inherit
                    .into_iter()
                    .map(|name| cache.get(&source[name]))
                    .collect()
            });
            LetBinding {
                bindings,
                body: Box::new(transform_ast(*body, cache, source)),
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
            condition: Box::new(transform_ast(condition, cache, source)),
            expr1: Box::new(transform_ast(expr1, cache, source)),
            expr2: Box::new(transform_ast(expr2, cache, source)),
            span,
        },
        ParserAst::Assertion {
            box condition,
            span,
            box expr,
        } => Assertion {
            condition: Box::new(transform_ast(condition, cache, source)),
            expr: Box::new(transform_ast(expr, cache, source)),
            span,
        },
        ParserAst::With {
            box set,
            box body,
            span,
        } => With {
            set: Box::new(transform_ast(set, cache, source)),
            body: Box::new(transform_ast(body, cache, source)),
            span,
        },
        ParserAst::Identifier(span) => Identifier(super::ast::Identifier {
            debrujin: 0,
            usize_name: cache.get(&source[span.clone()]),
            name: source[span.clone()].to_string(),
            span,
        }),

        ParserAst::NixString(span) => NixString(span),
        ParserAst::NixPath(span) => NixPath(span),
        ParserAst::List { items, span } => List {
            items: items
                .into_iter()
                .map(|l| transform_ast(l, cache, source))
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

/// A cache which maps variable names to the numbers 0..n.
struct Cache<'a> {
    map: HashMap<&'a str, usize>,
    count: usize,
}

impl<'a> Cache<'a> {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            count: 0,
        }
    }

    fn get(&mut self, var: &'a str) -> usize {
        if let Some(number) = self.map.get(var) {
            *number
        } else {
            self.map.insert(var, self.count);
            self.count += 1;
            self.count - 1
        }
    }
}
