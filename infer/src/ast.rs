use super::{InferError, InferResult};
use crate::{ast, types::Ty};
use core::str;
use logos::Span;
use parser::ast::{Ast as ParserAst, BinOp, UnOp};
use std::{
    cell::OnceCell,
    collections::HashMap,
    hash::{self, Hasher},
};
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

/// Part of a [Pattern].
#[derive(Debug, Clone, PartialEq)]
pub enum PatternElement {
    Identifier(Identifier),
    DefaultIdentifier(Identifier, Ast),
}

/// A pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Record {
        patterns: Vec<PatternElement>,
        is_wildcard: bool,
        name: Option<String>,
    },
    Identifier(Identifier),
}

/// An inherit statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Inherit {
    pub name: Option<Ast>,
    pub items: Vec<(Span, String)>,
}

/// An Identifier.
#[derive(Default, Debug, Clone, Eq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
    pub var: OnceCell<Ty>,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Identifier {
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
            var: OnceCell::new(),
        }
    }
}

impl hash::Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
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
        attrs: HashMap<Identifier, Ast>,
        inherit: Vec<Inherit>,
        is_recursive: bool,
        span: Span,
    },

    /// Let expression
    LetBinding {
        bindings: Vec<(Identifier, Ast)>,
        inherit: Vec<Inherit>,
        body: Box<Ast>,
        span: Span,
    },

    /// Function
    Lambda {
        pattern: Pattern,
        body: Box<Ast>,
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
        exprs: Vec<Ast>,
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
    pub fn from_parser_ast(value: ParserAst, source: &str) -> Self {
        transform_ast(value, source)
    }

    /// Tries to convert the ast to an identifier and then return name as string.
    pub fn as_identifier_str(&self) -> InferResult<String> {
        match self {
            Ast::Identifier(Identifier { name, .. }) => Ok(name.to_string()),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    pub fn get_identifier(&self) -> InferResult<&Identifier> {
        match self {
            Ast::Identifier(ident) => Ok(ident),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Get the span of the ast.
    pub fn get_span(&self) -> &Span {
        match self {
            Ast::UnaryOp { span, .. }
            | Ast::BinaryOp { span, .. }
            | Ast::AttrSet { span, .. }
            | Ast::LetBinding { span, .. }
            | Ast::Lambda { span, .. }
            | Ast::Conditional { span, .. }
            | Ast::Assertion { span, .. }
            | Ast::With { span, .. }
            | Ast::Identifier(Identifier { span, .. })
            | Ast::List { span, .. }
            | Ast::NixString(span)
            | Ast::NixPath(span)
            | Ast::Bool { span, .. }
            | Ast::Int { span, .. }
            | Ast::Float { span, .. }
            | Ast::Null(span)
            | Ast::Comment(span)
            | Ast::DocComment(span)
            | Ast::LineComment(span) => span,
        }
    }

    /// Tries to get the node at the given position.
    /// If not possible, return smallest surrounding node.
    pub fn get_ident_at(&self, position: usize) -> Option<&Identifier> {
        let containing = match self {
            Ast::UnaryOp { rhs, .. } => {
                if rhs.get_span().contains(&position) {
                    rhs.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::BinaryOp { lhs, rhs, .. } => {
                if lhs.get_span().contains(&position) {
                    lhs.get_ident_at(position)
                } else if rhs.get_span().contains(&position) {
                    rhs.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::AttrSet { attrs, .. } => attrs.iter().find_map(|(_, expr)| {
                if expr.get_span().contains(&position) {
                    expr.get_ident_at(position)
                } else {
                    None
                }
            }),
            Ast::LetBinding { bindings, body, .. } => {
                if body.get_span().contains(&position) {
                    body.get_ident_at(position)
                } else {
                    bindings.iter().find_map(|(ident, expr)| {
                        if ident.span.contains(&position) {
                            Some(ident)
                        } else if expr.get_span().contains(&position) {
                            expr.get_ident_at(position)
                        } else {
                            None
                        }
                    })
                }
            }
            Ast::Lambda { body, .. } => {
                if body.get_span().contains(&position) {
                    body.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::Conditional {
                condition,
                expr1,
                expr2,
                ..
            } => {
                if condition.get_span().contains(&position) {
                    condition.get_ident_at(position)
                } else if expr1.get_span().contains(&position) {
                    expr1.get_ident_at(position)
                } else if expr2.get_span().contains(&position) {
                    expr2.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::Assertion {
                condition, expr, ..
            } => {
                if condition.get_span().contains(&position) {
                    condition.get_ident_at(position)
                } else if expr.get_span().contains(&position) {
                    expr.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::With { set, body, .. } => {
                if set.get_span().contains(&position) {
                    set.get_ident_at(position)
                } else if body.get_span().contains(&position) {
                    body.get_ident_at(position)
                } else {
                    None
                }
            }
            Ast::Identifier(ident @ Identifier { span, .. }) => {
                if span.contains(&position) {
                    Some(ident)
                } else {
                    None
                }
            }
            Ast::List { exprs, .. } => exprs.iter().find_map(|expr| {
                if expr.get_span().contains(&position) {
                    expr.get_ident_at(position)
                } else {
                    None
                }
            }),
            Ast::NixString(..)
            | Ast::NixPath(..)
            | Ast::Bool { .. }
            | Ast::Int { .. }
            | Ast::Float { .. }
            | Ast::Null(..)
            | Ast::Comment(..)
            | Ast::DocComment(..)
            | Ast::LineComment(..) => None,
        };
        containing
    }

    /// Collect all identifiers in the ast.
    pub fn collect_identifiers(&self) -> Vec<&Identifier> {
        let mut ret = vec![];
        self.collect_identifiers_inner(&mut ret);
        ret
    }

    fn collect_identifiers_inner<'a>(&'a self, ret: &mut Vec<&'a Identifier>) {
        match self {
            Ast::UnaryOp { rhs, .. } => rhs.collect_identifiers_inner(ret),
            Ast::BinaryOp { lhs, rhs, .. } => {
                lhs.collect_identifiers_inner(ret);
                rhs.collect_identifiers_inner(ret);
            }
            Ast::AttrSet { attrs, .. } => {
                for (_, expr) in attrs.iter() {
                    expr.collect_identifiers_inner(ret);
                }
            }
            Ast::LetBinding { bindings, body, .. } => {
                for (ident, expr) in bindings.iter() {
                    ret.push(ident);
                    expr.collect_identifiers_inner(ret);
                }
                body.collect_identifiers_inner(ret);
            }
            Ast::Lambda { body, pattern, .. } => {
                body.collect_identifiers_inner(ret);
                match pattern {
                    Pattern::Identifier(ident) => {
                        ret.push(ident);
                    }
                    Pattern::Record { patterns, .. } => {
                        for pat in patterns.iter() {
                            match pat {
                                PatternElement::Identifier(ident) => ret.push(ident),
                                PatternElement::DefaultIdentifier(ident, expr) => {
                                    ret.push(ident);
                                    expr.collect_identifiers_inner(ret);
                                }
                            }
                        }
                    }
                }
            }
            Ast::Conditional {
                condition,
                expr1,
                expr2,
                ..
            } => {
                condition.collect_identifiers_inner(ret);
                expr1.collect_identifiers_inner(ret);
                expr2.collect_identifiers_inner(ret);
            }
            Ast::Assertion {
                condition, expr, ..
            } => {
                condition.collect_identifiers_inner(ret);
                expr.collect_identifiers_inner(ret);
            }
            Ast::With { set, body, .. } => {
                set.collect_identifiers_inner(ret);
                body.collect_identifiers_inner(ret);
            }
            Ast::Identifier(ident) => ret.push(ident),
            Ast::List { exprs, .. } => {
                for expr in exprs.iter() {
                    expr.collect_identifiers_inner(ret);
                }
            }
            _ => {}
        }
    }
}

/// Convert [ParserAst] to [Ast].
/// Replace every occurence of an identifier with an [Identifier].
fn transform_ast(value: ParserAst, source: &str) -> Ast {
    use Ast::*;
    match value {
        ParserAst::UnaryOp { op, box rhs, span } => UnaryOp {
            op,
            rhs: Box::new(transform_ast(rhs, source)),
            span,
        },
        ParserAst::BinaryOp {
            op,
            box lhs,
            box rhs,
            span,
        } => BinaryOp {
            op,
            lhs: Box::new(transform_ast(lhs, source)),
            rhs: Box::new(transform_ast(rhs, source)),
            span,
        },
        ParserAst::AttrSet {
            attrs,
            is_recursive,
            span,
            inherit,
        } => {
            let attrs = attrs
                .into_iter()
                .map(|(name, expr)| {
                    let ident = crate::Identifier::new(source[name.clone()].to_string(), name);
                    (ident, transform_ast(expr, source))
                })
                .collect();
            Ast::AttrSet {
                attrs,
                is_recursive,
                inherit: inherit
                    .into_iter()
                    .map(|inherit| Inherit {
                        name: inherit.name.map(|name| transform_ast(name, source)),
                        items: inherit
                            .items
                            .into_iter()
                            .map(|item| (item.clone(), source[item].to_string()))
                            .collect(),
                    })
                    .collect(),
                span,
            }
        }
        ParserAst::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            let bindings: Vec<(crate::Identifier, Ast)> = bindings
                .into_iter()
                .map(|(span, expr)| {
                    let ident = crate::Identifier::new(source[span.clone()].to_string(), span);
                    (ident, transform_ast(expr, source))
                })
                .collect();

            let body = transform_ast(*body, source);

            LetBinding {
                bindings,
                inherit: inherit
                    .into_iter()
                    .map(|inherit| Inherit {
                        name: inherit.name.map(|name| transform_ast(name, source)),
                        items: inherit
                            .items
                            .into_iter()
                            .map(|item| (item.clone(), source[item].to_string()))
                            .collect(),
                    })
                    .collect(),
                body: Box::new(body),
                span,
            }
        }
        ParserAst::Lambda {
            pattern,
            body,
            span,
        } => {
            let mut idents = vec![];
            let pattern = match pattern {
                parser::ast::Pattern::Set {
                    patterns,
                    is_wildcard,
                    name,
                } => {
                    let patterns = patterns.into_iter().map(|patt| match patt {
                        parser::ast::PatternElement::Identifier(name) => {
                            let ident =
                                crate::Identifier::new(source[name.clone()].to_string(), name);
                            idents.push(ident.clone());
                            PatternElement::Identifier(ident)
                        }
                        parser::ast::PatternElement::DefaultIdentifier {
                            identifier,
                            span,
                            ast,
                        } => {
                            let ident =
                                crate::Identifier::new(source[identifier].to_string(), span);
                            idents.push(ident.clone());
                            PatternElement::DefaultIdentifier(ident, transform_ast(ast, source))
                        }
                    });
                    Pattern::Record {
                        patterns: patterns.collect(),
                        is_wildcard,
                        name: name.map(|name| source[name].to_string()),
                    }
                }
                parser::ast::Pattern::Identifier(ident) => Pattern::Identifier(
                    crate::Identifier::new(source[ident.clone()].to_string(), ident),
                ),
            };

            let body = transform_ast(*body, source);
            Lambda {
                pattern,
                body: Box::new(body),
                span,
            }
        }
        ParserAst::Conditional {
            box condition,
            box expr1,
            box expr2,
            span,
        } => Conditional {
            condition: Box::new(transform_ast(condition, source)),
            expr1: Box::new(transform_ast(expr1, source)),
            expr2: Box::new(transform_ast(expr2, source)),
            span,
        },
        ParserAst::Assertion {
            box condition,
            span,
            box expr,
        } => Assertion {
            condition: Box::new(transform_ast(condition, source)),
            expr: Box::new(transform_ast(expr, source)),
            span,
        },
        ParserAst::With {
            box set,
            box body,
            span,
        } => With {
            set: Box::new(transform_ast(set, source)),
            body: Box::new(transform_ast(body, source)),
            span,
        },
        ParserAst::Identifier(span) => {
            Identifier(ast::Identifier::new(source[span.clone()].to_string(), span))
        }

        ParserAst::NixString(span) => NixString(span),
        ParserAst::NixPath(span) => NixPath(span),
        ParserAst::List { exprs, span } => List {
            exprs: exprs
                .into_iter()
                .map(|l| transform_ast(l, source))
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
        ParserAst::SearchPath(_path) => todo!(),
    }
}
