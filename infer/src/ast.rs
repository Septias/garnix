use super::{helpers::fold_path, InferError, InferResult};
use crate::Type;
use core::str;
use logos::Span;
use parser::ast::{Ast as ParserAst, BinOp, BinOpDiscriminants, UnOp};
use std::{
    cell::RefCell,
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
pub struct Pattern {
    pub patterns: Vec<PatternElement>,
    pub is_wildcard: bool,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
/// An Identifier.
/// TODO: is this Eq implemenation correct?
pub struct Identifier {
    pub debrujin: usize,
    pub name: String,
    pub span: Span,
    pub lower_bounds: RefCell<Vec<Type>>,
    pub upper_bounds: RefCell<Vec<Type>>,
}

impl Identifier {
    pub fn new(debrujin: usize, name: String, span: Span) -> Self {
        Self {
            debrujin,
            name,
            span,
            lower_bounds: RefCell::new(vec![]),
            upper_bounds: RefCell::new(vec![]),
        }
    }

    pub fn add_lb(&self, ty: Type) {
        self.lower_bounds.borrow_mut().push(ty);
    }

    pub fn add_ub(&self, ty: Type) {
        self.upper_bounds.borrow_mut().push(ty);
    }
}

impl hash::Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.debrujin.hash(state);
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
        is_recursive: bool,
        inherit: Vec<(String, Span)>,
        span: Span,
    },

    /// Let expression
    LetBinding {
        bindings: Vec<(Identifier, Ast)>,
        body: Box<Ast>,
        inherit: Vec<(String, Span)>,
        span: Span,
    },

    /// Function
    Lambda {
        arguments: Vec<Pattern>,
        body: Box<Ast>,
        arg_binding: Option<Identifier>,
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
        let mut vars = Cache::new();
        transform_ast(value, &mut vars, source)
    }

    /// Tries to convert the ast to an [Identifier] and adds as many path elements as possible.
    pub fn last_debrujin(&self) -> Result<usize, InferError> {
        match self {
            Ast::Identifier(Identifier { debrujin, .. }) => Ok(*debrujin),
            Ast::BinaryOp {
                op: BinOp::AttributeSelection,
                rhs,
                ..
            } => Ok(fold_path(rhs)?),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to a list.
    pub fn as_list(&self) -> InferResult<&Vec<Ast>> {
        match self {
            Ast::List { exprs, span: _ } => Ok(exprs),
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
    pub fn as_identifier_str(&self) -> InferResult<String> {
        match self {
            Ast::Identifier(Identifier { name, .. }) => Ok(name.to_string()),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// TODO: remove
    pub fn get_identifier(&self) -> InferResult<&Identifier> {
        match self {
            Ast::Identifier(ident) => Ok(ident),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to an [Identifier] and then return the De Bruijn index.
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
    #[allow(clippy::type_complexity)]
    pub fn as_attr_set(&self) -> InferResult<(&HashMap<Identifier, Ast>, &Vec<(String, Span)>)> {
        match self {
            Ast::AttrSet { attrs, inherit, .. } => Ok((attrs, inherit)),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::AttrSet.as_ref(),
            }),
        }
    }

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

    pub fn get_node_at(&self, position: usize) -> Option<&Ast> {
        let containing = match self {
            Ast::UnaryOp { rhs, .. } => {
                if rhs.get_span().contains(&position) {
                    rhs.get_node_at(position)
                } else {
                    None
                }
            }
            Ast::BinaryOp { lhs, rhs, .. } => {
                if lhs.get_span().contains(&position) {
                    lhs.get_node_at(position)
                } else if rhs.get_span().contains(&position) {
                    rhs.get_node_at(position)
                } else {
                    None
                }
            }
            Ast::AttrSet { attrs, .. } => attrs.iter().find_map(|(_, expr)| {
                if expr.get_span().contains(&position) {
                    expr.get_node_at(position)
                } else {
                    None
                }
            }),
            Ast::LetBinding { bindings, body, .. } => {
                if body.get_span().contains(&position) {
                    body.get_node_at(position)
                } else {
                    bindings.iter().find_map(|(_, expr)| {
                        if expr.get_span().contains(&position) {
                            expr.get_node_at(position)
                        } else {
                            None
                        }
                    })
                }
            }
            Ast::Lambda { body, .. } => {
                if body.get_span().contains(&position) {
                    body.get_node_at(position)
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
                    condition.get_node_at(position)
                } else if expr1.get_span().contains(&position) {
                    expr1.get_node_at(position)
                } else if expr2.get_span().contains(&position) {
                    expr2.get_node_at(position)
                } else {
                    None
                }
            }
            Ast::Assertion {
                condition, expr, ..
            } => {
                if condition.get_span().contains(&position) {
                    condition.get_node_at(position)
                } else if expr.get_span().contains(&position) {
                    expr.get_node_at(position)
                } else {
                    None
                }
            }
            Ast::With { set, body, .. } => {
                if set.get_span().contains(&position) {
                    set.get_node_at(position)
                } else if body.get_span().contains(&position) {
                    body.get_node_at(position)
                } else {
                    None
                }
            }
            Ast::Identifier(Identifier { span, .. }) => {
                if span.contains(&position) {
                    Some(self)
                } else {
                    None
                }
            }
            Ast::List { exprs, .. } => exprs.iter().find_map(|expr| {
                if expr.get_span().contains(&position) {
                    expr.get_node_at(position)
                } else {
                    None
                }
            }),
            Ast::NixString(span)
            | Ast::NixPath(span)
            | Ast::Bool { span, .. }
            | Ast::Int { span, .. }
            | Ast::Float { span, .. }
            | Ast::Null(span)
            | Ast::Comment(span)
            | Ast::DocComment(span)
            | Ast::LineComment(span) => {
                if span.contains(&position) {
                    Some(self)
                } else {
                    None
                }
            }
        };

        containing.or(if self.get_span().contains(&position) {
            Some(self)
        } else {
            None
        })
    }
}

/// Convert [ParserAst] to [Ast].
/// Replace every occurence of an identifier with an [Identifier].
fn transform_ast(value: ParserAst, cache: &mut Cache, source: &str) -> Ast {
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
            inherit,
        } => {
            let attrs = attrs
                .into_iter()
                .map(|(name, expr)| {
                    let ident = crate::Identifier::new(0, source[name.clone()].to_string(), name);
                    // TODO: Add recursion here
                    (ident, transform_ast(expr, cache, source))
                })
                .collect();
            Ast::AttrSet {
                attrs,
                is_recursive,
                inherit: inherit
                    .into_iter()
                    .map(|inherit| (source[inherit.clone()].to_string(), inherit))
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
                    let ident = crate::Identifier::new(0, source[span.clone()].to_string(), span);
                    (ident, transform_ast(expr, cache, source))
                })
                .collect();

            let mut new = vec![];
            let inherit = if let Some(inherit) = inherit {
                let mut inherits = Vec::with_capacity(inherit.len());
                for name in inherit {
                    let name_name = &source[name.clone()];
                    inherits.push((name_name.to_string(), name.clone()));
                    // TODO: maybe this should only be lookup instead of lookup & create
                    let identifier = cache.lookup(name_name, name.clone());
                    new.push(identifier);
                }
                inherits
            } else {
                vec![]
            };

            new.extend(bindings.iter().map(|(ident, _)| ident.clone()));
            let body = cache.with_scope(new, |cache| transform_ast(*body, cache, source));

            LetBinding {
                bindings,
                body: Box::new(body),
                inherit,
                span,
            }
        }
        ParserAst::Lambda {
            arguments,
            body,
            arg_binding,
            span,
        } => {
            let mut idents = vec![];
            let arguments = arguments
                .into_iter()
                .map(|pat| {
                    let patterns = pat
                        .patterns
                        .into_iter()
                        .map(|patt| match patt {
                            parser::ast::PatternElement::Identifier(name) => {
                                let ident = crate::Identifier::new(
                                    0,
                                    source[name.clone()].to_string(),
                                    name,
                                );
                                idents.push(ident.clone());
                                PatternElement::Identifier(ident)
                            }
                            parser::ast::PatternElement::DefaultIdentifier {
                                identifier,
                                span,
                                ast,
                            } => {
                                let ident =
                                    crate::Identifier::new(0, source[identifier].to_string(), span);
                                idents.push(ident.clone());
                                PatternElement::DefaultIdentifier(
                                    ident,
                                    transform_ast(ast, cache, source),
                                )
                            }
                        })
                        .collect();
                    Pattern {
                        patterns,
                        is_wildcard: pat.is_wildcard,
                    }
                })
                .collect();

            let body = cache.with_scope(idents, |cache| transform_ast(*body, cache, source));

            Lambda {
                arguments,
                body: Box::new(body),
                arg_binding: arg_binding
                    .map(|span| crate::Identifier::new(0, source[span.clone()].to_string(), span)), //TODO: reintroduce
                span,
            }
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
        ParserAst::Identifier(span) => {
            let ident = cache.lookup(&source[span.clone()], span.clone());
            cache.insert(ident.clone());
            Identifier(ident)
        }

        ParserAst::NixString(span) => NixString(span),
        ParserAst::NixPath(span) => NixPath(span),
        ParserAst::List { exprs, span } => List {
            exprs: exprs
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
struct Cache {
    bindings: Vec<Vec<Identifier>>,
}

impl Cache {
    fn new() -> Self {
        Self {
            bindings: vec![vec![]],
        }
    }

    /// Insert a new identifier into the current scope.
    pub(crate) fn insert(&mut self, ident: Identifier) {
        self.bindings.last_mut().unwrap().push(ident);
    }

    /// Add the given scope and execute the given function.
    pub(crate) fn with_scope<T>(
        &mut self,
        bindings: Vec<Identifier>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.bindings.push(bindings);
        let ret = f(self);
        self.bindings.pop();
        ret
    }

    /// Lookup the given name all scopes.
    /// Create a new Identifier if it does not exist.
    pub(crate) fn lookup(&self, name: &str, span: Span) -> Identifier {
        let mut n = 0;
        for scope in self.bindings.iter().rev() {
            for ident in scope.iter().rev() {
                n += 1;
                if ident.name == name {
                    return Identifier {
                        debrujin: n,
                        name: name.to_string(),
                        span,
                        ..Default::default()
                    };
                }
            }
        }
        Identifier {
            debrujin: n,
            name: name.to_string(),
            span,
            ..Default::default()
        }
    }
}
