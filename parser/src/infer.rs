#![allow(unused)]
//! Type inference using HM type system.

use core::str;
use std::{collections::HashMap, os::unix::fs::FileTypeExt};

use anyhow::Context as _;
use logos::Span;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};
use thiserror::Error;

use crate::ast::{
    Ast as ParserAst, BinOp, BinOpDiscriminants, Pattern as ParserPattern,
    PatternElement as ParserPatternElement, UnOp,
};

#[derive(Debug, Error)]
pub enum InferError {
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(String),
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: Type, found: Type },
    #[error("Can't convert {from} to {to}")]
    ConversionError { from: String, to: &'static str },
    #[error("Can't infer type of comment")]
    UnexpectedComment,
    #[error("Can't infer type of assert")]
    UnexpectedAssertion,
    #[error("Unknown function call")]
    UnknownFunction,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

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
    /// parsed by [crate::parser::set]
    AttrSet {
        /// A set of attributes
        attrs: Vec<(usize, Ast)>,
        is_recursive: bool,
        span: Span,
    },

    /// Let expression
    /// parsed by [crate::parser::let_binding]
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
    /// func = pattern: body
    /// parsed by [crate::parser::lambda]
    Lambda {
        arguments: Vec<Pattern>,
        body: Box<Ast>,
        arg_binding: Option<usize>,
        span: Span,
    },

    /// Conditional
    /// parsed by [crate::parser::conditional]
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
    /// parsed by [crate::parser::assert]
    Assertion {
        /// The condition to evaluate
        condition: Box<Ast>,
        /// The expression to evaluate if the condition is true
        span: Span,
    },

    /// A with-statement.
    /// parsed by [crate::parser::with]
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

type InferResult<T> = Result<T, InferError>;

fn infer_error<T>(expected: Type, found: Type) -> Result<T, InferError> {
    Err(InferError::TypeMismatch { expected, found })
}

fn fold_path(ast: &Ast, path: &mut Vec<String>) -> InferResult<()> {
    match ast {
        Ast::BinaryOp { op, lhs, rhs, span } => match op {
            BinOp::AttributeSelection => {
                fold_path(lhs, path);
                path.push(rhs.to_identifier_string()?);
            }
            _ => (),
        },
        _ => (),
    }
    Ok(())
}

impl Ast {
    /// Convert a parser ast to an infer ast.
    fn from_parser_ast(value: ParserAst, source: String) -> Self {
        let mut vars = Cache::new();
        transform_ast(value, &mut vars, &source, 0)
    }

    /// Tries to convert the ast to an [Identifier] and adds as many path elements as possible.
    fn as_ident(&self) -> InferResult<Identifier> {
        match self {
            Ast::Identifier { debrujin, .. } => Ok(Identifier {
                name: *debrujin,
                path: None,
            }),
            Ast::BinaryOp {
                op: BinOp::AttributeSelection,
                lhs,
                rhs,
                span,
            } => {
                let mut path = vec![];
                fold_path(rhs, &mut path)?;
                Ok(Identifier {
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
    fn as_list(&self) -> InferResult<&Vec<Ast>> {
        match self {
            Ast::List { items, span } => Ok(items),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::List.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to a function and then return the arguments.
    fn to_application(&self) -> InferResult<(&Ast, &Ast)> {
        match self {
            Ast::BinaryOp {
                op,
                box lhs,
                box rhs,
                span,
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
    fn to_identifier_string(&self) -> InferResult<String> {
        match self {
            Ast::Identifier { name: text, .. } => Ok(text.to_string()),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
            }),
        }
    }

    /// Tries to convert the ast to an identifier and then return the De Bruijn index.
    fn as_debrujin(&self) -> InferResult<usize> {
        match self {
            Ast::Identifier { debrujin, .. } => Ok(*debrujin),
            e => Err(InferError::ConversionError {
                from: e.as_ref().to_string(),
                to: AstDiscriminants::Identifier.as_ref(),
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
    fun_depth: usize,
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
                        cache.get(&source[name]),
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
                        cache.get(&source[name]),
                        transform_ast(expr, cache, source, fun_depth),
                    )
                })
                .collect();
            let inherit = inherit.map(|inherit| {
                inherit
                    .into_iter()
                    .map(|name| cache.get(&source[name]))
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
            arguments,
            body,
            arg_binding,
            span,
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
            debrujin: todo!(),
            name: source[span].to_string(),
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Identifier {
    name: usize,
    path: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Display, Default, EnumDiscriminants)]
#[strum_discriminants(derive(AsRefStr, Display))]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Identifier(Identifier),
    Null,
    Undefined,
    List(Vec<Type>),
    Function(Box<Type>, Box<Type>),
    Union(Box<Type>, Box<Type>),
    Set(HashMap<String, Type>),
    Var(String),
    #[default]
    Default,
}

impl Type {
    fn as_list(self) -> InferResult<Vec<Type>> {
        match self {
            Type::List(elems) => Ok(elems),
            t => infer_error(Type::List(vec![]), t.clone()),
        }
    }

    fn as_ident(self) -> InferResult<Identifier> {
        match self {
            Type::Identifier(i) => Ok(i),
            t => infer_error(Type::Identifier(Identifier::default()), t),
        }
    }

    fn as_function(&self) -> InferResult<(&Type, &Type)> {
        match self {
            Type::Function(box lhs, box rhs) => Ok((lhs, rhs)),
            t => infer_error(Type::Function(Box::default(), Box::default()), t.clone()),
        }
    }
}

type Constraint = (Identifier, Type);

pub(crate) struct Context(Vec<Vec<(usize, Type)>>);

/// Context to save variables and their types.
impl Context {
    pub(crate) fn new() -> Self {
        Self(vec![Vec::new()])
    }

    pub(crate) fn push_scope(&mut self) {
        self.0.push(Vec::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub(crate) fn insert(&mut self, name: usize, ty: Type) {
        self.0.last_mut().unwrap().push((name, ty));
    }

    pub(crate) fn lookup(&self, name: &usize) -> Option<&Type> {
        for scope in self.0.iter().rev() {
            for (n, ty) in scope.iter().rev() {
                if n == name {
                    return Some(ty);
                }
            }
        }
        None
    }
}

/// Lookup all bindings that are part of a `with`-expression and add them to the context.
/// This does not create a new scope
fn lookup_set_bindigs(context: &mut Context, bindings: &Ast) -> InferResult<()> {
    match bindings {
        Ast::AttrSet {
            attrs,
            is_recursive,
            span,
        } => {
            for (name, expr) in attrs {
                let ty = context
                    .lookup(name)
                    .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
                context.insert(*name, ty.clone());
            }
        }
        _ => todo!(),
    }
    Ok(())
}

/// Arguments:
/// - the function type
/// - the arguments that should be supplied to the function in form of application(lhs, application(lhs, ..))
fn reduce_function<'a>(
    function: &'a Type,
    arguments: &Ast,
    constraints: &mut Vec<Constraint>,
) -> InferResult<&'a Type> {
    let (from, to) = function.as_function().context("can't unpack function")?;

    // find out if this is a stacked function
    if matches!(to, Type::Function(_, _)) {
        // extract the last argument from the chain
        if let Ok((arg, next)) = arguments.to_application() {
            // further reduce the function
            let ret = reduce_function(to, arguments, constraints)?;

            // if it is an identifier we can formulate a constraint
            if let Ok(ident) = arg.as_ident() {
                constraints.push((ident, ret.clone()));
            }

            Ok(ret)
        } else {
            // If there is no argument to apply, just return a partial function
            Ok(function.as_function().context("can't unpack' function")?.1)
        }
    } else {
        if let Ok(ident) = arguments.as_ident() {
            constraints.push((ident, from.clone()));
        }
        Ok(to)
    }
}

/// Infer the type of an expression.
fn hm(context: &mut Context, expr: &Ast) -> Result<Type, InferError> {
    use Type::*;
    match expr {
        Ast::UnaryOp { rhs, .. } => hm(context, rhs),
        Ast::BinaryOp {
            op,
            lhs,
            box rhs,
            span,
        } => {
            let ty1 = hm(context, lhs)?;
            let ty2 = hm(context, rhs)?;

            match op {
                BinOp::Application => {
                    let fun = ty1.as_ident()?;
                    let fun_type = context
                        .lookup(&fun.name)
                        .ok_or(InferError::UnknownFunction)?;
                    if let Some(path) = fun.path {
                        todo!()
                    }
                    let mut constraint = vec![];
                    let typ = reduce_function(fun_type, rhs, &mut constraint)?;
                    Ok(typ.clone())
                }
                BinOp::ListConcat => {
                    let mut lhs = ty1.as_list()?;
                    let rhs = ty2.as_list()?;
                    Ok(Type::List([lhs, rhs].concat()))
                }
                BinOp::Mul => todo!(),
                BinOp::Div => todo!(),
                BinOp::Add => todo!(),
                BinOp::Sub => todo!(),
                BinOp::Update => todo!(),
                BinOp::HasAttribute => todo!(),
                BinOp::AttributeSelection => todo!(),
                BinOp::AttributeFallback => todo!(),
                BinOp::LessThan => todo!(),
                BinOp::LessThanEqual => todo!(),
                BinOp::GreaterThan => todo!(),
                BinOp::GreaterThanEqual => todo!(),
                BinOp::Equal => todo!(),
                BinOp::NotEqual => todo!(),
                BinOp::And => todo!(),
                BinOp::Or => todo!(),
                BinOp::Implication => todo!(),
            }
        }
        Ast::AttrSet {
            attrs,
            is_recursive,
            span,
        } => Ok(Set(attrs
            .iter()
            .map(|(name, expr)| ("".to_string(), hm(context, expr).unwrap()))
            .collect::<HashMap<_, _>>())),

        Ast::LetBinding {
            bindings,
            body,
            inherit,
            span,
        } => {
            context.push_scope();
            for (name, expr) in bindings {
                let ty = hm(context, expr)?;
                context.insert(*name, ty);
            }
            if let Some(inherit) = inherit {
                for name in inherit {
                    let ty = context
                        .lookup(name)
                        .ok_or(InferError::UnknownIdentifier("".to_string()))?; // Maybe don't make this a hard error
                    context.insert(*name, ty.clone());
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();
            Ok(ty)
        }
        Ast::Lambda {
            arguments,
            body,
            arg_binding,
            span,
        } => {
            context.push_scope();
            for patt in arguments {
                for patt in &patt.patterns {
                    match patt {
                        PatternElement::Identifier(name) => {
                            let ty = context
                                .lookup(name)
                                .ok_or(InferError::UnknownIdentifier("todo".to_string()))?;
                            context.insert(*name, ty.clone());
                        }
                        PatternElement::DefaultIdentifier(name, default) => {
                            let ty1 = context.lookup(name).cloned();
                            let ty2 = hm(context, default)?;

                            if let Some(ty1) = ty1 {
                                if ty1 != ty2 {
                                    return Err(InferError::TypeMismatch {
                                        expected: ty2,
                                        found: ty1,
                                    });
                                }
                            }

                            context.insert(*name, ty2);
                        }
                    }
                }
            }
            let ty = hm(context, body)?;
            context.pop_scope();

            /// TODO: somehow create curry style functions here
            Ok(Function(Box::new(Undefined), Box::new(ty)))
        }
        Ast::Conditional {
            condition,
            expr1,
            expr2,
            span,
        } => {
            let ty = hm(context, condition)?;
            if ty != Bool {
                return Err(InferError::TypeMismatch {
                    expected: Bool,
                    found: ty,
                });
            }
            let ty1 = hm(context, expr1)?;
            let ty2 = hm(context, expr2)?;
            if ty1 != ty2 {
                Ok(Union(Box::new(ty1), Box::new(ty2)))
            } else {
                Ok(ty1)
            }
        }
        Ast::Assertion { condition, span } => Err(InferError::UnexpectedComment),
        Ast::With { set, body, span } => {
            context.push_scope();
            lookup_set_bindigs(context, &set.as_ref().clone())?;
            hm(context, body)
        }
        Ast::Identifier { debrujin, .. } => {
            Ok(context.lookup(debrujin).cloned().unwrap_or(Undefined))
        }
        Ast::List { items, span } => Ok(Type::List(
            items.iter().flat_map(|ast| hm(context, ast)).collect(),
        )),
        Ast::NixString(_) => Ok(String),
        Ast::NixPath(_) => Ok(String),
        Ast::Bool { val, span } => Ok(Bool),
        Ast::Int { val, span } => Ok(Int),
        Ast::Float { val, span } => Ok(Float),
        Ast::Null(_) => Ok(Null),
        Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
    }
}

pub fn infer(expr: &Ast) -> Result<Type, InferError> {
    let mut context = Context::new();
    hm(&mut context, expr)
}
