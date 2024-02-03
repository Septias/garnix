//! Abstract syntax tree for the nix language.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use logos::Span;

use crate::lexer::Token;

/// Part of a [Pattern].
pub enum PatternElement {
    /// Pattern of the form `ident`
    Identifier(Span),
    /// Pattern of the form `ident ? <default>`
    DefaultIdentifier(Span, Ast),
}

/// A pattern.
pub struct Pattern {
    /// A list of patterns
    pub patterns: Vec<PatternElement>,
    /// Is widcard
    pub is_wildcard: bool,
}

/// Binary operators.
pub enum BinOp {
    // Function application
    Application,
    // List operators
    ListConcat,
    // Arithematic operators
    Mul,
    Div,
    Add,
    Sub,
    // Set operators
    Update,
    HasAttribute,
    AttributeSelection,
    AttributeFallback,
    // Comparison operators
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    // Logic operators
    And,
    Or,
    Implication,
}

impl BinOp {
    pub fn get_precedence(&self) -> (u8, u8) {
        match self {
            Self::AttributeFallback => (1, 2),
            Self::AttributeSelection => (3, 4),
            Self::Application => (5, 6),
            // Arithematic negation with 7
            Self::HasAttribute => (9, 10),
            Self::ListConcat => (12, 11), // Right associative
            Self::Mul | Self::Div => (13, 14),
            Self::Add | Self::Sub => (15, 16),
            // Logic negation with 17
            Self::Update => (20, 19), // Right associative
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => {
                (21, 22)
            }
            Self::Equal | Self::NotEqual => (23, 24),
            Self::And => (25, 26),
            Self::Or => (27, 28),
            Self::Implication => (29, 30),
        }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        use Token::*;
        match token {
            Equal => Some(BinOp::Equal),
            NotEqual => Some(BinOp::NotEqual),
            LessThan => Some(BinOp::LessThan),
            LessThanEqual => Some(BinOp::LessThanEqual),
            GreaterThan => Some(BinOp::GreaterThan),
            GreaterThanEqual => Some(BinOp::GreaterThanEqual),
            And => Some(BinOp::And),
            Or => Some(BinOp::Or),
            Implication => Some(BinOp::Implication),
            Plus => Some(BinOp::Add),
            Minus => Some(BinOp::Sub),
            Star => Some(BinOp::Mul),
            Slash => Some(BinOp::Div),
            Update => Some(BinOp::Update),
            ListConcat => Some(BinOp::ListConcat),
            AttributeFallback => Some(BinOp::AttributeFallback),
            Dot => Some(BinOp::AttributeSelection),
            Question => Some(BinOp::HasAttribute),
            _ => {
                // TODO: handle this
                Some(BinOp::Application)
            }
        }
    }
}

/// Unary operator.
pub enum UnOp {
    Negation,
    LogicalNegation,
}

impl UnOp {
    pub fn get_precedence(&self) -> u8 {
        match self {
            Self::Negation => 7,
            Self::LogicalNegation => 17,
        }
    }
}

/// Ast for the the nix language
#[repr(u8)]
pub enum Ast {
    /// ----------------- Operators -----------------

    /// Unary Operators
    UnaryOperator {
        op: UnOp,
        rhs: Box<Ast>,
    },

    /// Binary Operators
    BinaryOperator {
        op: BinOp,
        lhs: Box<Ast>,
        rhs: Box<Ast>,
    },

    /// ----------------- Language Constructs -----------------

    /// Attribute set
    /// parsed by [crate::parser::set]
    AttrSet {
        /// A set of attributes
        attrs: Vec<(Span, Ast)>,
        is_recursive: bool,
    },

    /// Let expression
    /// parsed by [crate::parser::let_binding]
    LetBinding {
        /// A set of bindings
        bindings: Vec<(Span, Ast)>,
        /// The expression to evaluate
        body: Box<Ast>,
        /// A list of identifiers to inherit from the parent scope
        inherit: Option<Vec<Span>>,
    },

    /// Function
    /// func = pattern: body
    /// parsed by [crate::parser::lambda]
    Lambda {
        arguments: Vec<Pattern>,
        body: Box<Ast>,
        arg_binding: Option<Span>,
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
    },

    /// An assert statement.
    /// parsed by [crate::parser::assert]
    Assertion {
        /// The condition to evaluate
        condition: Box<Ast>,
        /// The expression to evaluate if the condition is true
        then: Box<Ast>,
    },

    /// A with-statement.
    /// parsed by [crate::parser::with]
    With {
        /// The set-identifier to add
        set: Box<Ast>,
        /// The expression to evaluate
        body: Box<Ast>,
    },

    /// ----------------- Literals -----------------
    Identifier(Span),

    /// Primitives
    NixString(Span),
    NixPath(Span),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Null,

    /// Comments
    Comment(Span),
    DocComment(Span),
    LineComment(Span),
}

impl Ast {
    pub fn as_span(&self) -> Span {
        match &self {
            Self::Identifier(s) => s.clone(),
            _ => Span::default(),
        }
    }
}
