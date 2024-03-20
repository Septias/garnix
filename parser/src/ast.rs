//! Abstract syntax tree for the Nix language.

use logos::Span;
use strum_macros::{AsRefStr, Display, EnumDiscriminants};

use crate::lexer::Token;

/// Part of a [Pattern].
#[derive(Debug, Clone, PartialEq)]
pub enum PatternElement {
    /// Pattern of the form `ident`
    Identifier(Span),
    /// Pattern of the form `ident ? <default>`
    DefaultIdentifier {
        identifier: Span,
        span: Span,
        ast: Ast,
    },
}

/// A pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Set {
        /// A list of Identifiers
        patterns: Vec<PatternElement>,
        /// Is widcard
        is_wildcard: bool,
        /// An optional name binding
        name: Option<Span>,
    },
    Identifier(Span),
}

/// Binary operators.
#[derive(Debug, Clone, PartialEq, AsRefStr, EnumDiscriminants)]
#[strum_discriminants(derive(AsRefStr, Display))]
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
            Self::Implication => (2, 1),
            Self::Or => (3, 4),
            Self::And => (5, 6),
            Self::Equal | Self::NotEqual => (7, 8),
            Self::LessThan | Self::LessThanEqual => (9, 10),
            Self::GreaterThan | Self::GreaterThanEqual => (9, 10),
            Self::Update => (12, 11), // Right associative
            // Negation with 13
            Self::Add | Self::Sub => (15, 16),
            Self::Mul | Self::Div => (17, 18),
            Self::ListConcat => (20, 19),
            Self::HasAttribute => (21, 22),
            // Arithmetic negation with 23
            Self::Application => (25, 26),
            Self::AttributeSelection => (27, 28),
            Self::AttributeFallback => (29, 30),
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
            _ => None,
        }
    }
}

/// Unary operator.
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Inherit {
    pub name: Option<Ast>,
    pub items: Vec<Span>,
}

/// Ast for the the nix language
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, AsRefStr)]
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
        attrs: Vec<(Span, Ast)>,
        inherit: Vec<Inherit>,
        is_recursive: bool,
        span: Span,
    },

    /// Let expression
    /// parsed by [crate::parser::let_binding]
    LetBinding {
        /// A set of bindings
        bindings: Vec<(Span, Ast)>,
        /// The expression to evaluate
        body: Box<Ast>,
        /// A list of identifiers to inherit from the parent scope
        inherit: Vec<Inherit>,
        span: Span,
    },

    /// Function
    /// func = pattern: body
    /// parsed by [crate::parser::lambda]
    Lambda {
        pattern: Pattern,
        body: Box<Ast>,
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
        expr: Box<Ast>,
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

    /// ----------------- Literals -----------------
    Identifier(Span),
    List {
        exprs: Vec<Ast>,
        span: Span,
    },

    /// Primitives
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
    SearchPath(Span),

    /// Comments
    Comment(Span),
    DocComment(Span),
    LineComment(Span),
}

impl Ast {
    pub fn as_span(&self) -> Span {
        match &self {
            Ast::Identifier(span)
            | Ast::NixString(span)
            | Ast::NixPath(span)
            | Ast::Comment(span)
            | Ast::DocComment(span)
            | Ast::LineComment(span)
            | Ast::UnaryOp { span, .. }
            | Ast::BinaryOp { span, .. }
            | Ast::AttrSet { span, .. }
            | Ast::LetBinding { span, .. }
            | Ast::Lambda { span, .. }
            | Ast::Conditional { span, .. }
            | Ast::Assertion { span, .. }
            | Ast::With { span, .. }
            | Ast::List { span, .. }
            | Ast::Bool { span, .. }
            | Ast::Int { span, .. }
            | Ast::Float { span, .. }
            | Ast::SearchPath(span)
            | Ast::Null(span) => span.clone(),
        }
    }
}
