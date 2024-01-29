//! Abstract syntax tree for the nix language.
#![allow(unused)]

use std::{collections::HashMap, hash::Hash};

/// Binary operators of the Nix language ordered by precedence.
pub enum BinOp {
    /// Arithmetic operators
    Mul,
    Div,

    Add,
    Sub,

    /// String & Path operators
    ConcatString,
    ConcatPath,
    ConcatStringPath,
    ConcatPathString,

    /// Set operators
    Update,

    /// Comparison operators
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    /// Logical operators
    Equal,
    NotEqual,
    And,
    Or,
    Implication,
}

/// Ast for the the nix language
#[repr(u8)]
pub enum Ast<'a> {
    /// ----------------- Operators -----------------
    /// No associativity
    /// Precendence: 1
    AttributeSelection {
        /// Attrset
        set: Box<Ast<'a>>,
        /// Attrpath
        attribute: Box<Ast<'a>>,
        /// or Expr
        or: Option<Box<Ast<'a>>>,
    },

    /// Left associativity
    /// Precendence: 2
    /// Func expr
    Application {
        function: Box<Ast<'a>>,
        argument: Box<Ast<'a>>,
    },

    /// No Associativity
    /// Precendence: 3
    Negation(Box<Ast<'a>>),

    /// No Associativity
    /// Precendence: 4
    HasAttribute {
        /// Attrset
        set: Box<Ast<'a>>,
        /// Attrpath
        attribute: Box<Ast<'a>>,
    },

    /// Right associativity
    /// Precendence: 5
    ListConcat {
        lhs: Box<Ast<'a>>,
        rhs: Box<Ast<'a>>,
    },

    /// Associativity: Left
    /// [Mul], [Div] Presedence: 6
    /// [Sub], [Add], String con, path con Presedence: 7
    /// -- logical Negation Presedence: 8
    /// [Update] Presedence: 9
    /// [LessThan], [LessThanOrEqual], [GreaterThan], [GreaterThanOrEqual] Presedence: 10
    /// [Equal], [NotEqual] Presedence: 11
    /// [And] Presedence: 12
    /// [Or] Presedence: 13
    /// [Implication]
    BinOp {
        op: BinOp,
        lhs: Box<Ast<'a>>,
        rhs: Box<Ast<'a>>,
    },

    /// Associativity: Right
    /// Precendence: 8
    LogicalNegation(Box<Ast<'a>>),

    /// ----------------- Lange Constructs -----------------

    /// Attributeset
    /// rec-attrset = rec { [ name = expr ; ]... }
    AttrSet {
        /// A set of attributes
        attrs: HashMap<&'a str, (Ast<'a>)>,
        is_recursive: bool,
    },

    /// Let expression
    /// let-expr = let [ identifier = expr ; ]... in expr
    Let {
        /// A set of bindings
        bindings: Vec<(&'a str, Ast<'a>)>,
        /// The expression to evaluate
        body: Box<Ast<'a>>,
        /// A list of identifiers to inherit from the parent scope
        inherit: Option<Vec<&'a str>>,
    },

    /// Function
    /// func = pattern: body
    Lambda {
        pattern: Box<Ast<'a>>,
        body: Box<Ast<'a>>,
        arg_binding: Option<&'a str>,
    },

    /// Pattern
    Pattern {
        /// A list of patterns
        patterns: Vec<Ast<'a>>,
        /// Is widcard
        is_wildcard: bool,
    },

    /// Conditional
    /// if-expr = if expr then expr else expr
    Conditional {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        expr1: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is false
        expr2: Box<Ast<'a>>,
    },

    Assertion {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        then: Box<Ast<'a>>,
    },

    With {
        /// The set to evaluate
        set: Box<Ast<'a>>,
        /// The expression to evaluate
        body: Box<Ast<'a>>,
    },

    Comment(&'a str),
    Identifier(&'a str),
    IndetifierWDefault(&'a str, Box<Ast<'a>>),
}

impl<'a> Ast<'a> {
    pub fn to_string(&self) -> &'a str {
        match &self {
            Self::Identifier(s) => s,
            _ => "",
        }
    }
}
