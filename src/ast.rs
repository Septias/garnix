//! Abstract syntax tree for the nix language.
#![allow(unused)]

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

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

pub enum PatternElement<'a> {
    /// Pattern of the form `ident`
    Identifier(&'a str),
    /// Pattern of the form `ident ? <default>`
    DefaultIdentifier(&'a str, Ast<'a>),
}

pub struct Pattern<'a> {
    /// A list of patterns
    pub patterns: Vec<PatternElement<'a>>,
    /// Is widcard
    pub is_wildcard: bool,
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

    /// ----------------- Language Constructs -----------------

    /// Attributeset
    /// parsed by [crate::parser::rec_set]
    AttrSet {
        /// A set of attributes
        attrs: HashMap<&'a str, (Ast<'a>)>,
        is_recursive: bool,
    },

    /// Let expression
    /// parsed by [crate::parser::let_binding]
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
    /// parsed by [crate::parser::lambda]
    Lambda {
        arguments: Vec<Pattern<'a>>,
        body: Box<Ast<'a>>,
        arg_binding: Option<&'a str>,
    },

    /// Conditional
    /// if-expr = if expr then expr else expr
    /// parsed by [crate::parser::conditional]
    Conditional {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        expr1: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is false
        expr2: Box<Ast<'a>>,
    },

    /// An assert statement.
    /// assert-expr = assert expr [ ; expr ]
    /// parsed by [crate::parser::assert]
    Assertion {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        then: Box<Ast<'a>>,
    },

    /// A with-statement.
    With {
        /// The set to evaluate
        set: Box<Ast<'a>>,
        /// The expression to evaluate
        body: Box<Ast<'a>>,
    },

    /// ----------------- Literals -----------------
    Comment(&'a str),
    DocComment(&'a str),
    LineComment(&'a str),
    Identifier(&'a str),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    NixString(&'a str),
    NixPath(&'a str),
    Null,
}

impl<'a> Ast<'a> {
    pub fn to_string(&self) -> &'a str {
        match &self {
            Self::Identifier(s) => s,
            _ => "",
        }
    }
}
