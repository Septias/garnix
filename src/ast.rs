//! Abstract syntax tree for the nix language.
#![allow(unused)]

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/// Part of a [Pattern].
pub enum PatternElement<'a> {
    /// Pattern of the form `ident`
    Identifier(&'a str),
    /// Pattern of the form `ident ? <default>`
    DefaultIdentifier(&'a str, Ast<'a>),
}

/// A pattern.
pub struct Pattern<'a> {
    /// A list of patterns
    pub patterns: Vec<PatternElement<'a>>,
    /// Is widcard
    pub is_wildcard: bool,
}

/// Binary operators of the Nix language ordered by precedence.
pub enum BinOp {

    /// Precedence: 1
    AttributeFallback,

    /// Precedence: 1
    AttributeSelection,

    /// Precendence: 2
    Application,

    /// Precendence: 4
    HasAttribute,

    /// Right associativity
    /// Precendence: 5
    ListConcat,

    /// Precedence: 6
    Mul,
    Div,

    /// Precedence: 7
    Add,
    Sub,

    /// String & Path operators
    /// Precedence: 7
    ConcatString,
    ConcatPath,
    ConcatStringPath,
    ConcatPathString,

    /// Set operators
    /// Precedence: 9
    Update,

    /// Comparison operators
    /// Precedence: 10
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    /// Logical operators
    /// Precedence: 11
    Equal,
    NotEqual,

    /// Precedence: 12
    And,

    /// Precedence: 13
    Or,

    /// Precedence: 14
    Implication,
}

/// Ast for the the nix language
#[repr(u8)]
pub enum Ast<'a> {
    /// ----------------- Operators -----------------
    /// No Associativity
    /// Precendence: 3
    Negation(Box<Ast<'a>>),

    /// Associativity: Right
    /// Precendence: 8
    LogicalNegation(Box<Ast<'a>>),

    /// Binary Operators
    /// Precendence: 2, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14
    BinOp {
        op: BinOp,
        lhs: Box<Ast<'a>>,
        rhs: Box<Ast<'a>>,
    },

    /// ----------------- Language Constructs -----------------

    /// Attribute set
    /// parsed by [crate::parser::set]
    AttrSet {
        /// A set of attributes
        attrs: HashMap<&'a str, (Ast<'a>)>,
        is_recursive: bool,
    },

    /// Let expression
    /// parsed by [crate::parser::let_binding]
    LetBinding {
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
    /// parsed by [crate::parser::assert]
    Assertion {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        then: Box<Ast<'a>>,
    },

    /// A with-statement.
    /// parsed by [crate::parser::with]
    With {
        /// The set-identifier to add
        set: Box<Ast<'a>>,
        /// The expression to evaluate
        body: Box<Ast<'a>>,
    },

    /// ----------------- Literals -----------------
    Identifier(&'a str),
    
    /// Primitives
    NixString(&'a str),
    NixPath(&'a str),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Null,

    /// Comments
    Comment(&'a str),
    DocComment(&'a str),
    LineComment(&'a str),
}

impl<'a> Ast<'a> {
    pub fn to_string(&self) -> &'a str {
        match &self {
            Self::Identifier(s) => s,
            _ => "",
        }
    }
}
