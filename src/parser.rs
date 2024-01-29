#![allow(unused)]

use std::{fs::read_to_string, ops::RangeFrom, path::Path};

use logos::Logos;
use nom::{
    bytes::complete::{is_not, tag},
    character::complete::char,
    error::ParseError,
    multi::separated_list0,
    sequence::delimited,
    IResult, InputIter, InputLength, InputTake, Slice,
};

use crate::lexer::{
    nom_interop::token,
    NixTokens,
    Token::{self, *},
};

/// Binary operators of the Nix language ordered by precedence.
enum BinOp {
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

use BinOp::*;

/// Ast for the the nix language
#[repr(u8)]
enum Ast<'a> {
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
        attrs: Vec<(&'a str, Ast<'a>)>,
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

    /// Conditional
    /// if-expr = if expr then expr else expr
    Conditional {
        /// The condition to evaluate
        condition: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is true
        then: Box<Ast<'a>>,
        /// The expression to evaluate if the condition is false
        else_: Box<Ast<'a>>,
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
}

use Ast::*;

/// Parse a single identifier.
fn identifier<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Ast<'src>> {
    let (input, (_, name)) = token(Text)(input)?;
    assert!(name.len() != 0, "Expected identifier");
    Ok((input, Ast::Identifier(name)))
}

/// Parse a list of identifiers.
fn name_list<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Vec<Ast<'src>>> {
    separated_list0(token(Comma), identifier)(input)
}

fn parse_expr<'src, 'slice>(
    input: &'slice NixTokens<'src>,
) -> IResult<&'slice NixTokens<'src>, &'slice NixTokens<'src>> {
    todo!()
}

/// Parse a set lambda.
fn set_lambda<'src, 'slice>(
    input: NixTokens<'src>,
) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    let (input, names) = name_list(input)?;
    let (input, _) = token(DoubleColon)(input)?;
    let (input, body) = delimited(
        token(LBrace),
        is_not(NixTokens(&[(Token::RBrace, "")])),
        token(RBrace),
    )(input)?;
    //Ok((input, Ast::SetLambda(names, Box::new(Ast::Text(body)))))
    todo!()
}

/// Parse a recursive set definition.
fn recursive_set<'src, 'slice>(
    input: NixTokens<'src>,
) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    todo!()
}

/// Parse a set definition
fn set<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    todo!()
}

/// Parse a pattern.
fn pattern<'src, 'slice>(
    input: NixTokens<'src>,
) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    // Identifier is a pattern
    // A set might be a pattern
    // A set pattern might contain default values
    todo!()
}

/// Parse a lambda function.
fn lambda<'src, 'slice>(
    input: NixTokens<'src>,
) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    todo!()
}

/// Parse a file containing nix-code.
pub fn parse_file(path: &Path) {
    let source = read_to_string(path).expect("Failed to read file");
    let tree = parse(&source);
}

/// Parse a string containing nix-code.
pub fn parse(source: &str) {
    let mut lex = Token::lexer(source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token.unwrap(), &source[span]))
        .collect::<Vec<_>>();
}
