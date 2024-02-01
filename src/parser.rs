use logos::Logos;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::char,
    combinator::{cut, map, opt, success},
    error::{ParseError, VerboseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple, Tuple},
    IResult, InputIter, InputLength, InputTake, Parser, Slice,
};
use std::{collections::HashMap, fs::read_to_string, ops::RangeFrom, path::Path};

use crate::{
    ast::{
        Ast::{self, *},
        Pattern, PatternElement,
    },
    lexer::{
        nom_interop::token,
        NixTokens,
        Token::{
            self, At, Comma, Default, Dots, DoubleColon, Else, Equal, If, In, Inherit, LBrace, Let,
            Minus, RBrace, Rec, Semi, Text, Then,
        },
    },
};

pub type PResult<'a, R> = IResult<NixTokens<'a>, R>;

/// Parse a single identifier.
fn ident<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    token(Text)
        .map(|(_, name)| Ast::Identifier(name))
        .parse(input)
}

/// Parse an identifier with a default value.
fn ident_default_pattern<'a>(input: NixTokens<'a>) -> PResult<'a, PatternElement<'a>> {
    tuple((ident, token(Default), cut(expr)))
        .map(|(ident, _, expr)| PatternElement::DefaultIdentifier(ident.as_str(), expr))
        .parse(input)
}

/// Parse a set pattern.
fn set_pattern<'a>(input: NixTokens<'a>) -> PResult<'a, Pattern<'a>> {
    let elements = separated_list1(
        token(Comma),
        alt((
            ident.map(|ast| PatternElement::Identifier(ast.as_str())),
            ident_default_pattern,
        )),
    );
    let has_dots = opt(token(Dots)).map(|a| a.is_some());
    tuple((token(LBrace), elements, has_dots, token(RBrace)))
        .map(|(_, elems, has_dots, _)| Pattern {
            patterns: elems,
            is_wildcard: has_dots,
        })
        .parse(input)
}

/// Parse a pattern.
/// pattern = identifier | set-pattern
pub(crate) fn pattern<'a>(input: NixTokens<'a>) -> PResult<'a, Pattern<'a>> {
    alt((
        ident.map(|ast| Pattern {
            patterns: vec![PatternElement::Identifier(ast.as_str())],
            is_wildcard: false,
        }),
        set_pattern,
    ))(input)
}

/// Parse a single statement.
/// ident = expr;
pub(crate) fn statement<'a>(input: NixTokens<'a>) -> PResult<'a, (&'a str, Ast<'a>)> {
    pair(ident.map(|ast| ast.as_str()), preceded(token(Equal), expr)).parse(input)
}

/// Parse a set definition.
pub(crate) fn set<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    let (input, is_recursive) = opt(token(Rec)).map(|a| a.is_some()).parse(input)?;
    delimited(
        token(LBrace),
        many0(terminated(statement, token(Semi))).map(move |statements| AttrSet {
            attrs: statements.into_iter().collect(),
            is_recursive,
        }),
        token(RBrace),
    )(input)
}

/// Parse a lambda function.
/// lambda = ?
pub(crate) fn lambda<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    let patterns = many1(terminated(pattern, token(DoubleColon)));
    pair(patterns, expr)
        .map(|(patterns, body)| Lambda {
            arguments: patterns,
            body: Box::new(body),
            arg_binding: None,
        })
        .parse(input)
}

/// Parse a conditional.
/// conditional = if expr then expr else expr
pub(crate) fn conditional<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    tuple((token(If), expr, token(Then), expr, token(Else), expr))
        .map(|(_, condition, _, expr1, _, expr2)| Conditional {
            condition: Box::new(condition),
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        })
        .parse(input)
}

/// Parse an assertion.
/// assert = assert expr;
pub(crate) fn assert<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    tuple((token(Token::Assert), expr, token(Token::Semi)))
        .map(|(_, condition, _)| Assertion {
            condition: Box::new(condition),
            then: Box::new(Null),
        })
        .parse(input)
}

fn inherit<'a>(input: NixTokens<'a>) -> PResult<'a, Vec<Ast<'a>>> {
    delimited(token(Inherit), many0(ident), token(Semi))(input)
}

/// Parse a let binding.
/// let-expr = let [ identifier = expr ; with ;]... in expr
pub(crate) fn let_binding<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    pair(
        token(Let),
        cut(pair(
            many0(alt((
                statement.map(|(name, ast)| vec![(name, ast)]),
                inherit.map(|items| items.into_iter().map(|ast| (ast.as_str(), ast)).collect()),
            ))),
            preceded(token(In), set),
        )),
    )
    .map(|(_, (bindings, body))| LetBinding {
        bindings: bindings.into_iter().flatten().collect(),
        body: Box::new(body),
        inherit: None,
    })
    .parse(input)
}

/// Parse a with-statement.
/// with-expr = with ident ; expr
pub(crate) fn with<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    preceded(
        token(Token::With),
        cut(pair(terminated(ident, token(Semi)), expr)),
    )
    .map(|(set, body)| With {
        set: Box::new(set),
        body: Box::new(body),
    })
    .parse(input)
}

/// Parse a literal.
fn literal<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    alt((
        token(Token::Integer(12)).map(|(token, _)| Integer(token.as_i32().unwrap())),
        token(Token::Float(12.0)).map(|(token, _)| Float(token.as_f32().unwrap())),
        token(Token::Boolean(true)).map(|(token, _)| Boolean(token.as_bool().unwrap())),
        token(Token::Null).map(|_| Null),
        token(Token::Comment).map(|(_, comment)| Comment(comment)),
        token(Token::DocComment).map(|(_, comment)| DocComment(comment)),
        token(Token::LineComment).map(|(_, comment)| LineComment(comment)),
        token(Token::SingleString).map(|(_, string)| NixString(string)),
        // TODO: concatenate string
        token(Token::MultiString).map(|(_, string)| NixString(string)),
        token(Token::Path).map(|(_, path)| NixPath(path)),
    ))(input)
}

/// Parse an expression.
pub fn expr<'a, 'b>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    todo!()
}

pub fn atom<'a, 'b>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    alt((let_binding, conditional, set, literal, with))(input)
}

fn prett_parsing<'a>(mut input: NixTokens<'a>, min_bp: u8) -> PResult<'a, Ast<'a>> {
    let (input, lhs) = match input.peek().unwrap().0 {
        // Anything that resembles an atom
        Token::Path
        | If
        | Let
        | Rec
        | Token::LBrace
        | Token::With
        | Token::Boolean(_)
        | Token::MultiString
        | Token::SingleString
        | Token::Null
        | Token::Integer(_)
        | Token::Float(_)
        | Text => atom(input)?,

        // Skip these
        Token::Comment | Token::DocComment | Token::LineComment => {
            unimplemented!("Comments are not yet implemented")
        }

        Token::LParen => {
            let lhs = prett_parsing(input, 0)?;
            assert_eq!(input.peek().unwrap().0, Token::RParen);
            lhs
        }

        Token::Dot
        | Token::Update
        | Token::Mul
        | Token::Div
        | Token::Add
        | Token::LessThan
        | Token::GreaterThan
        | Token::LessThanEqual
        | Token::GreaterThanEqual
        | Equal
        | Token::NotEqual
        | Token::And
        | Token::Or
        | Token::Not
        | Token::ListConcat
        | Minus => {}

        // Error
        _ => panic!("Unexpected token"),
    };

    Ok((input, expr))
}
