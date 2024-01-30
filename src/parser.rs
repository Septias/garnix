#![allow(unused)]

use std::{collections::HashMap, fs::read_to_string, ops::RangeFrom, path::Path};

use logos::Logos;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::char,
    combinator::{opt, success},
    error::{ParseError, VerboseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple, Tuple},
    IResult, InputIter, InputLength, InputTake, Parser, Slice,
};

use crate::{
    ast::Ast::{self, *},
    lexer::{
        nom_interop::token,
        NixTokens,
        Token::{
            self, At, Comma, Default, Dots, DoubleColon, Else, Equal, If, LBrace, Minus, RBrace,
            Rec, Semi, Text, Then,
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

/// Parse a single identifier with given default value.
fn identifier_w_default<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    tuple((ident, token(Default), expr))
        .map(|(ident, _, expr)| IdentifierWDefault(ident.to_string(), Box::new(expr)))
        .parse(input)
}

/// Parse a set pattern.
/// - Default values
/// - Recursive set patterns
/// TODO: no recursive set patterns
fn set_pattern<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    let element = alt((ident, identifier_w_default, set_pattern));
    let elements = separated_list1(token(Comma), element);
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
fn pattern<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    alt((ident, set_pattern))(input)
}

/// Parse the body of a lambda expression.
fn body<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    //delimited(token(LBrace), f, token(RBrace))(input)
    todo!()
}

/// Parse a set lambda.
fn set_lambda<'a>(input: NixTokens<'a>) -> PResult<Ast<'a>> {
    let (input, (binding1, pattern, binding2)) = tuple((
        opt(terminated(ident, token(At))),
        pattern,
        opt(preceded(token(At), ident)),
    ))(input)?;

    let (input, _) = token(DoubleColon)(input)?;

    let (input, body) = body(input)?;

    assert!(
        !(binding1.is_some() && binding2.is_some()),
        "double bindings are not allowed"
    );

    Ok((
        input,
        Lambda {
            pattern: Box::new(pattern),
            body: Box::new(body),
            arg_binding: binding1.or(binding2).map(|a| a.to_string()),
        },
    ))
}

/// Parse a recursive set definition.
fn rec_set<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    pair(token(Rec), non_rec_set)(input).map(|(input, (_, attrs))| {
        (
            input,
            AttrSet {
                attrs,
                is_recursive: true,
            },
        )
    })
}

/// Parse a none-recursive set definition.
fn non_rec_set<'a>(input: NixTokens<'a>) -> PResult<'a, HashMap<&str, Ast<'a>>> {
    let assigment = tuple((ident, token(Equal), expr));
    delimited(
        token(LBrace),
        many0(pair(assigment, token(Semi))),
        token(RBrace),
    )
    .map(|assignments| {
        assignments
            .into_iter()
            .map(|((ident, _, expr), _)| (ident.to_string(), expr))
            .collect()
    })
    .parse(input)
}

/// Parse a set definition.
fn set<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    let (input, is_recursive) = opt(token(Rec)).map(|a| a.is_some()).parse(input)?;
    non_rec_set
        .map(|set| AttrSet {
            attrs: set,
            is_recursive,
        })
        .parse(input)
}

/// Parse a lambda function.
fn lambda<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    let patterns = many1(pair(pattern, token(DoubleColon)).map(|(a, b)| a));
    let body = alt((
        expr,
        non_rec_set.map(|set| AttrSet {
            attrs: set,
            is_recursive: false,
        }),
    ));
    pair(patterns, body)
        .map(|(patterns, body)| Lambda {
            pattern: Box::new(Pattern {
                patterns,
                is_wildcard: false,
            }),
            body: Box::new(body),
            arg_binding: None,
        })
        .parse(input)
}

/// Parse a conditional.
fn if_else<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
    tuple((token(If), expr, token(Then), expr, token(Else), expr))
        .map(|(_, condition, _, expr1, _, expr2)| Conditional {
            condition: Box::new(condition),
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        })
        .parse(input)
}

/// Parse a literal.
fn parse_literal<'a>(input: NixTokens<'a>) -> PResult<'a, Ast<'a>> {
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
