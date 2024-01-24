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

use crate::lexer::{NixTokens, Token::{self, *}};

/// Ast for the the nix language
#[repr(u8)]
enum Ast<'a> {
    Lambda,
    Application,
    Identifier(&'a str),
    SetLambda(Vec<Ast<'a>>, Box<Ast<'a>>),
    Text(&'a str),
    BinOp,
}

/* fn token(input: &NixTokens) -> IResult<&NixTokens, Ast> {
    todo!()

pub fn token<NixTokens, Error: ParseError<NixTokens>>(c: char) -> impl Fn(I) -> IResult<NixTokens, char, Error>
where
  I: Slice<RangeFrom<usize>> + InputIter,
  <I as InputIter>::Item: AsChar,
{
  move |i: I| match (i).iter_elements().next().map(|t| {
    let b = t.as_char() == c;
    (&c, b)
  }) {
    Some((c, true)) => Ok((i.slice(c.len()..), c.as_char())),
    _ => Err(Err::Error(Error::from_char(i, c))),
  }
}
} */

pub fn token<'a, Error: ParseError<NixTokens<'a>>>(
    c: Token,
) -> impl Fn(NixTokens<'a>) -> IResult<NixTokens<'a>, (Token, &'a str), Error> {
    move |i: NixTokens<'_>| match (i).iter_elements().next().map(|t| {
        let b = t.0 == c;
        (t, b)
    }) {
        Some((c, true)) => Ok((i.slice(1..), c.clone())),
        _ => Err(nom::Err::Error(Error::from_error_kind(
            i.clone(),
            nom::error::ErrorKind::Char,
        ))),
    }
}

fn identifier<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Ast<'src>> {
    let (input, name) = is_not(NixTokens(&[(Token::WS, "")]))(input)?;
    assert!(name.input_len() != 0, "Expected identifier");
    Ok((input, Ast::Identifier(name[0].1)))
}

fn name_list<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Vec<Ast<'src>>> {
    separated_list0(token(Comma), identifier)(input)
}

fn parse_expr<'src, 'slice>(
    input: &'slice NixTokens<'src>,
) -> IResult<&'slice NixTokens<'src>, &'slice NixTokens<'src>> {
    todo!()
}

fn set_lambda<'src, 'slice>(
    input: NixTokens<'src>,
) -> IResult<NixTokens<'src>, &'slice NixTokens<'src>> {
    let (input, names) = name_list(input)?;
    let (input, _) = token(DoubleColon)(input)?;
    let (input, body) = delimited(token(LBrace), is_not(NixTokens(&[(Token::RBrace, "")])), token(RBrace))(input)?;
    //Ok((input, Ast::SetLambda(names, Box::new(Ast::Text(body)))))
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
