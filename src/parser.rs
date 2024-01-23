#![allow(unused)]

use std::{fs::read_to_string, path::Path};

use logos::Logos;
use nom::{
    bytes::complete::{is_not, tag},
    character::complete::char,
    multi::separated_list0,
    sequence::delimited,
    IResult, InputLength, InputTake,
};

use crate::lexer::{NixTokens, Token};

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
} */

fn identifier<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Ast<'src>> {
    let (input, name) = is_not(r#" \t\n\f"#)(input)?;
    assert!(name.input_len() == 0, "Expected identifier");
    Ok((input, Ast::Identifier(name[0].1)))
}

fn name_list<'src, 'slice>(input: NixTokens<'src>) -> IResult<NixTokens<'src>, Vec<Ast<'src>>> {
    separated_list0(char(','), identifier)(input)
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
    let (input, _) = char(':')(input)?;
    let (input, body) = delimited(char('{'), is_not("}"), char('}'))(input)?;
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
