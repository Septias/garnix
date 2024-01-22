#![allow(unused)]

use std::{fs::read_to_string, path::Path};

use nom::{
    bytes::complete::{is_not, tag}, character::complete::char, multi::separated_list0,
    sequence::delimited, IResult,
};

use crate::lexer::{lex, NixTokens};

/// Ast for the the nix language
enum Ast<'a> {
    Lambda,
    Application,
    Identifier(&'a str),
    SetLambda(Vec<Ast<'a>>, Box<Ast<'a>>),
    Text(&'a str),
    BinOp,
}

fn identifier(input: &str) -> IResult<&str, Ast> {
    let (input, name) = is_not(r#" \t\n\f"#)(input)?;
    Ok((input, Ast::Identifier(name)))
}

fn name_list(input: &str) -> IResult<&str, Vec<Ast>> {
    separated_list0(char(','), identifier)(input)
}

fn test<'src, 'slice>(input: &'slice NixTokens<'src>) -> IResult<&'slice NixTokens<'src>, &'slice NixTokens<'src>> {
   tag("hi")(input)
}


fn set_lambda(input: &str) -> IResult<&str, Ast> {
    let (input, names) = name_list(input)?;
    let (input, _) = char(':')(input)?;
    let (input, body) = delimited(char('{'), is_not("}"), char('}'))(input)?;
    Ok((input, Ast::SetLambda(names, Box::new(Ast::Text(body)))))
}


/// Parse a file containing nix-code.
pub fn parse_file(path: &Path) {
    let source = read_to_string(path).expect("Failed to read file");
    let tree = parse(&source);
}

/// Parse a string containing nix-code.
pub fn parse(source: &str) {
    let tokens = lex(source);

}
