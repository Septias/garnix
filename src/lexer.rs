use std::{fs::read_to_string, path::Path};

use log::warn;
use logos::Logos;
use std::ops::Range;

use nom::{
    bytes::complete::is_not, character::complete::char, multi::separated_list0,
    sequence::delimited, IResult,
};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("let")]
    Let,

    #[token("in")]
    In,

    #[token("/*")]
    CommentStart,

    #[token("...")]
    Dots,

    #[token(":")]
    DoubleColon,

    #[token("*/")]
    CommentEnd,

    #[token("/**")]
    DocCommentStart,

    #[regex("[a-zA-Z]+")]
    Text,
}

fn parse_file(path: &Path) {
    let source = read_to_string(path).expect("Failed to read file");
    let tree = parse(&source);
}

fn parse(source: &str) {
    lex(source);
}

fn lex(source: &str) -> NixTokens {
    let mut lex = Token::lexer(source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token, &source[span]))
        .collect::<Vec<_>>();
    NixTokens(tokens)
}

fn name_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(char(','), is_not("}"))(input)
}

fn parens(input: &str) -> IResult<&str, &str> {
    delimited(char('{'), name_list, char('}'))(input)
}

struct NixTokens(Vec<(Token, Range<usize>)>);
