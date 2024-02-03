use std::{fs::read_to_string, path::Path};

use ast::Ast;
use lexer::{NixTokens, Token};
use logos::Logos;

mod ast;
mod lexer;
mod parser;

/// Result of parsing a String containing nix code.
pub struct ParseResult {
    pub ast: Ast,
    pub source: String,
}

/// Parse a file containing Nix code.
pub fn parse_file(path: &Path) -> ParseResult {
    let source = read_to_string(path).expect("Failed to read file");
    parse(source)
}

/// Parse a string containing Nix code.
pub fn parse(source: String) -> ParseResult {
    let lex = Token::lexer(&source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token.unwrap(), span))
        .collect::<Vec<_>>();

    let (_, ast) = parser::expr(NixTokens(&tokens)).unwrap();

    ParseResult { ast, source }
}
