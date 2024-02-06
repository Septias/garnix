//! A library for parsing Nix code.
use std::{fs::read_to_string, path::Path};

use ast::Ast;
use lexer::{NixTokens, Token};
use logos::Logos;

pub mod ast;
pub mod lexer;
pub mod parser;

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
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

pub(crate) fn lex(source: &str) -> Vec<(Token, logos::Span)> {
    let lex = Token::lexer(source);
    lex.spanned()
        .map(|(token, span)| {
            (
                token.expect(&format!("Unknown token: {}", &source[span.clone()])),
                span,
            )
        })
        .collect()
}

/// Parse a string containing Nix code.
pub fn parse(source: String) -> ParseResult {
    let tokens = lex(&source);
    let (_, ast) = parser::expr(NixTokens(&tokens)).unwrap();

    ParseResult { ast, source }
}
