#![allow(unused)]
use std::{fs::read_to_string, path::Path};

use ast::Ast;
use lexer::{NixTokens, Token};
use logos::Logos;

mod ast;
mod error;
mod infer;
mod lexer;
mod parser;

#[cfg(test)]
mod test;

/// Parse a file containing nix-code into [Ast].
pub fn parse_file(path: &Path) -> Ast {
    let source = read_to_string(path).expect("Failed to read file");
    let tree = parse(&source);
    todo!()
}

/// Parse a string containing nix-code into [Ast].
pub fn parse(source: &str) -> Ast {
    let lex = Token::lexer(source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token.unwrap(), &source[span]))
        .collect::<Vec<_>>();

    let (_, ast) = parser::expr(NixTokens(&tokens)).unwrap();
    todo!()
}
