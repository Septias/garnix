#![feature(box_patterns)]
//! A library for parsing Nix code.
use std::{fs::read_to_string, path::Path};

use ast::Ast;
use lexer::{NixTokens, Token};
use logos::{Logos, Span};

pub mod ast;
pub mod infer;
pub mod lexer;
pub mod parser;

pub use infer::hm::infer;
use parser::PResult;

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
/// Result of parsing a String containing nix code.
pub struct ParseResult {
    pub ast: Ast,
    pub source: String,
}

/// Parse a file containing Nix code.
pub fn parse_file(path: &Path) -> anyhow::Result<ParseResult> {
    let source = read_to_string(path).expect("Failed to read file");
    parse(source)
}

pub fn lex(source: &str) -> Vec<(Token, logos::Span)> {
    let lex = Token::lexer(source);
    lex.spanned()
        .map(|(token, span)| {
            (
                token.unwrap_or_else(|_| panic!("Unknown token: {}", &source[span.clone()])),
                span,
            )
        })
        .collect()
}

pub fn map_err<'a, T>(
    err: PResult<'a, T>,
    source: &str,
) -> Result<(NixTokens<'a>, T), nom::Err<std::string::String>> {
    err.map_err(|err| {
        let err = err.map(|e| {
            e.errors
                .into_iter()
                .map(|(tokens, kind)| {
                    let tokens = tokens.0;
                    let span = if tokens.is_empty() {
                        Span { start: 0, end: 0 }
                    } else {
                        Span {
                            start: tokens[0].1.start,
                            end: tokens.last().unwrap().1.end,
                        }
                    };
                    format!("{} <- {:?}", &source[span], kind)
                })
                .collect::<Vec<String>>()
                .join("\n\n -------------------------------------------\n")
        });
        err
    })
}

/// Parse a string containing Nix code.
pub fn parse(source: String) -> anyhow::Result<ParseResult> {
    let tokens: Vec<(Token, std::ops::Range<usize>)> = lex(&source);
    let (_, ast) = map_err(parser::expr(NixTokens(&tokens)), &source)?;
    Ok(ParseResult { ast, source })
}
