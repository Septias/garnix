#![feature(box_patterns)]
//! A library for parsing Nix code.
use std::{fs::read_to_string, path::Path};

use ast::Ast;
use lexer::{NixTokens, Token};
use logos::Logos;

pub mod ast;
pub mod lexer;
pub mod parser;
pub use logos::Span;

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

/// Lex a string containing Nix code.
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

/// Create a String Error from a nom error.
pub fn map_err<'a, T>(
    err: PResult<'a, T>,
    source: &str,
) -> Result<(NixTokens<'a>, T), nom::Err<std::string::String>> {
    err.map_err(|err| {
        err.map(|errors| {
            errors
                .errors
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
                    let mut lines = source[span]
                        .split('\n')
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>();
                    if let Some(a) = lines.get_mut(0) {
                        a.push_str(&format!(" <- {:?}", kind))
                    }

                    lines.join("\n")
                })
                .collect::<Vec<String>>()
                .join("\n\n -------------------------------------------\n")
        })
    })
}

/// Parse a string containing Nix code.
pub fn parse(source: String) -> anyhow::Result<ParseResult> {
    let tokens: Vec<(Token, std::ops::Range<usize>)> = lex(&source);
    let (_, ast) = map_err(parser::expr(NixTokens(&tokens)), &source)?;
    Ok(ParseResult { ast, source })
}
