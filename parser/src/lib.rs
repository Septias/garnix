#![feature(box_patterns)]
//! A library for parsing Nix code.
use ast::Ast;
use lexer::{NixTokens, Token};
use logos::Logos;
use nom::{
    error::{VerboseError, VerboseErrorKind},
    Err,
};
use std::{fs::read_to_string, path::Path};
use thiserror::Error;

pub mod ast;
pub mod lexer;
pub mod parser;
pub use logos::Span;

#[cfg(test)]
mod test;

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

/// An error that occured during parsing.
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Multiple errors: {errors:?}")]
    MultipleErrors {
        errors: Vec<(VerboseErrorKind, Span)>,
    },
}

/// Create a String Error from a nom error.
pub fn map_err<'a>(err: Err<VerboseError<NixTokens<'a>>>) -> ParseError {
    let errors = match err {
        nom::Err::Incomplete(_) => vec![],
        nom::Err::Error(e) | nom::Err::Failure(e) => e
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
                (kind, span)
            })
            .collect(),
    };
    ParseError::MultipleErrors { errors }
}

/// Parse a string containing Nix code.
pub fn parse(source: &str) -> Result<Ast, ParseError> {
    let tokens: Vec<(Token, std::ops::Range<usize>)> = lex(&source);
    let (_, ast) = parser::expr(NixTokens(&tokens)).map_err(|err| map_err(err))?;
    Ok(ast)
}

/// Parse a file containing Nix code.
pub fn parse_file(path: &Path) -> Result<Ast, ParseError> {
    let source = read_to_string(path).expect("Failed to read file");
    parse(&source)
}
