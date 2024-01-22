//! Lexer for the Nix language.

use logos::Logos;
use nom::{Compare, InputIter, InputLength, InputTake};

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

pub struct NixTokens<'a>(Vec<(Token, &'a str)>);

impl<'a> InputTake for &NixTokens<'a> {
    fn take(&self, count: usize) -> Self {
        todo!()
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        todo!()
    }
}

impl<'a> InputLength for &NixTokens<'a> {
    fn input_len(&self) -> usize {
        todo!()
    }
}

impl<'a> InputIter for &NixTokens<'a> {
    type Item = u8;

    type Iter;

    type IterElem;

    fn iter_indices(&self) -> Self::Iter {
        todo!()
    }

    fn iter_elements(&self) -> Self::IterElem {
        todo!()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        todo!()
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        todo!()
    }
}

impl<'a, T> Compare<T> for &NixTokens<'a> {
    fn compare(&self, t: T) -> nom::CompareResult {
        todo!()
    }

    fn compare_no_case(&self, t: T) -> nom::CompareResult {
        todo!()
    }
}

pub fn lex(source: &str) -> NixTokens {
    let mut lex = Token::lexer(source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token.unwrap(), &source[span]))
        .collect::<Vec<_>>();
    NixTokens(tokens)
}
