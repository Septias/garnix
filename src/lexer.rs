#![allow(unused)]
//! Lexer for the Nix language.

use logos::Logos;
use std::usize;

#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
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
#[derive(Clone)]
pub struct NixTokens<'a>(Vec<(Token, &'a str)>);

mod nom_interop {
    use std::{
        iter::{Cloned, Enumerate}, ops::Index, slice::Iter
    };

    use super::{NixTokens, Token};
    use nom::{InputIter, InputLength, InputTake, Slice, UnspecializedInput};
    impl<'a> From<&[(Token, &'a str)]> for NixTokens<'a> {
        fn from(value: &[(Token, &'a str)]) -> Self {
            todo!()
        }
    }

    impl<'a> Index<usize> for NixTokens<'a> {
        type Output = (Token, &'a str);

        fn index(&self, index: usize) -> &Self::Output {
            &self.0[index]
        }
    }

    impl<'a> InputTake for &NixTokens<'a> {
        fn take(&self, count: usize) -> Self {
            //self.0[..count].to_vec()
            todo!()
        }

        fn take_split(&self, count: usize) -> (Self, Self) {
            todo!()
        }
    }

    impl<'a, R> Slice<R> for &NixTokens<'a> {
        fn slice(&self, range: R) -> Self {
            todo!()
        }
    }

    impl<'a> InputLength for &NixTokens<'a> {
        fn input_len(&self) -> usize {
            self.0.len()
        }
    }

    impl<'a> InputIter for &NixTokens<'a> {
        type Item = u8;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Cloned<Iter<'a, u8>>;

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

    impl<'a> UnspecializedInput for &NixTokens<'a> {}
}

pub fn lex(source: &str) -> NixTokens {
    let mut lex = Token::lexer(source);
    let tokens = lex
        .spanned()
        .map(|(token, span)| (token.unwrap(), &source[span]))
        .collect::<Vec<_>>();
    NixTokens(tokens)
}
