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
pub struct NixTokens<'a>(&'a [(Token, &'a str)]);

mod nom_interop {
    use std::{
        iter::{Cloned, Enumerate},
        ops::{Index, Range},
        slice::Iter,
    };

    use super::{NixTokens, Token};
    use nom::{InputIter, InputLength, InputTake, Needed, Slice, UnspecializedInput};

    impl<'a> From<&'a [(Token, &'a str)]> for NixTokens<'a> {
        fn from(value: &'a [(Token, &'a str)]) -> Self {
            Self(value)
        }
    }

    impl<'a> Index<usize> for NixTokens<'a> {
        type Output = (Token, &'a str);

        fn index(&self, index: usize) -> &Self::Output {
            &self.0[index]
        }
    }

    impl<'a> InputTake for NixTokens<'a> {
        fn take(&self, count: usize) -> Self {
            Self(&self.0[..count])
        }

        fn take_split(&self, count: usize) -> (Self, Self) {
            let (l, r) = self.0.split_at(count);
            (Self(l), Self(r))
        }
    }

    impl<'a> Slice<Range<usize>> for NixTokens<'a> {
        fn slice(&self, range: Range<usize>) -> Self {
            Self(&self.0[range])
        }
    }

    impl<'a> InputLength for NixTokens<'a> {
        fn input_len(&self) -> usize {
            self.0.len()
        }
    }

    impl<'a> InputIter for NixTokens<'a> {
        type Item = (Token, &'a str);
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Cloned<Iter<'a, (Token, &'a str)>>;

        fn iter_indices(&self) -> Self::Iter {
            self.iter_elements().enumerate()
        }

        fn iter_elements(&self) -> Self::IterElem {
            self.0.iter().cloned()
        }

        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            self.0.iter().position(|b| predicate(*b))
        }

        fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
            if self.0.len() >= count {
                Ok(count)
            } else {
                Err(Needed::new(count - self.0.len()))
            }
        }
    }

    impl<'a> UnspecializedInput for NixTokens<'a> {}
}
