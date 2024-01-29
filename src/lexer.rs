#![allow(unused)]
//! Lexer for the Nix language.

use logos::Logos;
use nom::InputTake;
use std::usize;

/// All tokens from the Nix language.
#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    // Let Bindings
    #[token("let")]
    Let,

    #[token("in")]
    In,

    // Comments
    #[token("/*")]
    CommentStart,

    #[token("*/")]
    CommentEnd,

    #[token("/**")]
    DocCommentStart,

    // Sets
    #[token("...")]
    Dots,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(".")]
    Dot,

    #[token(",")]
    Comma,

    #[token("//")]
    Update,

    #[token("inherit")]
    Inherit,

    // Lambdas
    #[token(":")]
    DoubleColon,

    // Arith Operators
    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    // Comparisons
    #[token("<")]
    LessThan,

    #[token(">")]
    GreaterThan,

    #[token("<=")]
    LessThanEqual,

    #[token(">=")]
    GreaterThanEqual,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    // Boolean Operators
    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("!")]
    Not,

    // Lists
    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("++")]
    ListConcat,

    // Paths
    #[regex(r#"./|~/|/"#)]
    Path,

    // Patterns
    #[token("@")]
    At,

    #[token("?")]
    Default,

    // Conditionals
    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    // Booleans
    #[token("true")]
    True,

    #[token("false")]
    False,

    // Strings
    #[token("''")]
    MultiString,

    #[token("\"")]
    SingleString,

    // Literals
    #[token("null")]
    Null,

    #[token("rec")]
    Rec,

    #[token("import")]
    Import,

    #[token("assert")]
    Assert,

    #[token("with")]
    With,

    #[regex(r"-?[0-9]+")]
    Number,

    // Misc
    #[regex("[a-zA-Z]+")]
    Text,

    #[token(";")]
    Semi,
}
#[derive(Copy, Clone)]
pub struct NixTokens<'a>(pub &'a [(Token, &'a str)]);

/// Interop between [NixTokens] and nom.
mod nom_interop {
    use std::{
        iter::{Cloned, Enumerate},
        ops::{Index, Range, RangeFrom},
        slice::Iter,
    };

    use super::{NixTokens, Token};
    use nom::{
        FindToken, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed, Slice,
        UnspecializedInput,
    };

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

    impl<'a> Slice<RangeFrom<usize>> for NixTokens<'a> {
        fn slice(&self, range: RangeFrom<usize>) -> Self {
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
            self.0.iter().position(|b| predicate(b.clone()))
        }

        fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
            if self.0.len() >= count {
                Ok(count)
            } else {
                Err(Needed::new(count - self.0.len()))
            }
        }
    }

    impl<'a> FindToken<(Token, &'a str)> for NixTokens<'a> {
        fn find_token(&self, token: <NixTokens<'a> as InputIter>::Item) -> bool {
            todo!()
        }
    }

    impl<'a> UnspecializedInput for NixTokens<'a> {}
}
