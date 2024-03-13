//! Lexer for the Nix language.

use logos::{Logos, Span};

use std::usize;

/// All tokens from the Nix language.
#[derive(Clone, Copy, Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    // Let Bindings
    #[token("let")]
    Let,

    #[token("in")]
    In,

    // Comments
    #[regex(r"/\*[a-zA-Z ]*\*/")]
    Comment,

    #[regex(r"/\*\*[a-zA-Z ]*\*/")]
    DocComment,

    #[regex(r"#[a-zA-Z ]*\n")]
    LineComment,

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
    Star,

    #[token("/")]
    Slash,

    #[token("+")]
    Plus,

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

    #[token("->")]
    Implication,

    // Lists
    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("++")]
    ListConcat,

    // Paths
    #[regex("(./|~/|/)([a-zA-Z.]+/?)+")]
    Path,

    #[regex("<[a-zA-Z.][a-zA-Z./]+>")]
    SearchPath,

    // Patterns
    #[token("@")]
    At,

    #[token("?")]
    Question,

    // Conditionals
    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    // Booleans
    #[regex("true|false", |lex| lex.slice() == "true")]
    Boolean(bool),

    // Strings
    #[regex(r#"''[a-zA-Z\n ]*''"#)]
    MultiString,

    #[regex(r#""[^"]*""#)]
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

    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i32),

    #[regex(r"[0-9]*\.[0-9]+", |lex| lex.slice().parse().ok())]
    Float(f32),

    // Misc
    #[regex("[a-zA-Z_]+")]
    Text,

    #[token(";")]
    Semi,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("-")]
    Minus,

    #[token("or")]
    AttributeFallback,

    #[token("=")]
    Assignment,
}

impl Token {
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            Self::Integer(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_f32(&self) -> Option<f32> {
        match self {
            Self::Float(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Boolean(b) => Some(*b),
            _ => None,
        }
    }
}

type SpannedToken = (Token, Span);

#[derive(Debug, Copy, Clone)]
pub struct NixTokens<'a>(pub &'a [SpannedToken]);

impl<'a> NixTokens<'a> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<SpannedToken> {
        if self.0.is_empty() {
            None
        } else {
            let (token, ref text) = self.0[0];
            self.0 = &self.0[1..];
            Some((token, text.clone()))
        }
    }

    pub fn peek(&self) -> Option<&SpannedToken> {
        self.0.first()
    }
}

impl<'a> From<&'a [SpannedToken]> for NixTokens<'a> {
    fn from(value: &'a [SpannedToken]) -> Self {
        Self(value)
    }
}

/// Interop between [NixTokens] and nom.
pub mod nom_interop {
    use std::{
        iter::{Cloned, Enumerate},
        mem::discriminant,
        ops::{Index, Range, RangeFrom},
        slice::Iter,
    };

    use super::{NixTokens, SpannedToken, Token};
    use nom::{
        error::ParseError, FindToken, IResult, InputIter, InputLength, InputTake, Needed, Slice,
        UnspecializedInput,
    };

    impl<'a> Index<usize> for NixTokens<'a> {
        type Output = SpannedToken;

        fn index(&self, index: usize) -> &Self::Output {
            &self.0[index]
        }
    }

    impl<'a> Index<Range<usize>> for NixTokens<'a> {
        type Output = [(Token, Range<usize>)];

        fn index(&self, range: Range<usize>) -> &Self::Output {
            &self.0[range]
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
        type Item = SpannedToken;
        type Iter = Enumerate<Self::IterElem>;
        type IterElem = Cloned<Iter<'a, SpannedToken>>;

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

    impl<'a> FindToken<SpannedToken> for NixTokens<'a> {
        fn find_token(&self, token: SpannedToken) -> bool {
            let token_disc = discriminant(&token.0);
            self.0.iter().fold(false, |acc, (token, _)| {
                discriminant(token) == token_disc || acc
            })
        }
    }

    impl<'a> UnspecializedInput for NixTokens<'a> {}

    pub fn token<'a, Error: ParseError<NixTokens<'a>>>(
        c: Token,
    ) -> impl Fn(NixTokens<'a>) -> IResult<NixTokens<'a>, SpannedToken, Error> {
        #[allow(clippy::blocks_in_conditions)]
        move |i: NixTokens<'_>| match (i).iter_elements().next().map(|t| {
            let b = discriminant(&t.0) == discriminant(&c);
            (t, b)
        }) {
            Some((c, true)) => Ok((i.slice(1..), c.clone())),
            _ => Err(nom::Err::Error(Error::from_error_kind(
                i,
                nom::error::ErrorKind::IsNot,
            ))),
        }
    }
}
