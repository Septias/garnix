#![allow(unused)]
use logos::Span;
use nom::{
    branch::alt,
    combinator::{cut, opt, success},
    error::{context, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};

use crate::{
    ast::{
        Ast::{self, *},
        BinOp, Pattern, PatternElement, UnOp,
    },
    lexer::{
        nom_interop::token,
        NixTokens,
        Token::{
            self, Assignment, Comma, Dots, DoubleColon, Else, If, In, Inherit, LBrace, Let, Minus,
            Question, RBrace, Rec, Semi, Text, Then,
        },
    },
};

pub type PResult<'a, R> = IResult<NixTokens<'a>, R, VerboseError<NixTokens<'a>>>;

/// Parse a single identifier.
pub(crate) fn ident(input: NixTokens<'_>) -> PResult<'_, Ast> {
    token(Text)
        .map(|(_, name)| Ast::Identifier(name))
        .parse(input)
}

/// Parse an identifier with a default value.
pub(crate) fn ident_default_pattern(input: NixTokens<'_>) -> PResult<'_, PatternElement> {
    tuple((ident, token(Question), cut(expr)))
        .map(|(ident, _, expr)| PatternElement::DefaultIdentifier(ident.as_span(), expr))
        .parse(input)
}

/// Parse a literal.
pub(crate) fn literal(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "literal",
        alt((
            token(Token::Integer(12)).map(|(token, _)| Integer(token.as_i32().unwrap())),
            token(Token::Float(12.0)).map(|(token, _)| Float(token.as_f32().unwrap())),
            token(Token::Boolean(true)).map(|(token, _)| Boolean(token.as_bool().unwrap())),
            token(Token::Null).map(|_| Null),
            token(Token::Comment).map(|(_, comment)| Comment(comment)),
            token(Token::DocComment).map(|(_, comment)| DocComment(comment)),
            token(Token::LineComment).map(|(_, comment)| LineComment(comment)),
            token(Token::SingleString).map(|(_, string)| NixString(string)),
            // TODO: concatenate string
            token(Token::MultiString).map(|(_, string)| NixString(string)),
            token(Token::Path).map(|(_, path)| NixPath(path)),
        )),
    )(input)
}

/// Parse a set pattern.
pub(crate) fn set_pattern(input: NixTokens<'_>) -> PResult<'_, Pattern> {
    let elements = separated_list1(
        token(Comma),
        alt((
            ident.map(|ast| PatternElement::Identifier(ast.as_span())),
            ident_default_pattern,
        )),
    );
    context(
        "Pattern",
        preceded(
            token(LBrace),
            cut(terminated(
                pair(
                    elements,
                    opt(preceded(token(Comma), token(Dots))).map(|a| a.is_some()),
                )
                .map(|elem| Pattern {
                    patterns: elem.0,
                    is_wildcard: elem.1,
                })
                .or(token(Dots).map(|_| Pattern {
                    patterns: vec![],
                    is_wildcard: true,
                }))
                .or(success(Pattern {
                    patterns: vec![],
                    is_wildcard: false,
                })),
                token(RBrace),
            )),
        ),
    )(input)
}

/// Parse a pattern.
/// pattern = identifier | set-pattern
pub(crate) fn pattern(input: NixTokens<'_>) -> PResult<'_, Pattern> {
    alt((
        ident.map(|ast| Pattern {
            patterns: vec![PatternElement::Identifier(ast.as_span())],
            is_wildcard: false,
        }),
        set_pattern,
    ))(input)
}

/// Parse a single statement.
/// ident = expr;
pub(crate) fn statement(input: NixTokens<'_>) -> PResult<'_, (Span, Ast)> {
    context(
        "Statement",
        terminated(
            pair(
                ident.map(|ast| ast.as_span()),
                cut(preceded(token(Assignment), expr)),
            ),
            token(Semi),
        ),
    )
    .parse(input)
}

/// Parse a set definition.
pub(crate) fn set(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "Set",
        pair(
            opt(token(Rec)).map(|a| a.is_some()),
            delimited(token(LBrace), many0(statement), token(RBrace)),
        )
        .map(|(is_recursive, statements)| AttrSet {
            attrs: statements.into_iter().collect(),
            is_recursive,
        }),
    )(input)
}

/// Parse a lambda function.
/// lambda = ?
pub(crate) fn lambda(input: NixTokens<'_>) -> PResult<'_, Ast> {
    let patterns = many1(terminated(pattern, token(DoubleColon)));
    pair(patterns, expr)
        .map(|(patterns, body)| Lambda {
            arguments: patterns,
            body: Box::new(body),
            arg_binding: None,
        })
        .parse(input)
}

/// Parse a conditional.
/// conditional = if expr then expr else expr
pub(crate) fn conditional(input: NixTokens<'_>) -> PResult<'_, Ast> {
    tuple((token(If), expr, token(Then), expr, token(Else), expr))
        .map(|(_, condition, _, expr1, _, expr2)| Conditional {
            condition: Box::new(condition),
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
        })
        .parse(input)
}

/// Parse an assertion.
/// assert = assert expr;
pub(crate) fn assert(input: NixTokens<'_>) -> PResult<'_, Ast> {
    tuple((token(Token::Assert), expr, token(Token::Semi)))
        .map(|(_, condition, _)| Assertion {
            condition: Box::new(condition),
            then: Box::new(Null),
        })
        .parse(input)
}

pub(crate) fn inherit(input: NixTokens<'_>) -> PResult<'_, Vec<Ast>> {
    delimited(token(Inherit), many0(ident), token(Semi))(input)
}

/// Parse a let binding.
/// let-expr = let [ identifier = expr ; with ;]... in expr
pub(crate) fn let_binding(input: NixTokens<'_>) -> PResult<'_, Ast> {
    pair(
        token(Let),
        cut(pair(
            many0(alt((
                statement.map(|(name, ast)| vec![(name, ast)]),
                inherit.map(|items| items.into_iter().map(|ast| (ast.as_span(), ast)).collect()),
            ))),
            preceded(token(In), expr),
        )),
    )
    .map(|(_, (bindings, body))| LetBinding {
        bindings: bindings.into_iter().flatten().collect(),
        body: Box::new(body),
        inherit: None,
    })
    .parse(input)
}

/// Parse a with-statement.
/// with-expr = with expr;
pub(crate) fn with(input: NixTokens<'_>) -> PResult<'_, Ast> {
    preceded(token(Token::With), cut(terminated(expr, token(Semi))))(input)
}

pub(crate) fn atom<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
    alt((let_binding, conditional, set, literal))(input)
}

/// Parse an expression.
pub(crate) fn expr<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "expr",
        pair(opt(with), alt((literal, ident, set, assert, lambda))).map(|(with, expr)| {
            if let Some(with) = with {
                With {
                    set: Box::new(with),
                    body: Box::new(expr),
                }
            } else {
                expr
            }
        }),
    )(input)
}

pub(crate) fn prett_parsing(mut input: NixTokens<'_>, min_bp: u8, eof: Token) -> PResult<'_, Ast> {
    let (mut input, mut lhs) = match input.peek().unwrap().0 {
        // Anything that resembles an atom
        Token::Path
        | If
        | Let
        | Rec
        | Token::LBrace
        | Token::Boolean(_)
        | Token::MultiString
        | Token::SingleString
        | Token::Null
        | Token::Integer(_)
        | Token::Float(_)
        | Text => atom(input)?,

        // Skip these
        Token::Comment | Token::DocComment | Token::LineComment => {
            unimplemented!("Comments are not yet implemented")
        }

        Token::LParen => {
            input.next();
            println!("before: {input:?}");
            let (mut input, lhs) = prett_parsing(input, 0, eof)?;
            println!("{input:?}");
            assert_eq!(input.next().unwrap().0, Token::RParen);
            (input, lhs)
        }

        Minus => {
            input.next();
            let right_bp = 23;
            let (input, rhs) = prett_parsing(input, right_bp, eof)?;
            (
                input,
                Ast::UnaryOp {
                    op: UnOp::Negation,
                    rhs: Box::new(rhs),
                },
            )
        }

        Token::Not => {
            input.next();
            let right_bp = 13;
            let (input, rhs) = prett_parsing(input, right_bp, eof)?;
            (
                input,
                Ast::UnaryOp {
                    op: UnOp::LogicalNegation,
                    rhs: Box::new(rhs),
                },
            )
        }

        t => panic!("Unexpected token: {t:?}"),
    };

    loop {
        if input.peek().unwrap().0 == eof {
            break;
        }

        let op = BinOp::from_token(input.peek().unwrap().0);

        if let Some(op) = op {
            let (left_bp, right_bp) = op.get_precedence();
            if left_bp < min_bp {
                break;
            }

            input.next();

            let (_input, rhs) = prett_parsing(input, right_bp, eof)?;
            input = _input;
            lhs = Ast::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };

            continue;
        };
        break;
    }

    Ok((input, lhs))
}
