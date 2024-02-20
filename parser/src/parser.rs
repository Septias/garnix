use logos::Span;
use nom::{
    branch::alt,
    combinator::{cut, opt, success},
    error::{context, ParseError, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, InputLength, Parser,
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

pub(crate) fn span_diff(input: &NixTokens<'_>, new_input: &NixTokens<'_>) -> Span {
    let spans = &input[0..input.input_len() - new_input.input_len()];

    if spans.is_empty() {
        Span { start: 0, end: 0 }
    } else {
        Span {
            start: spans[0].1.start,
            end: spans.last().unwrap().1.end,
        }
    }
}

/// Take the output of a parser and combine it with the span it took from the input to create a new value.
pub(crate) fn spanned<'a, T, X>(
    mut parser: impl Parser<NixTokens<'a>, T, VerboseError<NixTokens<'a>>>,
    spanned: impl Fn(Span, T) -> X,
) -> impl FnMut(NixTokens<'a>) -> PResult<'a, X> {
    move |input| {
        let (new_input, res) = parser.parse(input)?;
        let range = span_diff(&input, &new_input);
        Ok((new_input, spanned(range, res)))
    }
}

/// Parse a single identifier.
pub(crate) fn ident(input: NixTokens<'_>) -> PResult<'_, Ast> {
    token(Text)
        .map(|(_, name)| Ast::Identifier(name))
        .parse(input)
}

/// Parse an identifier with a default value.
pub(crate) fn ident_default_pattern(input: NixTokens<'_>) -> PResult<'_, PatternElement> {
    spanned(
        tuple((ident, token(Question), cut(expr))),
        |span, (identifier, _, ast)| PatternElement::DefaultIdentifier {
            identifier: identifier.as_span(),
            span,
            ast,
        },
    )(input)
}

/// Parse a literal.
pub(crate) fn literal(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "literal",
        alt((
            token(Token::Integer(12)).map(|(token, span)| Int {
                val: token.as_i32().unwrap(),
                span,
            }),
            token(Token::Float(12.0)).map(|(token, span)| Float {
                val: token.as_f32().unwrap(),
                span,
            }),
            token(Token::Boolean(true)).map(|(token, span)| Bool {
                val: token.as_bool().unwrap(),
                span,
            }),
            token(Token::Null).map(|(_, span)| Null(span)),
            token(Token::Comment).map(|(_, comment)| Comment(comment)),
            token(Token::DocComment).map(|(_, comment)| DocComment(comment)),
            token(Token::LineComment).map(|(_, comment)| LineComment(comment)),
            token(Token::SingleString).map(|(_, string)| NixString(string)),
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
        "statement",
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
        "set",
        spanned(
            pair(
                opt(token(Rec)).map(|a| a.is_some()),
                delimited(token(LBrace), many0(statement), token(RBrace)),
            ),
            |span, (is_recursive, statements)| AttrSet {
                attrs: statements.into_iter().collect(),
                is_recursive,
                span,
            },
        ),
    )(input)
}

/// Parse a lambda function.
/// lambda = ?
pub(crate) fn lambda(input: NixTokens<'_>) -> PResult<'_, Ast> {
    let patterns = many1(terminated(pattern, token(DoubleColon)));
    spanned(pair(patterns, expr), |span, (patterns, body)| Lambda {
        arguments: patterns,
        body: Box::new(body),
        arg_binding: None,
        span,
    })(input)
}

/// Parse a conditional.
/// conditional = if expr then expr else expr
pub(crate) fn conditional(input: NixTokens<'_>) -> PResult<'_, Ast> {
    spanned(
        tuple((token(If), expr, token(Then), expr, token(Else), expr)),
        |span, (_, condition, _, expr1, _, expr2)| Conditional {
            condition: Box::new(condition),
            expr1: Box::new(expr1),
            expr2: Box::new(expr2),
            span,
        },
    )(input)
}

/// Parse an assertion.
/// assert = assert expr;
pub(crate) fn assert(input: NixTokens<'_>) -> PResult<'_, Ast> {
    spanned(
        tuple((token(Token::Assert), expr, token(Token::Semi), expr)),
        |span, (_, condition, _, expr)| Assertion {
            condition: Box::new(condition),
            expr: Box::new(expr),
            span,
        },
    )(input)
}

pub(crate) fn inherit(input: NixTokens<'_>) -> PResult<'_, Vec<Ast>> {
    delimited(token(Inherit), many0(ident), token(Semi))(input)
}

/// Parse a let binding.
/// let-expr = let [ identifier = expr ; with ;]... in expr
pub(crate) fn let_binding(input: NixTokens<'_>) -> PResult<'_, Ast> {
    spanned(
        pair(
            token(Let),
            cut(pair(
                many0(alt((
                    statement.map(|(name, ast)| vec![(name, ast)]),
                    inherit
                        .map(|items| items.into_iter().map(|ast| (ast.as_span(), ast)).collect()),
                ))),
                preceded(token(In), expr),
            )),
        ),
        |span, (_, (bindings, body))| LetBinding {
            bindings: bindings.into_iter().flatten().collect(),
            body: Box::new(body),
            inherit: None,
            span,
        },
    )
    .parse(input)
}

/// Parse a with-statement.
/// with-expr = with expr;
pub(crate) fn with(input: NixTokens<'_>) -> PResult<'_, Ast> {
    preceded(token(Token::With), cut(terminated(expr, token(Semi))))(input)
}

pub(crate) fn atom(input: NixTokens<'_>) -> PResult<'_, Ast> {
    alt((let_binding, conditional, set, literal, ident))(input)
}

/// Parse an expression.
pub(crate) fn expr(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "expr",
        spanned(
            pair(
                opt(with),
                alt((
                    |input| prett_parsing(input, 0, Token::Semi),
                    lambda,
                    ident,
                    literal,
                    set,
                    assert,
                    let_binding,
                )),
            ),
            |span, (with, expr)| {
                #[cfg(test)]
                println!("expr: {:#?}", expr);
                if let Some(with) = with {
                    With {
                        set: Box::new(with),
                        body: Box::new(expr),
                        span,
                    }
                } else {
                    expr
                }
            },
        ),
    )(input)
}

pub(crate) fn prett_parsing(mut input: NixTokens<'_>, min_bp: u8, eof: Token) -> PResult<'_, Ast> {
    println!("input: {:?}", input);
    let (mut input, mut lhs) = match input
        .peek()
        .ok_or(nom::Err::Error(VerboseError::from_error_kind(
            input,
            nom::error::ErrorKind::Fail,
        )))?
        .0
    {
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

        Token::Comment | Token::DocComment | Token::LineComment => {
            unimplemented!("Comments are not yet implemented")
        }

        Token::LParen => {
            input.next();
            let (mut input, lhs) = prett_parsing(input, 0, eof)?;
            assert_eq!(input.next().unwrap().0, Token::RParen);
            (input, lhs)
        }

        Minus => {
            input.next();
            let right_bp = 23;
            let (new_input, rhs) = prett_parsing(input, right_bp, eof)?;
            (
                new_input,
                Ast::UnaryOp {
                    op: UnOp::Negation,
                    rhs: Box::new(rhs),
                    span: span_diff(&input, &new_input),
                },
            )
        }

        Token::Not => {
            input.next();
            let right_bp = 13;
            let (new_input, rhs) = prett_parsing(input, right_bp, eof)?;
            (
                new_input,
                Ast::UnaryOp {
                    op: UnOp::LogicalNegation,
                    rhs: Box::new(rhs),
                    span: span_diff(&input, &new_input),
                },
            )
        }

        e => {
            println!("unexpected token: {:?}", e);
            return Err(nom::Err::Error(VerboseError::from_error_kind(
                input,
                nom::error::ErrorKind::Tag,
            )));
        }
    };

    let mut application = false;
    loop {
        if let Some((token, _)) = input.peek() {
            if *token == eof {
                break;
            }
        } else {
            return Err(nom::Err::Error(VerboseError::from_error_kind(
                input,
                nom::error::ErrorKind::Eof,
            )));
        }

        let mut op = BinOp::from_token(input.peek().unwrap().0);

        if op.is_none() && (matches!(lhs, Ast::Identifier(..))) || application {
            application = true;
            op = Some(BinOp::Application);
        }

        if let Some(op) = op {
            let (left_bp, right_bp) = op.get_precedence();

            if left_bp < min_bp {
                break;
            }

            if !application {
                input.next();
            }

            let (_input, rhs) = prett_parsing(input, right_bp, eof).unwrap();
            input = _input;
            lhs = Ast::BinaryOp {
                op,
                span: Span {
                    start: lhs.as_span().start,
                    end: rhs.as_span().end,
                },
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };

            continue;
        };
        break;
    }

    Ok((input, lhs))
}
