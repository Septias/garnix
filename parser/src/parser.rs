use logos::Span;
use nom::{
    branch::alt,
    combinator::{cut, opt, success},
    error::{context, ParseError, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Err, IResult, InputLength, Parser,
};

use crate::{
    ast::{
        Ast::{self, *},
        BinOp, Inherit, Pattern, PatternElement, UnOp,
    },
    lexer::{
        nom_interop::token,
        NixTokens,
        Token::{
            self, Assignment, Comma, Dots, DoubleColon, Else, If, In, LBrace, Let, Minus, Question,
            RBrace, Rec, Semi, Text, Then,
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
pub(crate) fn ident(input: NixTokens<'_>) -> PResult<'_, Span> {
    token(Text).map(|(_, name)| name).parse(input)
}

/// Parse an identifier with a default value.
pub(crate) fn ident_default_pattern(input: NixTokens<'_>) -> PResult<'_, PatternElement> {
    spanned(
        tuple((ident, token(Question), cut(expr))),
        |span, (identifier, _, ast)| PatternElement::DefaultIdentifier {
            identifier,
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
            token(Token::SearchPath).map(|(_, span)| SearchPath(span)),
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
pub(crate) fn set_pattern(input: NixTokens<'_>) -> PResult<'_, (Vec<PatternElement>, bool)> {
    let elements = separated_list1(
        token(Comma),
        alt((
            ident.map(|ast| PatternElement::Identifier(ast)),
            ident_default_pattern,
        )),
    );

    context(
        "Pattern",
        preceded(
            token(LBrace),
            terminated(
                pair(
                    elements,
                    opt(preceded(token(Comma), token(Dots))).map(|a| a.is_some()),
                )
                .map(|elem| (elem.0, elem.1))
                .or(token(Dots).map(|_| (vec![], true)))
                .or(success((vec![], false))),
                token(RBrace),
            ),
        ),
    )(input)
}

/// Parse a pattern.
/// pattern = identifier | set-pattern
pub(crate) fn pattern(input: NixTokens<'_>) -> PResult<'_, Pattern> {
    alt((
        ident.map(|ast| Pattern::Identifier(ast)),
        pair(opt(terminated(ident, token(Token::At))), set_pattern).map(
            |(name, (patterns, is_wildcard))| Pattern::Set {
                patterns,
                name,
                is_wildcard,
            },
        ),
        pair(set_pattern, opt(terminated(ident, token(Token::At)))).map(
            |((patterns, is_wildcard), name)| Pattern::Set {
                patterns,
                name,
                is_wildcard,
            },
        ),
    ))(input)
}

/// Parse a single statement.
/// ident = expr;
/// inherit [()] [ident ];
pub(crate) fn statement(input: NixTokens<'_>) -> PResult<'_, Statement> {
    context(
        "statement",
        alt((
            terminated(
                pair(ident, cut(preceded(token(Assignment), expr))),
                token(Semi),
            )
            .map(|statement| Statement::Assignment(statement.0, statement.1)),
            inherit.map(|inherit| Statement::Inherit(inherit)),
        )),
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
            |span, (is_recursive, statements)| {
                let (attrs, inherits): (Vec<Statement>, Vec<Statement>) = statements
                    .into_iter()
                    .partition(|stmt| matches!(stmt, Statement::Assignment(..)));
                AttrSet {
                    attrs: attrs
                        .into_iter()
                        .map(|stmt| match stmt {
                            Statement::Assignment(name, ast) => (name, ast),
                            _ => unreachable!(),
                        })
                        .collect(),
                    is_recursive,
                    inherit: inherits
                        .into_iter()
                        .map(|stmt| match stmt {
                            Statement::Inherit(inherit) => inherit,
                            _ => unreachable!(),
                        })
                        .collect(),
                    span,
                }
            },
        ),
    )(input)
}

/// Parse a lambda function.
/// lambda = pat : expr
pub fn lambda(input: NixTokens<'_>) -> PResult<'_, Ast> {
    spanned(
        pair(terminated(pattern, token(DoubleColon)), expr),
        |span, (pattern, body)| Lambda {
            pattern,
            body: Box::new(body),
            span,
        },
    )(input)
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

pub(crate) fn inherit(input: NixTokens<'_>) -> PResult<'_, Inherit> {
    let inherit_root = delimited(token(Token::LParen), expr, token(Token::RParen));
    delimited(
        token(Token::Inherit),
        pair(
            opt(inherit_root),
            many1(alt((ident, token(Token::SingleString).map(|a| a.1)))),
        ),
        token(Semi),
    )
    .map(|(name, items)| Inherit { name, items })
    .parse(input)
}

pub(crate) enum Statement {
    Inherit(Inherit),
    Assignment(Span, Ast),
}

/// Parse a let binding.
/// let-expr = let [ identifier = expr ; with ;]... in expr
pub(crate) fn let_binding(input: NixTokens<'_>) -> PResult<'_, Ast> {
    spanned(
        pair(
            token(Let),
            cut(pair(many0(statement), preceded(token(In), expr))),
        ),
        |span, (_, (statements, body))| {
            let (attrs, inherits): (Vec<Statement>, Vec<Statement>) = statements
                .into_iter()
                .partition(|stmt| matches!(stmt, Statement::Assignment(..)));

            let inherit: Vec<_> = inherits
                .into_iter()
                .map(|stmt| match stmt {
                    Statement::Inherit(inherit) => inherit,
                    Statement::Assignment(_, _) => unreachable!(),
                })
                .collect();

            LetBinding {
                bindings: attrs
                    .into_iter()
                    .map(|stmt| match stmt {
                        Statement::Assignment(name, ast) => (name, ast),
                        _ => unreachable!(),
                    })
                    .collect(),
                body: Box::new(body),
                inherit,
                span,
            }
        },
    )
    .parse(input)
}

/// with-statement = with expr; expr;
pub(crate) fn with(input: NixTokens<'_>) -> PResult<'_, Ast> {
    preceded(token(Token::With), cut(terminated(expr, token(Semi))))(input)
}

pub(crate) fn list(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "list",
        spanned(
            delimited(
                token(Token::LBracket),
                many0(alt((ident.map(|ast| Ast::Identifier(ast)), literal))),
                token(Token::RBracket),
            ),
            |span, exprs| List { exprs, span },
        ),
    )(input)
}

/// Parse an expression.
pub(crate) fn expr(input: NixTokens<'_>) -> PResult<'_, Ast> {
    context(
        "expr",
        spanned(
            pair(
                opt(with),
                alt((
                    lambda,
                    |input| prett_parsing(input, 0, Token::Semi),
                    assert,
                    let_binding,
                    literal,
                    ident.map(|ast| Ast::Identifier(ast)),
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

const ILLEGAL: [Token; 7] = [RBrace, In, Let, Rec, Token::With, Token::Else, Token::Then];

pub(crate) fn pratt_atom(input: NixTokens<'_>) -> PResult<'_, Ast> {
    alt((
        let_binding,
        conditional,
        set,
        list,
        literal,
        ident.map(|ast| Ast::Identifier(ast)),
    ))(input)
}

pub(crate) fn prett_parsing(mut input: NixTokens<'_>, min_bp: u8, eof: Token) -> PResult<'_, Ast> {
    #[cfg(test)]
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
        | Token::LBracket
        | Token::Integer(_)
        | Token::Float(_)
        | Text => pratt_atom(input)?,

        Token::Comment | Token::DocComment | Token::LineComment => {
            unimplemented!("Comments are not yet implemented")
        }

        Token::LParen => {
            input.next();
            if let Ok((mut input, lhs)) = lambda(input) {
                assert_eq!(input.next().unwrap().0, Token::RParen);
                (input, lhs)
            } else {
                let (mut input, lhs) = prett_parsing(input, 0, eof)?;
                assert_eq!(input.next().unwrap().0, Token::RParen);
                (input, lhs)
            }
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

        _ => {
            return Err(nom::Err::Error(VerboseError::from_error_kind(
                input,
                nom::error::ErrorKind::Tag,
            )));
        }
    };

    let mut application = false;
    loop {
        if let Some((token, _)) = input.peek() {
            if *token == eof || *token == Token::RParen || *token == Token::RBracket {
                break;
            }
        } else {
            break;
        }

        let mut op = BinOp::from_token(input.peek().unwrap().0);

        if op.is_none() && matches!(lhs, Ast::Identifier(..)) {
            if let Some((token, _)) = input.peek() {
                if ILLEGAL.contains(token) {
                    break;
                }
            }
            application = true;
            op = Some(BinOp::Application);
        }

        if application {
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

            let (_input, rhs) = prett_parsing(input, right_bp, eof).map_err(|e| match e {
                Err::Incomplete(_) => todo!(),
                Err::Error(e) | Err::Failure(e) => Err::Failure(e),
            })?;
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
