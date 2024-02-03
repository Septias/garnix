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
fn ident(input: NixTokens<'_>) -> PResult<'_, Ast> {
    token(Text)
        .map(|(_, name)| Ast::Identifier(name))
        .parse(input)
}

/// Parse an identifier with a default value.
fn ident_default_pattern(input: NixTokens<'_>) -> PResult<'_, PatternElement> {
    tuple((ident, token(Question), cut(expr)))
        .map(|(ident, _, expr)| PatternElement::DefaultIdentifier(ident.as_span(), expr))
        .parse(input)
}

/// Parse a literal.
fn literal(input: NixTokens<'_>) -> PResult<'_, Ast> {
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
fn set_pattern(input: NixTokens<'_>) -> PResult<'_, Pattern> {
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

fn inherit(input: NixTokens<'_>) -> PResult<'_, Vec<Ast>> {
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

pub fn atom<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
    alt((let_binding, conditional, set, literal))(input)
}

/// Parse an expression.
pub fn expr<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
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

fn prett_parsing(input: NixTokens<'_>, min_bp: u8) -> PResult<'_, Ast> {
    let (mut input, mut lhs) = match input.peek().unwrap().0 {
        // Anything that resembles an atom
        Token::Path
        | If
        | Let
        | Rec
        | Token::LBrace
        | Token::With
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

        // Bracketed result in an atom
        Token::LParen => {
            let lhs = prett_parsing(input, 0)?;
            assert_eq!(input.peek().unwrap().0, Token::RParen);
            lhs
        }

        Minus => {
            let right_bp = 7;
            let (input, rhs) = prett_parsing(input, right_bp)?;
            (
                input,
                Ast::UnaryOperator {
                    op: UnOp::Negation,
                    rhs: Box::new(rhs),
                },
            )
        }

        Token::Not => {
            let right_bp = 17;
            let (input, rhs) = prett_parsing(input, right_bp)?;
            (
                input,
                Ast::UnaryOperator {
                    op: UnOp::LogicalNegation,
                    rhs: Box::new(rhs),
                },
            )
        }

        _ => panic!("Unexpected token"),
    };

    loop {
        let op = BinOp::from_token(input.peek().unwrap().0);

        if let Some(op) = op {
            let (left_bp, right_bp) = op.get_precedence();
            if left_bp < min_bp {
                break;
            }

            input.next();

            let (_input, rhs) = prett_parsing(input, right_bp)?;
            input = _input;
            lhs = Ast::BinaryOperator {
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

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use super::{
        assert, atom, conditional, expr, ident, ident_default_pattern, inherit, lambda,
        let_binding, literal, pattern, prett_parsing, set, set_pattern, statement, with,
    };
    use crate::{
        ast::{Ast, Pattern, PatternElement},
        lex,
        lexer::NixTokens,
    };

    #[test]
    fn test_ident() {
        let tokens = lex("player");
        let (input, ast) = ident(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(ast, Ast::Identifier(Range { start: 0, end: 6 }));
    }

    #[test]
    fn test_ident_default_pattern() {
        let tokens = lex("player ? 12");
        let (input, ast) = ident_default_pattern(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            PatternElement::DefaultIdentifier(Range { start: 0, end: 6 }, Ast::Integer(12))
        );
    }

    #[test]
    fn test_set_pattern() {
        let tokens = lex("{ player, position , ... }");
        let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Pattern {
                patterns: vec![
                    PatternElement::Identifier(Range { start: 2, end: 8 }),
                    PatternElement::Identifier(Range { start: 10, end: 18 })
                ],
                is_wildcard: true,
            }
        );

        let tokens = lex("{ ... }");
        let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Pattern {
                patterns: vec![],
                is_wildcard: true,
            }
        );

        let tokens = lex("{ }");
        let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Pattern {
                patterns: vec![],
                is_wildcard: false,
            }
        );
    }

    #[test]
    fn test_pattern() {
        let tokens = lex("player");
        let (input, ast) = pattern(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Pattern {
                patterns: vec![PatternElement::Identifier(Range { start: 0, end: 6 })],
                is_wildcard: false,
            }
        );
    }

    #[test]
    fn test_statement() {
        let tokens = lex("player = 12;");
        let (input, (name, ast)) = statement(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(name, Range { start: 0, end: 6 });
        assert_eq!(ast, Ast::Integer(12));
    }

    #[test]
    fn test_set() {
        let tokens = lex("{ player = 12; position = 13; }");
        let (input, ast) = set(NixTokens(&tokens))
            .map_err(|err| println!("{:#?}", err))
            .unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::AttrSet {
                attrs: vec![
                    (Range { start: 2, end: 8 }, Ast::Integer(12)),
                    (Range { start: 15, end: 23 }, Ast::Integer(13))
                ],
                is_recursive: false,
            }
        );

        let tokens = lex("{ }");
        let (input, ast) = set(NixTokens(&tokens))
            .map_err(|err| println!("{:#?}", err))
            .unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::AttrSet {
                attrs: vec![],
                is_recursive: false,
            }
        );

        let tokens = lex("rec { }");
        let (input, ast) = set(NixTokens(&tokens))
            .map_err(|err| println!("{:#?}", err))
            .unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::AttrSet {
                attrs: vec![],
                is_recursive: true,
            }
        )
    }

    #[test]
    fn test_lambda() {
        let tokens = lex("player: 12");
        let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::Lambda {
                arguments: vec![Pattern {
                    patterns: vec![PatternElement::Identifier(Range { start: 0, end: 6 })],
                    is_wildcard: false,
                }],
                body: Box::new(Ast::Integer(12)),
                arg_binding: None,
            }
        );

        let tokens = lex("player: position: 12");
        let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::Lambda {
                arguments: vec![
                    Pattern {
                        patterns: vec![PatternElement::Identifier(Range { start: 0, end: 6 })],
                        is_wildcard: false,
                    },
                    Pattern {
                        patterns: vec![PatternElement::Identifier(Range { start: 8, end: 16 })],
                        is_wildcard: false,
                    }
                ],
                body: Box::new(Ast::Integer(12)),
                arg_binding: None,
            }
        );

        let tokens = lex("{player}: 12");

        let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::Lambda {
                arguments: vec![Pattern {
                    patterns: vec![PatternElement::Identifier(Range { start: 1, end: 7 })],
                    is_wildcard: false,
                }],
                body: Box::new(Ast::Integer(12)),
                arg_binding: None,
            }
        );
    }

    #[test]
    fn test_conditional() {
        let tokens = lex("if true then 12 else 13");
        println!("{:?}", tokens);
        let (input, ast) = conditional(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::Conditional {
                condition: Box::new(Ast::Boolean(true)),
                expr1: Box::new(Ast::Integer(12)),
                expr2: Box::new(Ast::Integer(13)),
            }
        );
    }

    #[test]
    fn test_assert() {
        let tokens = lex("assert true;");
        let (input, ast) = assert(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::Assertion {
                condition: Box::new(Ast::Boolean(true)),
                then: Box::new(Ast::Null),
            }
        );
    }

    #[test]
    fn test_inherit() {
        let tokens = lex("inherit player;");
        let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(ast, vec![Ast::Identifier(Range { start: 8, end: 14 })]);

        let tokens = lex("inherit player position borders;");
        let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            vec![
                Ast::Identifier(Range { start: 8, end: 14 }),
                Ast::Identifier(Range { start: 15, end: 23 }),
                Ast::Identifier(Range { start: 24, end: 31 })
            ]
        );
    }

    #[test]
    fn test_let_binding() {
        let tokens = lex("let player = 12; in player");
        let (input, ast) = let_binding(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::LetBinding {
                bindings: vec![(Range { start: 4, end: 10 }, Ast::Integer(12))],
                body: Box::new(Ast::Identifier(Range { start: 20, end: 26 })),
                inherit: None,
            }
        );

        let tokens = lex("let player = 12; position = 13; in {}");
        let (input, ast) = let_binding(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::LetBinding {
                bindings: vec![
                    (Range { start: 4, end: 10 }, Ast::Integer(12)),
                    (Range { start: 17, end: 25 }, Ast::Integer(13))
                ],
                body: Box::new(Ast::AttrSet {
                    attrs: vec![],
                    is_recursive: false
                }),
                inherit: None,
            }
        );
    }

    #[test]
    fn test_with() {
        let tokens = lex("with player;");
        let (input, ast) = with(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(ast, Ast::Identifier(Range { start: 5, end: 11 }));

        let tokens = lex("with { a = 1; };");
        let (input, ast) = with(NixTokens(&tokens)).unwrap();
        assert!(input.0.is_empty());
        assert_eq!(
            ast,
            Ast::AttrSet {
                attrs: vec![(Range { start: 7, end: 8 }, Ast::Integer(1))],
                is_recursive: false,
            }
        );
    }
}
