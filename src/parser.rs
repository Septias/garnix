use logos::Span;
use nom::{
    branch::alt,
    combinator::{cut, opt},
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
            self, Comma, Dots, DoubleColon, Else, Equal, If, In, Inherit, LBrace, Let, Minus,
            Question, RBrace, Rec, Semi, Text, Then,
        },
    },
};

pub type PResult<'a, R> = IResult<NixTokens<'a>, R>;

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

/// Parse a set pattern.
fn set_pattern(input: NixTokens<'_>) -> PResult<'_, Pattern> {
    let elements = separated_list1(
        token(Comma),
        alt((
            ident.map(|ast| PatternElement::Identifier(ast.as_span())),
            ident_default_pattern,
        )),
    );
    let has_dots = opt(token(Dots)).map(|a| a.is_some());
    tuple((token(LBrace), elements, has_dots, token(RBrace)))
        .map(|(_, elems, has_dots, _)| Pattern {
            patterns: elems,
            is_wildcard: has_dots,
        })
        .parse(input)
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
    pair(ident.map(|ast| ast.as_span()), preceded(token(Equal), expr)).parse(input)
}

/// Parse a set definition.
pub(crate) fn set(input: NixTokens<'_>) -> PResult<'_, Ast> {
    let (input, is_recursive) = opt(token(Rec)).map(|a| a.is_some()).parse(input)?;
    delimited(
        token(LBrace),
        many0(terminated(statement, token(Semi))).map(move |statements| AttrSet {
            attrs: statements.into_iter().collect(),
            is_recursive,
        }),
        token(RBrace),
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
            preceded(token(In), set),
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
/// with-expr = with ident ; expr
pub(crate) fn with(input: NixTokens<'_>) -> PResult<'_, Ast> {
    preceded(
        token(Token::With),
        cut(pair(terminated(ident, token(Semi)), expr)),
    )
    .map(|(set, body)| With {
        set: Box::new(set),
        body: Box::new(body),
    })
    .parse(input)
}

/// Parse a literal.
fn literal(input: NixTokens<'_>) -> PResult<'_, Ast> {
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
    ))(input)
}

/// Parse an expression.
pub fn expr<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
    prett_parsing(input, 0)
}

pub fn atom<'b>(input: NixTokens<'_>) -> PResult<'_, Ast> {
    alt((let_binding, conditional, set, literal))(input)
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

    #[test]
    fn test_expr() {}
}
