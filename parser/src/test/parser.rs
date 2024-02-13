use std::ops::Range;

use crate::ast::BinOp;
use crate::lexer::Token;
use crate::parser::{
    assert, conditional, expr, ident, ident_default_pattern, inherit, lambda, let_binding, literal,
    pattern, prett_parsing, set, set_pattern, statement, with,
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
        PatternElement::DefaultIdentifier(Range { start: 0, end: 6 }, Ast::Int(12))
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
    assert_eq!(ast, Ast::Int(12));

    let tokens = lex("player = 12 + 13;");
    let (input, (name, ast)) = statement(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(name, Range { start: 0, end: 6 });
    assert_eq!(
        ast,
        Ast::BinaryOp {
            lhs: Box::new(Ast::Int(12)),
            rhs: Box::new(Ast::Int(13)),
            op: BinOp::Add,
        }
    );
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
                (Range { start: 2, end: 8 }, Ast::Int(12)),
                (Range { start: 15, end: 23 }, Ast::Int(13))
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
            body: Box::new(Ast::Int(12)),
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
            body: Box::new(Ast::Int(12)),
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
            body: Box::new(Ast::Int(12)),
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
            condition: Box::new(Ast::Bool(true)),
            expr1: Box::new(Ast::Int(12)),
            expr2: Box::new(Ast::Int(13)),
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
            condition: Box::new(Ast::Bool(true)),
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
            bindings: vec![(Range { start: 4, end: 10 }, Ast::Int(12))],
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
                (Range { start: 4, end: 10 }, Ast::Int(12)),
                (Range { start: 17, end: 25 }, Ast::Int(13))
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
            attrs: vec![(Range { start: 7, end: 8 }, Ast::Int(1))],
            is_recursive: false,
        }
    );
}

#[test]
fn test_literal() {
    let tokens = lex("null");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Null);

    let tokens = lex("true");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Bool(true));

    let tokens = lex("false");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Bool(false));

    let tokens = lex("12");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Int(12));

    let tokens = lex("12.12");
    println!("{:?}", tokens);
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Float(12.12));

    let tokens = lex("/** hi */");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::DocComment(Range { start: 0, end: 9 }));

    let tokens = lex("/* hi */");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Comment(Range { start: 0, end: 8 }));

    let tokens = lex("# hi\n");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::LineComment(Range { start: 0, end: 5 }));

    let tokens = lex(r#""hallo""#);
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixString(Range { start: 0, end: 7 }));

    let tokens = lex(r#"''hallo''"#);
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixString(Range { start: 0, end: 9 }));

    let tokens = lex("./files/pictures");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixPath(Range { start: 0, end: 16 }));

    let tokens = lex("/files/pictures");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixPath(Range { start: 0, end: 15 }));

    let tokens = lex("~/files/pictures");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixPath(Range { start: 0, end: 16 }));
}

#[test]
fn test_expression() {
    let tokens = lex("a");
    let (input, ast) = expr(NixTokens(&tokens)).unwrap();

    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::LetBinding {
            bindings: vec![(Range { start: 4, end: 10 }, Ast::Int(12))],
            body: Box::new(Ast::Identifier(Range { start: 20, end: 26 })),
            inherit: None,
        }
    );
}

#[test]
fn test_prett_parsing() {
    use BinOp::*;

    let tokens = lex("12 + 12 * 13;");
    let (mut input, ast) = prett_parsing(NixTokens(&tokens), 0, Token::Semi).unwrap();
    input.next();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::BinaryOp {
            lhs: Box::new(Ast::Int(12)),
            rhs: Box::new(Ast::BinaryOp {
                lhs: Box::new(Ast::Int(12)),
                rhs: Box::new(Ast::Int(13)),
                op: Mul,
            }),
            op: Add,
        }
    );

    let tokens = lex("(12 + 12) * 13;");
    let (mut input, ast) = prett_parsing(NixTokens(&tokens), 0, Token::Semi).unwrap();
    input.next();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::BinaryOp {
            lhs: Box::new(Ast::BinaryOp {
                lhs: Box::new(Ast::Int(12)),
                rhs: Box::new(Ast::Int(12)),
                op: Add,
            }),
            rhs: Box::new(Ast::Int(13)),
            op: Mul,
        }
    );
}

#[test]
fn test_long_lambda() {
    let tokens = lex(r#"let player = 12; position = 12 * 11; name = "bob"; in {}"#);
    let (input, _) = expr(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());

    let tokens = lex(
        r#"let player = "hi"; position = (12 * 11) + 1; name = "bob"; set = {x = "1";}; in {}"#,
    );
    let (input, _) = expr(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
}

#[test]
fn test_application() {
    let tokens = lex(r#"map 1;"#);
    let (input, _) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(input.0.get(0).unwrap().0, Token::Semi);

    let tokens = lex(r#"map (1+1);"#);
    let (input, _) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(input.0.get(0).unwrap().0, Token::Semi);

    let tokens = lex(r#"map map (1+1);"#);
    let (input, _) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(input.0.get(0).unwrap().0, Token::Semi);
}


#[test]
fn test_attribute_access() {
    todo!()
}