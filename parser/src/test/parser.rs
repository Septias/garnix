use core::panic;
use std::ops::Range;

use crate::ast::{BinOp, Inherit};
use crate::lexer::Token;
use crate::parser::{
    assert, conditional, expr, ident, ident_default_pattern, inherit, lambda, let_binding, literal,
    pattern, prett_parsing, set, set_pattern, statement, with, Statement,
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
    assert_eq!(ast, Range { start: 0, end: 6 });
}

#[test]
fn test_ident_default_pattern() {
    let tokens = lex("player ? 12");
    let (input, ast) = ident_default_pattern(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        PatternElement::DefaultIdentifier {
            identifier: Range { start: 0, end: 6 },
            span: Range { start: 0, end: 11 },
            ast: Ast::Int {
                val: 12,
                span: Range { start: 9, end: 11 }
            },
        }
    );
}

#[test]
fn test_set_pattern() {
    let tokens = lex("{ player, position , ... }");
    let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        (
            vec![
                PatternElement::Identifier(Range { start: 2, end: 8 }),
                PatternElement::Identifier(Range { start: 10, end: 18 })
            ],
            true
        )
    );

    let tokens = lex("{ ... }");
    let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, (vec![], true));

    let tokens = lex("{ }");
    let (input, ast) = set_pattern(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, (vec![], false));
}

#[test]
fn test_pattern() {
    let tokens = lex("player");
    let (input, ast) = pattern(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Pattern::Identifier(Range { start: 0, end: 6 }));
}

#[test]
fn test_statement() {
    let tokens = lex("player = 12;");
    let (input, stmnt) = statement(NixTokens(&tokens)).unwrap();
    if let Statement::Assignment(name, ast) = stmnt {
        assert!(input.0.is_empty());
        assert_eq!(name, Range { start: 0, end: 6 });
        assert_eq!(
            ast,
            Ast::Int {
                val: 12,
                span: Range { start: 9, end: 11 }
            }
        );
    } else {
        panic!("Expected an assignment statement");
    }

    let tokens = lex("player = 12 + 13;");
    let (input, stmnt) = statement(NixTokens(&tokens)).unwrap();
    if let Statement::Assignment(name, ast) = stmnt {
        assert!(input.0.is_empty());
        assert_eq!(name, Range { start: 0, end: 6 });
        assert_eq!(
            ast,
            Ast::BinaryOp {
                lhs: Box::new(Ast::Int {
                    val: 12,
                    span: Range { start: 9, end: 11 }
                }),
                rhs: Box::new(Ast::Int {
                    val: 13,
                    span: Range { start: 14, end: 16 }
                }),
                op: BinOp::Add,
                span: Range { start: 9, end: 16 }
            }
        );
    } else {
        panic!("Expected an assignment statement");
    }
}

#[test]
fn test_set() {
    let tokens = lex("{ player = 12; position = 13; }");
    let (input, ast) = set(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::AttrSet {
            attrs: vec![
                (
                    Range { start: 2, end: 8 },
                    Ast::Int {
                        val: 12,
                        span: Range { start: 11, end: 13 }
                    }
                ),
                (
                    Range { start: 15, end: 23 },
                    Ast::Int {
                        val: 13,
                        span: Range { start: 26, end: 28 }
                    }
                )
            ],
            inherit: vec![],
            is_recursive: false,
            span: Range { start: 0, end: 31 }
        }
    );

    let tokens = lex("{ }");
    let (input, ast) = set(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::AttrSet {
            attrs: vec![],
            is_recursive: false,
            span: Range { start: 0, end: 3 },
            inherit: vec![],
        }
    );

    let tokens = lex("rec { }");
    let (input, ast) = set(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::AttrSet {
            attrs: vec![],
            is_recursive: true,
            span: Range { start: 0, end: 7 },
            inherit: vec![],
        }
    );
    let tokens = lex("rec { inherit test; }");
    let (input, ast) = set(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::AttrSet {
            attrs: vec![],
            is_recursive: true,
            inherit: vec![Inherit {
                name: None,
                items: vec![Range { start: 14, end: 18 }]
            }],
            span: Range { start: 0, end: 21 }
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
            arguments: vec![Pattern::Identifier(Range { start: 0, end: 6 })],
            body: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 8, end: 10 }
            }),
            arg_binding: None,
            span: Range { start: 0, end: 10 }
        }
    );

    let tokens = lex("player: position: 12");
    let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Lambda {
            arguments: vec![
                Pattern::Identifier(Range { start: 0, end: 6 }),
                Pattern::Identifier(Range { start: 8, end: 16 }),
            ],
            body: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 18, end: 20 }
            }),
            arg_binding: None,
            span: Range { start: 0, end: 20 }
        }
    );

    let tokens = lex("{player}: 12");

    let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Lambda {
            arguments: vec![Pattern::Set {
                patterns: vec![PatternElement::Identifier(Range { start: 1, end: 7 })],
                is_wildcard: false,
                name: None,
            }],
            body: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 10, end: 12 }
            }),
            arg_binding: None,
            span: Range { start: 0, end: 12 }
        }
    );

    let tokens = lex("{}: 12");

    let (input, ast) = lambda(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Lambda {
            arguments: vec![Pattern::Set {
                patterns: vec![],
                is_wildcard: false,
                name: None,
            }],
            body: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 4, end: 6 }
            }),
            arg_binding: None,
            span: Range { start: 0, end: 6 }
        }
    );

    let tokens = lex("{}: let x = 12; in {};");
    let (_input, ast) = lambda(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::Lambda {
            arguments: vec![Pattern::Set {
                patterns: vec![],
                is_wildcard: false,
                name: None,
            }],
            body: Box::new(Ast::LetBinding {
                bindings: vec![(
                    Range { start: 8, end: 9 },
                    Ast::Int {
                        val: 12,
                        span: Range { start: 12, end: 14 }
                    }
                )],
                body: Box::new(Ast::AttrSet {
                    attrs: vec![],
                    is_recursive: false,
                    inherit: vec![],
                    span: Range { start: 19, end: 21 }
                }),
                inherit: vec![],
                span: Range { start: 4, end: 21 }
            }),
            arg_binding: None,
            span: Range { start: 0, end: 21 }
        }
    );

    let tokens = lex("x = {}: {inherit x;};");
    statement(NixTokens(&tokens)).unwrap();
}

#[test]
fn test_conditional() {
    let tokens = lex("if true then 12 else 13");
    let (input, ast) = conditional(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Conditional {
            condition: Box::new(Ast::Bool {
                val: true,
                span: Range { start: 3, end: 7 }
            }),
            expr1: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 13, end: 15 }
            }),
            expr2: Box::new(Ast::Int {
                val: 13,
                span: Range { start: 21, end: 23 }
            }),
            span: Range { start: 0, end: 23 }
        }
    );
}

#[test]
fn test_assert() {
    let tokens = lex("assert true;1");
    let (input, ast) = assert(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Assertion {
            condition: Box::new(Ast::Bool {
                val: true,
                span: Range { start: 7, end: 11 }
            }),
            span: Range { start: 0, end: 13 },
            expr: Box::new(Ast::Int {
                val: 1,
                span: Range { start: 12, end: 13 }
            }),
        }
    );
}

#[test]
fn test_inherit() {
    let tokens = lex("inherit player;");
    let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Inherit {
            name: None,
            items: vec![Range { start: 8, end: 14 }]
        }
    );

    let tokens = lex("inherit player position borders;");
    let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Inherit {
            items: vec![
                Range { start: 8, end: 14 },
                Range { start: 15, end: 23 },
                Range { start: 24, end: 31 }
            ],
            name: None
        }
    );

    let tokens = lex("inherit (lib) player position borders;");
    let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Inherit {
            items: vec![
                Range { start: 14, end: 20 },
                Range { start: 21, end: 29 },
                Range { start: 30, end: 37 }
            ],
            name: Some(vec![Range { start: 9, end: 12 }])
        }
    );

    let tokens = lex("inherit (lib.pkgs) player position borders;");
    let (input, ast) = inherit(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Inherit {
            items: vec![
                Range { start: 19, end: 25 },
                Range { start: 26, end: 34 },
                Range { start: 35, end: 42 }
            ],
            name: Some(vec![Range { start: 9, end: 12 }, Range { start: 13, end: 17 }])
        }
    );

    let tokens = lex("inherit () player position borders;");
    assert!(inherit(NixTokens(&tokens)).is_err());
}

#[test]
fn test_let_binding() {
    let tokens = lex("let player = 12; in player");
    let (input, ast) = let_binding(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::LetBinding {
            bindings: vec![(
                Range { start: 4, end: 10 },
                Ast::Int {
                    val: 12,
                    span: Range { start: 13, end: 15 }
                }
            )],
            body: Box::new(Ast::Identifier(Range { start: 20, end: 26 })),
            inherit: vec![],
            span: Range { start: 0, end: 26 }
        }
    );

    let tokens = lex("let player = 12; position = 13; in {};");
    let (_input, ast) = let_binding(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::LetBinding {
            bindings: vec![
                (
                    Range { start: 4, end: 10 },
                    Ast::Int {
                        val: 12,
                        span: Range { start: 13, end: 15 }
                    }
                ),
                (
                    Range { start: 17, end: 25 },
                    Ast::Int {
                        val: 13,
                        span: Range { start: 28, end: 30 }
                    }
                )
            ],
            body: Box::new(Ast::AttrSet {
                attrs: vec![],
                is_recursive: false,
                inherit: vec![],
                span: Range { start: 35, end: 37 }
            }),
            inherit: vec![],
            span: Range { start: 0, end: 37 }
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
            attrs: vec![(
                Range { start: 7, end: 8 },
                Ast::Int {
                    val: 1,
                    span: Range { start: 11, end: 12 }
                }
            )],
            is_recursive: false,
            inherit: vec![],
            span: Range { start: 5, end: 15 }
        }
    );
}

#[test]
fn test_literal() {
    let tokens = lex("null");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Null(Range { start: 0, end: 4 }));

    let tokens = lex("true");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Bool {
            val: true,
            span: Range { start: 0, end: 4 }
        }
    );

    let tokens = lex("false");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Bool {
            val: false,
            span: Range { start: 0, end: 5 }
        }
    );

    let tokens = lex("12");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Int {
            val: 12,
            span: Range { start: 0, end: 2 }
        }
    );

    let tokens = lex("12.12");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(
        ast,
        Ast::Float {
            val: 12.12,
            span: Range { start: 0, end: 5 }
        }
    );

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

    let tokens = lex("./.");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::NixPath(Range { start: 0, end: 3 }));

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

    let tokens = lex("<pictures.md>");
    let (input, ast) = literal(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::SearchPath(Range { start: 0, end: 13 }));
}

#[test]
fn test_expression() {
    let tokens = lex("a");
    let (input, _ast) = expr(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());

    let tokens = lex("a");
    let (input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert!(input.0.is_empty());
    assert_eq!(ast, Ast::Identifier(Range { start: 0, end: 1 }));

    let tokens = lex(r#"[1 2] ++ ["hi" 1.0];"#);
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            lhs: Box::new(Ast::List {
                exprs: vec![
                    Ast::Int {
                        val: 1,
                        span: Range { start: 1, end: 2 }
                    },
                    Ast::Int {
                        val: 2,
                        span: Range { start: 3, end: 4 }
                    }
                ],
                span: Range { start: 0, end: 5 }
            }),
            rhs: Box::new(Ast::List {
                exprs: vec![
                    Ast::NixString(Range { start: 10, end: 14 }),
                    Ast::Float {
                        val: 1.0,
                        span: Range { start: 15, end: 18 }
                    }
                ],
                span: Range { start: 9, end: 19 }
            }),
            op: BinOp::ListConcat,
            span: Range { start: 0, end: 19 }
        }
    );

    let tokens = r#"{ x = 1; } // { y = 2; };"#;
    let (_input, ast) = expr(NixTokens(&lex(tokens))).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            lhs: Box::new(Ast::AttrSet {
                attrs: vec![(
                    Range { start: 2, end: 3 },
                    Ast::Int {
                        val: 1,
                        span: Range { start: 6, end: 7 }
                    }
                )],
                is_recursive: false,
                inherit: vec![],
                span: Range { start: 0, end: 10 }
            }),
            rhs: Box::new(Ast::AttrSet {
                attrs: vec![(
                    Range { start: 16, end: 17 },
                    Ast::Int {
                        val: 2,
                        span: Range { start: 20, end: 21 }
                    }
                )],
                is_recursive: false,
                inherit: vec![],
                span: Range { start: 14, end: 24 }
            }),
            op: BinOp::Update,
            span: Range { start: 0, end: 24 }
        }
    );

    let tokens = lex("let t = { x = 1; } ? x; in {};");
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::LetBinding {
            bindings: vec![(
                Range { start: 4, end: 5 },
                Ast::BinaryOp {
                    op: BinOp::HasAttribute,
                    lhs: Box::new(Ast::AttrSet {
                        attrs: vec![(
                            Range { start: 10, end: 11 },
                            Ast::Int {
                                val: 1,
                                span: Range { start: 14, end: 15 }
                            }
                        )],
                        is_recursive: false,
                        inherit: vec![],
                        span: Range { start: 8, end: 18 }
                    }),
                    rhs: Box::new(Ast::Identifier(Range { start: 21, end: 22 })),
                    span: Range { start: 8, end: 22 }
                },
            )],
            body: Box::new(Ast::AttrSet {
                attrs: vec![],
                is_recursive: false,
                inherit: vec![],
                span: Range { start: 27, end: 29 }
            }),
            inherit: vec![],
            span: Range { start: 0, end: 29 }
        }
    );

    let tokens = lex(";");
    assert!(expr(NixTokens(&tokens)).is_err());
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
            lhs: Box::new(Ast::Int {
                val: 12,
                span: Range { start: 0, end: 2 }
            }),
            rhs: Box::new(Ast::BinaryOp {
                lhs: Box::new(Ast::Int {
                    val: 12,
                    span: Range { start: 5, end: 7 }
                }),
                rhs: Box::new(Ast::Int {
                    val: 13,
                    span: Range { start: 10, end: 12 }
                }),
                span: Range { start: 5, end: 12 },
                op: Mul,
            }),
            op: Add,
            span: Range { start: 0, end: 12 }
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
                lhs: Box::new(Ast::Int {
                    val: 12,
                    span: Range { start: 1, end: 3 }
                }),
                rhs: Box::new(Ast::Int {
                    val: 12,
                    span: Range { start: 6, end: 8 }
                }),
                span: Range { start: 1, end: 8 },
                op: Add,
            }),
            rhs: Box::new(Ast::Int {
                val: 13,
                span: Range { start: 12, end: 14 }
            }),
            op: Mul,
            span: Range { start: 1, end: 14 },
        }
    );
}

#[test]
fn test_long_lambda() {
    let tokens = lex(r#"let player = 12; position = 12 * 11; name = "bob"; in {};"#);
    let (_input, _) = expr(NixTokens(&tokens)).unwrap();

    let tokens = lex(
        r#"let player = "hi"; position = (12 * 11) + 1; name = "bob"; set = {x = "1";}; in {};"#,
    );
    let (_input, _) = expr(NixTokens(&tokens)).unwrap();
}

#[test]
fn test_application() {
    let tokens = lex(r#"map map (1+1);"#);
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            op: BinOp::Application,
            lhs: Box::new(Ast::BinaryOp {
                op: BinOp::Application,
                lhs: Box::new(Ast::Identifier(Range { start: 0, end: 3 })),
                rhs: Box::new(Ast::Identifier(Range { start: 4, end: 7 })),
                span: Range { start: 0, end: 7 }
            }),
            rhs: Box::new(Ast::BinaryOp {
                op: BinOp::Add,
                lhs: Box::new(Ast::Int {
                    val: 1,
                    span: Range { start: 9, end: 10 }
                }),
                rhs: Box::new(Ast::Int {
                    val: 1,
                    span: Range { start: 11, end: 12 }
                }),
                span: Range { start: 9, end: 12 }
            }),
            span: Range { start: 0, end: 12 }
        }
    );

    let tokens = lex(r#"map map.map (1+1);"#);
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            op: BinOp::Application,
            lhs: Box::new(Ast::BinaryOp {
                op: BinOp::Application,
                lhs: Box::new(Ast::Identifier(Range { start: 0, end: 3 })),
                rhs: Box::new(Ast::BinaryOp {
                    op: BinOp::AttributeSelection,
                    lhs: Box::new(Ast::Identifier(Range { start: 4, end: 7 })),
                    rhs: Box::new(Ast::Identifier(Range { start: 8, end: 11 })),
                    span: Range { start: 4, end: 11 }
                }),
                span: Range { start: 0, end: 11 }
            }),
            rhs: Box::new(Ast::BinaryOp {
                op: BinOp::Add,
                lhs: Box::new(Ast::Int {
                    val: 1,
                    span: Range { start: 13, end: 14 }
                }),
                rhs: Box::new(Ast::Int {
                    val: 1,
                    span: Range { start: 15, end: 16 }
                }),
                span: Range { start: 13, end: 16 }
            }),
            span: Range { start: 0, end: 16 }
        }
    );

    let tokens = lex(r#"map (x: x*x) (map (x: x*x) [1 2 3]);"#);
    expr(NixTokens(&tokens)).unwrap();
}

#[test]
fn test_attribute_access() {
    let tokens = lex(r#"map.map;"#);
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            op: BinOp::AttributeSelection,
            lhs: Box::new(Ast::Identifier(Range { start: 0, end: 3 })),
            rhs: Box::new(Ast::Identifier(Range { start: 4, end: 7 })),
            span: Range { start: 0, end: 7 }
        }
    );

    let tokens = lex(r#"map.map.map;"#);
    let (_input, ast) = expr(NixTokens(&tokens)).unwrap();
    assert_eq!(
        ast,
        Ast::BinaryOp {
            op: BinOp::AttributeSelection,
            lhs: Box::new(Ast::BinaryOp {
                op: BinOp::AttributeSelection,
                lhs: Box::new(Ast::Identifier(Range { start: 0, end: 3 })),
                rhs: Box::new(Ast::Identifier(Range { start: 4, end: 7 })),
                span: Range { start: 0, end: 7 }
            }),
            rhs: Box::new(Ast::Identifier(Range { start: 8, end: 11 })),
            span: Range { start: 0, end: 11 }
        }
    );
}
