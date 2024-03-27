use crate::{coalesced, infer, types::Var, InferError, Type};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use Type::*;

#[test]
fn test_primitve() {
    let source = "{}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Type::Record(HashMap::new()));

    let source = "{ x = 2;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), Number)].into()));

    let source = r#"{ x = "hi";}"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), String)].into()));

    let source = "{ x = 2.1;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), Number)].into()));

    let source = "{ x = true; y = false;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record([("x".to_string(), Bool), ("y".to_string(), Bool)].into())
    );

    let source = "{ x = ./.;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), Path)].into()));

    let source = "{ x = null;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), Null)].into()));

    let source = "{ x = [2 1];}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("x".to_string(), List(vec![Number]))].into()));

    let source = r#"{ x = ["hi" 1];}"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record([("x".to_string(), List(vec![String, Number]))].into())
    );

    let source = "{ x = {y = 1;};}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record([("x".to_string(), Record([("y".to_string(), Number)].into()))].into())
    );
}

#[test]
fn test_numbers() {
    let source = "1 + 1;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = "1 + 1.0;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = "1 * 1.0;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = "1 / 1.0;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = "1 - 1.0;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = r#"1 + "hi";"#;
    let res = infer(source);
    if let Err(e) = res {
        assert_eq!(
            e.error,
            InferError::TypeMismatch {
                expected: Number.get_name(),
                found: String.get_name()
            }
        );
    }

    let source = "x: x - 1;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Number])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Number)
        )
    );

    let source = "x: x + 1;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Number])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Number)
        )
    );

    let source = r#"x: x + "hi";"#;

    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![String])),
                id: 0,
                ..Default::default()
            })),
            Box::new(String)
        )
    );

    let source = r#"x: x + ./yeet/;"#;

    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Path])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Path)
        )
    );
    // TODO: test errors
    let source = r#"x: y: x + y;"#;
    let (ty, _) = infer(source).unwrap();

    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                lower_bounds: Rc::new(RefCell::new(vec![Number, String, Path])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Type::Function(
                Box::new(Var(Var {
                    lower_bounds: Rc::new(RefCell::new(vec![Number, String, Path])),
                    id: 1,
                    ..Default::default()
                })),
                Box::new(Type::Union(
                    Box::new(Type::Number),
                    Box::new(Type::Union(Box::new(Type::String), Box::new(Type::Path)))
                ))
            ))
        )
    );
}

#[test]
fn test_bools() {
    let source = "true && false;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Bool);

    let source = "true || false;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Bool);

    let source = "true -> false;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Bool);

    let source = "true && 1;";
    let res = infer(source);
    if let Err(e) = res {
        assert_eq!(
            e.error,
            InferError::CannotConstrain {
                lhs: Number,
                rhs: Bool
            }
        );
    }

    let source = "x: x && true;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Bool])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Bool)
        )
    );
}

#[test]
fn test_comparisons() {
    // TODO
}

#[test]
fn test_record_access() {
    let source = "x: x.y";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Record(
                    [(
                        "y".to_string(),
                        Var(Var {
                            id: 1,
                            ..Default::default()
                        })
                    )]
                    .into()
                )])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Var(Var::new(0, 1)))
        )
    );

    let source = "{y = 1;}.y";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = r#"{y = {z = "hi";};}.y.z"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, String);

    let source = r#"{y = {};}.y.x"#;
    let res = infer(source);
    assert_eq!(
        res,
        Err(InferError::MissingRecordField {
            field: "x".to_string()
        }
        .span(&(0..13)))
    );

    let source = r#"x: {y = {};}.y.x"#;
    let res = infer(source);
    assert_eq!(
        res,
        Err(InferError::MissingRecordField {
            field: "x".to_string()
        }
        .span(&(3..16)))
    )
}

#[test]
fn test_inherit() {
    let source = "let x = 1; in {inherit x;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Record(
            [(
                "x".to_string(),
                Var(Var {
                    lower_bounds: Rc::new(RefCell::new(vec![Number])),
                    id: 1,
                    ..Default::default()
                })
            )]
            .into()
        )
    );

    let source = "x: {inherit x;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                ..Default::default()
            })),
            Box::new(Record(
                [(
                    "x".to_string(),
                    Var(Var {
                        id: 0,
                        ..Default::default()
                    })
                )]
                .into()
            ))
        )
    );

    let source = "x: {inherit (x) y z;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Record(
                    [
                        (
                            "y".to_string(),
                            Var(Var {
                                id: 1,
                                ..Default::default()
                            })
                        ),
                        (
                            "z".to_string(),
                            Var(Var {
                                id: 2,
                                ..Default::default()
                            })
                        )
                    ]
                    .into()
                )])),
                ..Default::default()
            })),
            Box::new(Record(
                [
                    (
                        "y".to_string(),
                        Var(Var {
                            id: 1,
                            ..Default::default()
                        })
                    ),
                    (
                        "z".to_string(),
                        Var(Var {
                            id: 2,
                            ..Default::default()
                        })
                    )
                ]
                .into()
            ))
        )
    );

    let source = "{inherit ({x = 1;}) x;};";
    let (ty, _) = infer(source).unwrap();

    assert_eq!(ty, Record([("x".to_string(), Number)].into()));
}

#[test]
fn test_with() {
    let source = "with {y = 1;}; {z = y;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("z".to_string(), Number)].into()));

    let source = "let x = {y = 1;}; in with x; {z = y;};";
    infer(source).unwrap();
    assert_eq!(ty, Record([("z".to_string(), Number)].into()));

    let source = "x: with x; {z = y;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Record(
                    [(
                        "y".to_string(),
                        Var(Var {
                            id: 1,
                            ..Default::default()
                        })
                    )]
                    .into()
                )])),
                ..Default::default()
            })),
            Box::new(Record([("z".to_string(), Var(Var::new(0, 1)))].into()))
        )
    );

    // shadowing
    let source = r#"let y = true; in with {y = 1;}; {z = y;};"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record(
            [(
                "z".to_string(),
                Var(Var {
                    id: 1,
                    lower_bounds: Rc::new(RefCell::new(vec![Bool])),
                    ..Default::default()
                })
            )]
            .into()
        )
    );
}

#[test]
fn test_joining() {
    let source = r#"[1 2] ++ ["hi" "di"];"#;
    let res = infer(source).unwrap();
    assert_eq!(res.0, List(vec![Number, String]));

    let source = r#"x: x ++ ["hi" "di"];"#;
    let res = infer(source).unwrap();
    assert_eq!(
        res.0,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![List(vec![])])),
                ..Default::default()
            })),
            Box::new(List(vec![String]))
        )
    );

    let source = r#"x: ["hi" "di"] ++ x;"#;
    let res = infer(source).unwrap();
    assert_eq!(
        res.0,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![List(vec![])])),
                ..Default::default()
            })),
            Box::new(List(vec![String]))
        )
    );

    let source = r#"{ x = 1; } // { y = 2; };"#;
    let res = infer(source).unwrap();
    assert_eq!(
        res.0,
        Record([("x".to_string(), Number), ("y".to_string(), Number)].into())
    );

    let source = r#"{ x = 1; } // { x = 2; };"#;
    let res = infer(source).unwrap();
    assert_eq!(res.0, Record([("x".to_string(), Number)].into()));

    let source = r#"x: x // { x = 2; };"#;
    let res = infer(source).unwrap();
    assert_eq!(
        res.0,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Record(HashMap::new())])),
                ..Default::default()
            })),
            Box::new(Record([("x".to_string(), Number)].into()))
        )
    );

    let source = r#"x: {x = 2;} // x;"#;
    let res = infer(source).unwrap();

    assert_eq!(
        res.0,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Record([].into())])),
                ..Default::default()
            })),
            Box::new(Record([("x".to_string(), Number)].into()))
        )
    );
}

#[test]
fn test_attribute_fallback() {
    let source = r#"let t = y: y or 1; in {t = t;};"#;
    let (_ty, _ast) = infer(source).unwrap();
    println!("ast: {:?}", _ast);
}

#[test]
fn test_has_attribute() {
    let source = r#"{ x = 1; } ? x"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(ty, Bool);

    let source = r#"x: x ? y"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Record(
                    [("y".to_string(), Type::Optional(Box::new(Type::Undefined)))].into()
                )])),
                ..Default::default()
            })),
            Box::new(Bool)
        )
    );
}

#[test]
fn test_attr_set() {
    let source = "rec {f = x: x + y; y = 1;}";
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record(
            [
                (
                    "f".to_string(),
                    Var(Var {
                        id: 1,
                        lower_bounds: Rc::new(RefCell::new(vec![Function(
                            Box::new(Var(Var {
                                upper_bounds: Rc::new(RefCell::new(vec![Number])),
                                id: 2,
                                ..Default::default()
                            })),
                            Box::new(Var(Var {
                                id: 3,
                                upper_bounds: Rc::new(RefCell::new(vec![Number])),
                                ..Default::default()
                            }))
                        )])),
                        ..Default::default()
                    })
                ),
                (
                    "y".to_string(),
                    Var(Var {
                        id: 3,
                        upper_bounds: Rc::new(RefCell::new(vec![Number])),
                        ..Default::default()
                    })
                )
            ]
            .into()
        )
    );

    let source = "rec {x = {y = x;};}";
}

#[test]
fn test_let_binding() {
    let source = "let t = true; in t";
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Var(Var {
            lower_bounds: Rc::new(RefCell::new(vec![Bool])),
            id: 1,
            ..Default::default()
        })
    );

    let source = r#"let t = { x = 1; }; in t;"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Var(Var {
            lower_bounds: Rc::new(RefCell::new(vec![Record(
                [("x".to_string(), Number)].into()
            )])),
            id: 1,
            ..Default::default()
        })
    );

    let source = r#"let t = { x = 1; }; f = 1; in {inherit f t;};"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record(
            [
                (
                    "f".to_string(),
                    Var(Var {
                        id: 2,
                        lower_bounds: Rc::new(RefCell::new(vec![Number])),
                        ..Default::default()
                    })
                ),
                (
                    "t".to_string(),
                    Var(Var {
                        id: 3,
                        lower_bounds: Rc::new(RefCell::new(vec![Record(
                            [("x".to_string(), Number)].into()
                        )])),
                        ..Default::default()
                    })
                )
            ]
            .into()
        )
    );

    let source = r#"let t = { x = 1; }; f = t; in {inherit f t;};"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record(
            [
                (
                    "f".to_string(),
                    Var(Var {
                        id: 2,
                        lower_bounds: Rc::new(RefCell::new(vec![Record(
                            [("x".to_string(), Number)].into()
                        )])),
                        ..Default::default()
                    })
                ),
                (
                    "t".to_string(),
                    Var(Var {
                        id: 3,
                        lower_bounds: Rc::new(RefCell::new(vec![Record(
                            [("x".to_string(), Number)].into()
                        )])),
                        upper_bounds: Rc::new(RefCell::new(vec![Var(Var {
                            id: 4,
                            lower_bounds: Rc::new(RefCell::new(vec![Record(
                                [("x".to_string(), Number)].into()
                            )])),
                            ..Default::default()
                        })])),
                        ..Default::default()
                    })
                )
            ]
            .into()
        )
    );

    let source = r#"let f = t; t = { x = 1; }; in {inherit f t;};"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Record(
            [
                (
                    "f".to_string(),
                    Var(Var {
                        id: 2,
                        lower_bounds: Rc::new(RefCell::new(vec![Record(
                            [("x".to_string(), Number)].into()
                        )])),
                        ..Default::default()
                    })
                ),
                (
                    "t".to_string(),
                    Var(Var {
                        id: 3,
                        lower_bounds: Rc::new(RefCell::new(vec![Record(
                            [("x".to_string(), Number)].into()
                        )])),
                        upper_bounds: Rc::new(RefCell::new(vec![Var(Var {
                            id: 4,
                            lower_bounds: Rc::new(RefCell::new(vec![Record(
                                [("x".to_string(), Number)].into()
                            )])),
                            ..Default::default()
                        })])),
                        ..Default::default()
                    })
                )
            ]
            .into()
        )
    );

    let source = r#"let t = t + 1; in t;"#;
    let (ty, _) = coalesced(source).unwrap();
    println!("{:?}", ty.show());
}

#[test]
fn test_lambda() {
    let source = r#"let t = x: x; in { t = t; };"#;
    let (ty, _) = infer(source).unwrap();

    assert_eq!(
        ty,
        Record(
            [(
                "t".to_string(),
                Var(Var {
                    lower_bounds: Rc::new(RefCell::new(vec![Function(
                        Box::new(Var(Var {
                            lower_bounds: Rc::new(RefCell::new(vec![])),
                            upper_bounds: Rc::new(RefCell::new(vec![])),
                            id: 3,
                            ..Default::default()
                        })),
                        Box::new(Var(Var {
                            lower_bounds: Rc::new(RefCell::new(vec![])),
                            upper_bounds: Rc::new(RefCell::new(vec![])),
                            id: 3,
                            ..Default::default()
                        }))
                    )])),
                    id: 2,
                    ..Default::default()
                })
            )]
            .into()
        )
    );

    // Example taken from the paper
    let source = r#"f: x: f (f x)"#;
    let (ty, _) = coalesced(&source).unwrap();
    assert_eq!(
        ty.show(),
        "Var(0) ∧ Var(1) -> (Var(2)) ∧ Var(2) -> (Var(3)) -> (Var(1) -> (Var(3)))".to_string()
    );

    let source = r#"{ x, y }: y"#;
    let (ty, _) = infer(&source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Type::Pattern(
                [
                    ("x".to_string(), (Type::Var(Var::new(0, 0)), None)),
                    ("y".to_string(), (Type::Var(Var::new(0, 1)), None))
                ]
                .into(),
                false
            )),
            Box::new(Var(Var {
                id: 1,
                ..Default::default()
            }))
        )
    );

    let source = r#"{ x ? 1, y }: x"#;
    let (ty, _) = infer(&source).unwrap();
    let out = Var(Var {
        id: 0,
        upper_bounds: Rc::new(RefCell::new(vec![Type::Number])),
        ..Default::default()
    });
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Type::Pattern(
                [
                    ("x".to_string(), (out.clone(), Some(Type::Number))),
                    ("y".to_string(), (Type::Var(Var::new(0, 1)), None))
                ]
                .into(),
                false
            )),
            Box::new(out.clone())
        )
    );

    let source = r#"{ x ? 1, y } @ bind: bind"#;
    let (ty, _) = infer(&source).unwrap();
    let pat = Type::Pattern(
        [
            ("x".to_string(), (out.clone(), Some(Type::Number))),
            ("y".to_string(), (Type::Var(Var::new(0, 1)), None)),
        ]
        .into(),
        false,
    );
    assert_eq!(
        ty,
        Type::Function(
            Box::new(pat.clone()),
            Box::new(Var(Var {
                id: 2,
                upper_bounds: Rc::new(RefCell::new(vec![pat])),
                ..Default::default()
            }))
        )
    );

    let source = r#"{ ... } @ bind: bind"#;
    let (ty, _) = infer(&source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Type::Pattern(HashMap::new(), true)),
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Type::Record(HashMap::new())])),
                ..Default::default()
            }))
        )
    );
}

#[test]
fn test_application() {
    let source = "x: x 1;";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Function(
                    Box::new(Number),
                    Box::new(Var(Var {
                        id: 1,
                        ..Default::default()
                    }))
                )])),
                ..Default::default()
            })),
            Box::new(Var(Var {
                id: 1,
                ..Default::default()
            }))
        )
    );

    let source = "let f = x: x; in f 1";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Var(Var {
            id: 4,
            lower_bounds: Rc::new(RefCell::new(vec![Number])),
            ..Default::default()
        })
    );

    let source = "let f = { x }: x; in f { x = 1; };";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Var(Var {
            id: 4,
            lower_bounds: Rc::new(RefCell::new(vec![Number])),
            ..Default::default()
        })
    );

    let source = "let f = { x }: x; in f 1";
    let err = infer(source);
    assert_eq!(
        err,
        Err(InferError::CannotConstrain {
            lhs: Number,
            rhs: Pattern(
                [("x".to_string(), (Var(Var::new(1, 1)), None))].into(),
                false
            )
        }
        .span(&(21..22)))
    );

    let source = "y: let f = { x }: x; in f y";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                id: 0,
                upper_bounds: Rc::new(RefCell::new(vec![Pattern(
                    [(
                        "x".to_string(),
                        (
                            Var(Var {
                                id: 4,
                                upper_bounds: Rc::new(RefCell::new(vec![Var(Var {
                                    id: 5,
                                    ..Default::default()
                                })])),
                                ..Default::default()
                            }),
                            None
                        )
                    )]
                    .into(),
                    false
                )])),
                ..Default::default()
            })),
            Box::new(Var(Var {
                id: 5,
                ..Default::default()
            }))
        )
    );

    let source = "let f = { x }: x; in f {y = 1;}";
    let err = infer(source);
    assert_eq!(
        err,
        Err(InferError::MissingRecordField {
            field: "x".to_string()
        }
        .span(&(21..22)))
    );

    let source = "let f = { x }: x; in f {x = 1; y = 2;}";
    let err = infer(source);
    assert_eq!(
        err,
        Err(InferError::TooManyField {
            field: "y".to_string()
        }
        .span(&(21..22)))
    );

    let source = "let f = { x, ... }: x; in f {x = 1; y = 2;}";
    let ok = infer(source);
    assert!(ok.is_ok());

    let source = "let f = { x ? 1 }: x; in f {x = 1;}";
    infer(source).unwrap();

    let source = r#"let f = { x ? 1 }: x; in f {x = "hi";}"#;
    let err = infer(source);
    assert_eq!(
        err,
        Err(InferError::CannotConstrain {
            lhs: String,
            rhs: Number,
        }
        .span(&(25..26)))
    );
}

#[test]
fn test_assert() {
    let source = r#"assert true; {};"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([].into()));

    // TODO: why though
    let source = r#"assert 1; {};"#;
    let res = infer(source);
    if let Err(e) = res {
        assert_eq!(
            e.error,
            InferError::TypeMismatch {
                expected: Bool.get_name(),
                found: Number.get_name()
            }
        );
    }
}

#[test]
fn test_conditionals() {
    let source = r#"if true then 1 else "hi";"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Union(Box::new(Number), Box::new(String)));

    let source = r#"if true then 1 else 2;"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

    let source = r#"x: if x then 1 else 2;"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Function(
            Box::new(Var(Var {
                upper_bounds: Rc::new(RefCell::new(vec![Bool])),
                id: 0,
                ..Default::default()
            })),
            Box::new(Number)
        )
    );

    let source = r#"if "true" then 1 else 2;"#;
    let res = infer(source);
    if let Err(e) = res {
        assert_eq!(
            e.error,
            InferError::TypeMismatch {
                expected: Bool.get_name(),
                found: String.get_name()
            }
        );
    }
}
