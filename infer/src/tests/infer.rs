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
    assert_eq!(
        ty,
        Record([("x".to_string(), List(vec![Number, Number]))].into())
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
                Type::Var(Var {
                    lower_bounds: Rc::new(RefCell::new(vec![Number])),
                    id: 1,
                    ..Default::default()
                })
            )]
            .into()
        )
    );
}

#[test]
fn test_with() {
    let source = "with {y = 1;}; {z = y;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Record([("z".to_string(), Number)].into()));

    let _source = "let x = {y = 1;}; in with x; {z = y;};";
    infer(source).unwrap();
}

#[test]
fn test_joining() {
    let source = r#"[1 2] ++ ["hi" "di"];"#;
    let res = infer(source).unwrap();
    assert_eq!(res.0, List(vec![Number, Number, String, String]));

    let source = r#"{ x = 1; } // { y = 2; };"#;
    let res = infer(source).unwrap();
    assert_eq!(
        res.0,
        Record([("x".to_string(), Number), ("y".to_string(), Number)].into())
    );

    let source = r#"{ x = 1; } // { x = 2; };"#;
    let res = infer(source).unwrap();
    assert_eq!(res.0, Record([("x".to_string(), Number)].into()));
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
    assert_eq!(ty, Type::Bool);
}

#[test]
fn test_let_binding() {
    let source = r#"let t = { x = 1; }; in t;"#;
    let (ty, _ast) = infer(source).unwrap();
    assert_eq!(
        ty,
        Type::Var(Var {
            lower_bounds: Rc::new(RefCell::new(vec![Record(
                [("x".to_string(), Number)].into()
            )])),
            id: 1,
            ..Default::default()
        })
    );
}

#[test]
fn test_function() {
    let source = r#"let t = x: x; in { t = t; };"#;
    /* let (ty, _) = infer(source).unwrap();

    if let Type::Record(rec) = &ty {
        println!(
            "whyyy {}",
            rec.iter()
                .map(|(name, ty)| format!(
                    "{}: {}",
                    name,
                    match ty {
                        Type::Var(var) =>
                            var.lower_bounds.borrow().iter().map(|t| t.show()).join(" "),
                        t => t.show(),
                    }
                ))
                .join("\n")
        );
    } else {
        panic!("Expected record type");
    }

    assert_eq!(
        ty,
        Record(
            [(
                "t".to_string(),
                Type::Var(Var {
                    lower_bounds: Rc::new(RefCell::new(vec![Function(
                        Box::new(Type::Var(Var {
                            lower_bounds: Rc::new(RefCell::new(vec![])),
                            upper_bounds: Rc::new(RefCell::new(vec![])),
                            id: 3,
                            ..Default::default()
                        })),
                        Box::new(Type::Var(Var {
                            lower_bounds: Rc::new(RefCell::new(vec![])),
                            upper_bounds: Rc::new(RefCell::new(vec![])),
                            id: 4,
                            ..Default::default()
                        }))
                    )])),
                    id: 2,
                    ..Default::default()
                })
            )]
            .into()
        )
    ); */

    let source = r#"f: x: f f x"#;

    let (ty, ast) = coalesced(&source).unwrap();
    println!("{}", ty.show())
}

#[test]
fn test_application() {}

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
fn test_if_conditions() {
    let source = r#"if true then 1 else "hi";"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Union(Box::new(Number), Box::new(String)));

    let source = r#"if true then 1 else 2;"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Number);

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
