use crate::{infer, Type};
use std::collections::HashMap;
use Type::*;

#[test]
fn test_primitve() {
    let source = "{}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Type::Set(HashMap::new()));

    let source = "{ x = 2;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), Int)].into()));

    let source = r#"{ x = "hi";}"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), String)].into()));

    let source = "{ x = 2.1;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), Float)].into()));

    let source = "{ x = true; y = false;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Set([("x".to_string(), Bool), ("y".to_string(), Bool)].into())
    );

    let source = "{ x = ./.;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), Path)].into()));

    let source = "{ x = null;}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), Null)].into()));

    let source = "hi";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Undefined);

    let source = "{ x = [2 1];}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), List(vec![Int, Int]))].into()));

    let source = "{ x = {y = 1;};}";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Set([("x".to_string(), Set([("y".to_string(), Int)].into()))].into())
    );

    let source = r#"if true then 1 else "hi";"#;
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Union(Box::new(Int), Box::new(String)));
}

#[test]
fn test_inherit() {
    let source = "let x = 1; in {inherit x;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(ty, Set([("x".to_string(), Int)].into()));
}

#[test]
fn test_with() {
    let source = "let x = {y = 1;}; in with x; {z = y;};";
    let (ty, _) = infer(source).unwrap();
    assert_eq!(
        ty,
        Set([("x".to_string(), Int), ("y".to_string(), Int)].into())
    );
}
