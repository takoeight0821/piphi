use crate::{
    eval::{eval, flatten},
    parser::{
        lexer::{remove_whitespace, tokenize},
        parse,
    },
};
use log::debug;
use std::{collections::HashMap, rc::Rc};

fn init() {
    let _ = env_logger::builder().is_test(true).try_init();
}

#[test]
fn test_simple() {
    eval_test("{ # x -> x } 1", super::Value::number(1));
}

#[test]
fn test_record() {
    eval_test(".get { .get # -> 1 }", super::Value::number(1));
}

#[test]
fn test_multi_args() {
    eval_test("{ # x y -> y } 1 2", super::Value::number(2));
}

#[test]
fn test_flattened() {
    eval_test(
        ".get ({ # x y -> { .get # -> x } } 1 2)",
        super::Value::number(1),
    );
}

#[test]
fn test_flattened2() {
    eval_test(
        ".get ({ # x -> { .get (# y) -> x } } 1 2)",
        super::Value::number(1),
    );
}

#[test]
fn test_complex() {
    eval_test(".get ({ .get (# x y) -> x } 1 2)", super::Value::number(1));
}

#[test]
fn test_nest_field() {
    eval_test(
        ".head (.tail { .head # -> 1, .head (.tail #) -> 2 })",
        super::Value::number(2),
    );
}

#[test]
fn test_let() {
    eval_test("let x = 1 in x", super::Value::number(1));
}

#[test]
fn test_fix() {
    eval_test(
        ".head (.tail ((fix f in { .head (# n) -> n, .tail (# n) -> f n}) 1))",
        super::Value::number(1),
    )
}

#[test]
fn test_ones() {
    eval_test(
        "let repeat = fix f in { .head (# n) -> n, .tail (# n) -> f n} in .head (.tail (repeat 1))",
        super::Value::number(1),
    )
}

fn eval_test(src: &str, expected: super::Value) {
    init();
    let tokens = tokenize(src).unwrap();
    let ast = parse(remove_whitespace(&tokens)).unwrap();
    let ast = flatten(&ast);
    debug!("{}", ast);
    let value = eval(Rc::new(HashMap::new()), &ast);
    assert_eq!(value, expected);
}
