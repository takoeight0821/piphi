use crate::{
    eval::{eval, flatten, new_env},
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

#[test]
fn test_countup() {
    eval_test(
        "let countup = fix f in { .head (# n) -> n, .tail (# n) -> f (add n 1)} in .head (.tail (countup 1))",
        super::Value::number(2),
    )
}

#[test]
fn test_fibo() {
    eval_test(
        r#"let zipWith = fix zipWith in {
              .head (# f xs ys) -> f (.head xs) (.head ys),
              .tail (# f xs ys) -> zipWith f (.tail xs) (.tail ys)
            } in
            let fibo = fix fibo in {
              .head # -> 1,
              .head (.tail #) -> 1,
              .tail (.tail #) -> zipWith add fibo (.tail fibo)
            } in
            .head (.tail (.tail fibo))"#,
        super::Value::number(2),
    );
}

#[test]
fn test_magic_swap() {
    eval_test(
        r#"
        let swap = { # (.fst x) -> .snd x, # (.snd x) -> .fst x } in
        let x = { .fst # -> 1, .snd # -> 2 } in
        swap (.snd x)"#,
        super::Value::number(1),
    )
}

fn eval_test(src: &str, expected: super::Value) {
    init();
    let tokens = tokenize(src).unwrap();
    let ast = parse(remove_whitespace(&tokens)).unwrap();
    let ast = flatten(&ast);
    debug!("{}", ast);
    let value = eval(new_env(), &ast);
    assert_eq!(value, expected);
}
