use super::{
    lexer::{remove_whitespace, tokenize},
    parse,
};
use crate::syntax::{Clause, Expr, ExprKind, Pat};
use pretty_assertions::assert_eq;

#[test]
fn parse_test() {
    let fun = Expr::codata(
        vec![
            Clause::new(
                Pat::sequence(
                    &[
                        Pat::label("get", Default::default()),
                        Pat::sequence(
                            &[
                                Pat::this(Default::default()),
                                Pat::variable("x", Default::default()),
                            ],
                            Default::default(),
                        ),
                    ],
                    Default::default(),
                ),
                Expr::variable("x", Default::default()),
            ),
            Clause::new(
                Pat::sequence(
                    &vec![
                        Pat::label("set", Default::default()),
                        Pat::sequence(
                            &[
                                Pat::this(Default::default()),
                                Pat::variable("p", Default::default()),
                            ],
                            Default::default(),
                        ),
                        Pat::variable("y", Default::default()),
                    ],
                    Default::default(),
                ),
                Expr::apply(
                    Expr::variable("fun", Default::default()),
                    Expr::variable("y", Default::default()),
                    Default::default(),
                ),
            ),
        ],
        Default::default(),
    );
    let right = Expr::apply(fun, Expr::number(1, Default::default()), Default::default());
    let left = Expr::label("get", Default::default());
    let expr = Expr::apply(left, right, Default::default());

    let src = format!("{}", expr);

    let tokens = tokenize(&src).unwrap();

    let parsed = parse(remove_whitespace(&tokens)).map(Box::new);

    assert_eq!(parsed.ok(), Some(Box::new(expr)))
}

#[test]
fn parse_let() {
    let expr = Expr::let_(
        "x",
        Expr::number(1, Default::default()),
        Expr::variable("x", Default::default()),
        Default::default(),
    );

    let src = format!("{}", expr);

    let tokens = tokenize(&src).unwrap();

    let parsed = parse(remove_whitespace(&tokens)).map(Box::new);

    assert_eq!(parsed.ok(), Some(Box::new(expr)))
}

#[test]
fn parse_fix() {
    let src = "fix f in { x -> f x }";
    let tokens = tokenize(src).unwrap();
    let parsed = parse(remove_whitespace(&tokens)).map(Box::new);
    assert!(parsed.is_ok_and(|x| matches!(x.kind, ExprKind::Fix(_, _))));
}

#[test]
fn parse_number_pat() {
    let src = "{ # 0 -> 1 }";
    let tokens = tokenize(src).unwrap();
    let parsed = parse(remove_whitespace(&tokens)).map(Box::new);
    assert!(parsed.is_ok_and(|x| matches!(x.kind, ExprKind::Codata(_))));
}
