use super::{
    lexer::{remove_whitespace, tokenize},
    parse,
    range::{Position, Range},
};
use crate::syntax::{Clause, Expr, ExprKind, Pat, PatKind};
use pretty_assertions::assert_eq;

trait ResetPosition {
    fn reset(&self) -> Self;
}

impl ResetPosition for Position {
    fn reset(&self) -> Self {
        Position {
            offset: Default::default(),
            line: Default::default(),
            column: Default::default(),
        }
    }
}

impl ResetPosition for Range {
    fn reset(&self) -> Self {
        Range {
            start: self.start.reset(),
            end: self.end.reset(),
        }
    }
}

impl ResetPosition for Expr {
    fn reset(&self) -> Self {
        Expr {
            kind: self.kind.reset(),
            range: self.range.reset(),
        }
    }
}

impl ResetPosition for ExprKind {
    fn reset(&self) -> Self {
        match self {
            ExprKind::Apply(f, x) => ExprKind::Apply(Box::new(f.reset()), Box::new(x.reset())),
            ExprKind::Codata(cs) => ExprKind::Codata(cs.iter().map(|c| c.reset()).collect()),
            ExprKind::Let(name, value, body) => ExprKind::Let(
                name.clone(),
                Box::new(value.reset()),
                Box::new(body.reset()),
            ),
            _ => self.clone(),
        }
    }
}

impl ResetPosition for Clause {
    fn reset(&self) -> Self {
        Clause {
            pattern: self.pattern.reset(),
            body: self.body.reset(),
        }
    }
}

impl ResetPosition for Pat {
    fn reset(&self) -> Self {
        Pat {
            kind: self.kind.reset(),
            range: self.range.reset(),
        }
    }
}

impl ResetPosition for PatKind {
    fn reset(&self) -> Self {
        match self {
            PatKind::Sequence(ps) => PatKind::Sequence(ps.iter().map(|p| p.reset()).collect()),
            _ => self.clone(),
        }
    }
}

#[test]
fn parse_test() {
    let fun = Expr::codata(
        vec![
            Clause::new(
                &Pat::sequence(
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
                &Expr::variable("x", Default::default()),
            ),
            Clause::new(
                &Pat::sequence(
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
                &Expr::apply(
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

    let parsed = parse(remove_whitespace(&tokens)).map(|x| Box::new(x.reset()));

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

    let parsed = parse(remove_whitespace(&tokens)).map(|x| Box::new(x.reset()));

    assert_eq!(parsed.ok(), Some(Box::new(expr)))
}

#[test]
fn parse_fix() {
    let src = "fix f in { x -> f x }";
    let tokens = tokenize(src).unwrap();
    let parsed = parse(remove_whitespace(&tokens)).map(|x| Box::new(x.reset()));
    assert!(parsed.is_ok_and(|x| matches!(x.kind, ExprKind::Fix(_, _))));
}
