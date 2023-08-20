mod parser;
mod syntax;
use anyhow::Result;

use crate::parser::{lexer::remove_whitespace, range::Range};

fn main() -> Result<()> {
    use syntax::*;

    let fun = syntax::Expr::codata(vec![
        Clause::new(
            &Pat::sequence(
                vec![
                    Pat::label("get", Range::default()),
                    Pat::sequence(
                        vec![
                            Pat::this(Range::default()),
                            Pat::variable("x", Range::default()),
                        ],
                        Range::default(),
                    ),
                ],
                Range::default(),
            ),
            &Expr::variable("x"),
        ),
        Clause::new(
            &Pat::sequence(
                vec![
                    Pat::label("set", Range::default()),
                    Pat::sequence(
                        vec![
                            Pat::this(Range::default()),
                            Pat::variable("p", Range::default()),
                        ],
                        Range::default(),
                    ),
                    Pat::variable("y", Range::default()),
                ],
                Range::default(),
            ),
            &Expr::apply(&Expr::variable("fun"), &Expr::variable("y")),
        ),
    ]);
    let right = Expr::apply(&fun, &Expr::number(1));
    let left = Expr::variable(".get");
    let expr = Expr::apply(&left, &right);

    println!("{}", expr);

    let src = format!("{}", expr);

    let tokens = parser::lexer::tokenize(&src)?;

    let expr = parser::parse(remove_whitespace(&tokens))?;
    println!("{}", expr);

    Ok(())
}
