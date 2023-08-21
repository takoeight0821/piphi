mod parser;
mod syntax;

use anyhow::Result;

use crate::parser::{lexer::remove_whitespace, range::Range};

fn main() -> Result<()> {
    use syntax::*;

    let fun = syntax::Expr::codata(
        vec![
            Clause::new(
                &Pat::sequence(
                    vec![
                        Pat::label("get", Default::default()),
                        Pat::sequence(
                            vec![
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
                    vec![
                        Pat::label("set", Default::default()),
                        Pat::sequence(
                            vec![
                                Pat::this(Default::default()),
                                Pat::variable("p", Default::default()),
                            ],
                            Default::default(),
                        ),
                        Pat::variable("y", Default::default()),
                    ],
                    Range::default(),
                ),
                &Expr::apply(
                    &Expr::variable("fun", Default::default()),
                    &Expr::variable("y", Default::default()),
                    Range::default(),
                ),
            ),
        ],
        Range::default(),
    );
    let right = Expr::apply(
        &fun,
        &Expr::number(1, Default::default()),
        Default::default(),
    );
    let left = Expr::variable(".get", Default::default());
    let expr = Expr::apply(&left, &right, Default::default());

    println!("{}", expr);

    let src = format!("{}", expr);

    let tokens = parser::lexer::tokenize(&src)?;

    let expr = parser::parse(remove_whitespace(&tokens))?;
    println!("{}", expr);

    Ok(())
}
