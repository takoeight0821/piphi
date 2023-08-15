mod parser;
mod syntax;
use anyhow::Result;

fn main() -> Result<()> {
    use syntax::*;

    let fun = syntax::Expr::codata(vec![
        Clause::new(
            &Pat::sequence(vec![
                Pat::label("get"),
                Pat::sequence(vec![Pat::this(), Pat::variable("x")]),
            ]),
            &Expr::variable("x"),
        ),
        Clause::new(
            &Pat::sequence(vec![
                Pat::label("set"),
                Pat::sequence(vec![Pat::this(), Pat::variable("_")]),
                Pat::variable("y"),
            ]),
            &Expr::apply(&Expr::variable("fun"), &Expr::variable("y")),
        ),
    ]);
    let right = Expr::apply(&fun, &Expr::number(1));
    let left = Expr::variable("get");
    let expr = Expr::apply(&left, &right);

    println!("{}", expr);

    let src = format!("{}", expr);

    let tokens = parser::lexer::tokenize(&src)?;
    for token in &tokens {
        println!("{}", token);
    }

    Ok(())
}
