use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
}

impl Expr {
    pub fn variable(name: &str) -> Expr {
        Expr {
            kind: ExprKind::Variable(Ident {
                name: name.to_string(),
            }),
        }
    }

    pub fn number(n: i64) -> Expr {
        Expr {
            kind: ExprKind::Number(n),
        }
    }

    pub fn add(left: &Rc<Expr>, right: &Rc<Expr>) -> Expr {
        Expr {
            kind: ExprKind::Add(left.clone(), right.clone()),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Variable(Ident),
    Number(i64),
    Add(Rc<Expr>, Rc<Expr>),
}

#[derive(Debug)]
pub struct Ident {
    name: String,
}

fn main() {
    let left = Rc::new(Expr::number(1));
    let right = Rc::new(Expr::variable("x"));
    let expr = Expr::add(&left, &right);

    println!("{:?}", expr);
}
