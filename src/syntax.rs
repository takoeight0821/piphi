use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Expr {
    kind: ExprKind,
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Expr {
    pub fn variable(name: &str) -> Rc<Expr> {
        Rc::new(Expr {
            kind: ExprKind::Variable(Ident {
                name: name.to_string(),
            }),
        })
    }

    pub fn number(n: i64) -> Rc<Expr> {
        Rc::new(Expr {
            kind: ExprKind::Number(n),
        })
    }

    pub fn apply(left: &Rc<Expr>, right: &Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr {
            kind: ExprKind::Apply(left.clone(), right.clone()),
        })
    }

    pub fn codata(clauses: Vec<Clause>) -> Rc<Expr> {
        Rc::new(Expr {
            kind: ExprKind::Codata(clauses),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Variable(Ident),
    Number(i64),
    Apply(Rc<Expr>, Rc<Expr>),
    Codata(Vec<Clause>),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ExprKind::Variable(ident) => write!(f, "{}", ident.name),
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Apply(left, right) => write!(f, "({} {})", left, right),
            ExprKind::Codata(clauses) => {
                write!(f, "{{ ")?;
                write!(f, "{}", clauses[0])?;
                for clause in clauses[1..].iter() {
                    write!(f, ", {}", clause)?;
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub(crate) name: String,
}

#[derive(Debug, Clone)]
pub struct Clause {
    pub pattern: Pat,
    pub body: Expr,
}

impl Clause {
    pub fn new(pattern: &Pat, body: &Expr) -> Clause {
        Clause {
            pattern: pattern.clone(),
            body: body.clone(),
        }
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.body)
    }
}

#[derive(Debug, Clone)]
pub struct Pat {
    pub kind: PatKind,
}

impl Pat {
    pub fn sequence(patterns: Vec<Pat>) -> Pat {
        Pat {
            kind: PatKind::Sequence(patterns),
        }
    }

    pub fn this() -> Pat {
        Pat {
            kind: PatKind::This,
        }
    }

    pub fn variable(name: &str) -> Pat {
        Pat {
            kind: PatKind::Variable(Ident {
                name: name.to_string(),
            }),
        }
    }

    pub fn label(name: &str) -> Pat {
        Pat {
            kind: PatKind::Label(Ident {
                name: name.to_string(),
            }),
        }
    }

    pub fn number(n: i64) -> Pat {
        Pat {
            kind: PatKind::Number(n),
        }
    }

    /// Returns true if the pattern is a function pattern.
    /// A function pattern is a pattern that starts with a This (#) pattern.
    pub fn is_function(&self) -> bool {
        match &self.kind {
            PatKind::Sequence(patterns) => matches!(
                patterns.get(0),
                Some(Pat {
                    kind: PatKind::This,
                })
            ),
            _ => false,
        }
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum PatKind {
    Sequence(Vec<Pat>),
    This,
    Variable(Ident),
    Label(Ident),
    Number(i64),
}

impl Display for PatKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PatKind::Sequence(patterns) if !patterns.is_empty() => {
                write!(f, "(")?;
                write!(f, "{}", patterns[0])?;
                for pattern in &patterns[1..] {
                    write!(f, " {}", pattern)?;
                }
                write!(f, ")")
            }
            PatKind::Sequence(_) => fmt::Result::Ok(()),
            PatKind::This => write!(f, "#"),
            PatKind::Variable(ident) => write!(f, "{}", ident.name),
            PatKind::Label(ident) => write!(f, "{}", ident.name),
            PatKind::Number(n) => write!(f, "{}", n),
        }
    }
}
