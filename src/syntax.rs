use crate::parser::range::Range;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub range: Range,
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Expr {
    pub fn variable(name: &str, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Variable(Ident {
                name: name.to_string(),
            }),
            range,
        }
    }

    pub fn label(name: &str, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Label(Ident {
                name: name.to_string(),
            }),
            range,
        }
    }

    pub fn number(n: i64, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Number(n),
            range,
        }
    }

    pub fn apply(left: &Expr, right: &Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Apply(Box::new(left.clone()), Box::new(right.clone())),
            range,
        }
    }

    pub fn codata(clauses: Vec<Clause>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Codata(clauses),
            range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Variable(Ident),
    Label(Ident),
    Number(i64),
    Apply(Box<Expr>, Box<Expr>),
    Codata(Vec<Clause>),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ExprKind::Variable(ident) => write!(f, "{}", ident.name),
            ExprKind::Label(ident) => write!(f, ".{}", ident.name),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Pat {
    pub kind: PatKind,
    pub range: Range,
}

impl Pat {
    pub fn sequence(patterns: Vec<Pat>, range: Range) -> Pat {
        Pat {
            kind: PatKind::Sequence(patterns),
            range,
        }
    }

    pub fn this(range: Range) -> Pat {
        Pat {
            kind: PatKind::This,
            range,
        }
    }

    pub fn variable(name: &str, range: Range) -> Pat {
        Pat {
            kind: PatKind::Variable(Ident {
                name: name.to_string(),
            }),
            range,
        }
    }

    pub fn label(name: &str, range: Range) -> Pat {
        Pat {
            kind: PatKind::Label(Ident {
                name: name.to_string(),
            }),
            range,
        }
    }

    pub fn number(n: i64, range: Range) -> Pat {
        Pat {
            kind: PatKind::Number(n),
            range,
        }
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq)]
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
            PatKind::Label(ident) => write!(f, ".{}", ident.name),
            PatKind::Number(n) => write!(f, "{}", n),
        }
    }
}

impl PatKind {
    pub fn is_variable(&self) -> bool {
        match self {
            PatKind::Variable(_) => true,
            _ => false,
        }
    }
}
