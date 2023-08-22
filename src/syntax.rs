use crate::parser::range::Range;
use std::collections::HashMap;
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

    pub fn hole(params: Vec<Ident>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Hole(params),
            range,
        }
    }

    pub fn function(params: Vec<Ident>, arg: &Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Function(params, Box::new(arg.clone())),
            range,
        }
    }

    pub fn object(map: HashMap<Ident, Expr>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Object(map),
            range,
        }
    }

    pub fn case(ss: Vec<Ident>, branches: Vec<(Vec<Pat>, Expr)>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Case(ss, branches),
            range,
        }
    }

    pub fn let_(ds: Vec<(Ident, Expr)>, arg: &Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Let(ds, Box::new(arg.clone())),
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
    // Used for desugaring
    Function(Vec<Ident>, Box<Expr>),
    // Used for desugaring
    Object(HashMap<Ident, Expr>),
    Case(Vec<Ident>, Vec<(Vec<Pat>, Expr)>),
    Hole(Vec<Ident>),
    Let(Vec<(Ident, Expr)>, Box<Expr>),
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
            ExprKind::Function(params, body) => {
                write!(f, "λ(")?;
                write!(f, "{}", params[0].name)?;
                for param in params[1..].iter() {
                    write!(f, " {}", param.name)?;
                }
                write!(f, ").({})", body)
            }
            ExprKind::Object(map) => {
                write!(f, "{{ ")?;
                let (first_key, first_value) = map.iter().next().unwrap();
                write!(f, ".{}: {}", first_key.name, first_value)?;
                for (key, value) in map.iter().skip(1) {
                    write!(f, ", .{}: {}", key.name, value)?;
                }
                write!(f, " }}")
            }
            ExprKind::Case(ss, clauses) => {
                write!(f, "case ")?;
                write!(f, "{}", ss[0].name)?;
                for s in ss[1..].iter() {
                    write!(f, " {}", s.name)?;
                }
                write!(f, " {{ ")?;
                let (first_patterns, first_body) = clauses.iter().next().unwrap();
                for pattern in first_patterns.iter() {
                    write!(f, "{} ", pattern)?;
                }
                write!(f, "-> {}", first_body)?;

                for (patterns, body) in clauses.iter().skip(1) {
                    write!(f, ", ")?;
                    for pattern in patterns.iter() {
                        write!(f, "{} ", pattern)?;
                    }
                    write!(f, "-> {}", body)?;
                }
                write!(f, " }}")
            }
            ExprKind::Hole(args) => {
                write!(f, "([.]")?;
                if !args.is_empty() {
                    write!(f, "{}", args[0].name)?;
                    for arg in args[1..].iter() {
                        write!(f, " {}", arg.name)?;
                    }
                }
                write!(f, ")")
            }
            ExprKind::Let(binds, body) => {
                write!(f, "let ")?;
                let (first_ident, first_expr) = binds.iter().next().unwrap();
                write!(f, "{} = {}", first_ident.name, first_expr)?;
                for (ident, expr) in binds.iter().skip(1) {
                    write!(f, ", {} = {}", ident.name, expr)?;
                }
                write!(f, " in {}", body)
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

    pub fn empty() -> Pat {
        Pat {
            kind: PatKind::Empty,
            range: Default::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.kind, PatKind::Empty)
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
    Empty,
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
            PatKind::Empty => write!(f, "_"),
        }
    }
}

impl PatKind {
    pub fn is_variable(&self) -> bool {
        matches!(self, PatKind::Variable(_))
    }

    pub fn is_label(&self) -> bool {
        matches!(self, PatKind::Label(_))
    }
}
