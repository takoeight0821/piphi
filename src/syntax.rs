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
            kind: ExprKind::Variable(Ident::new(name)),
            range,
        }
    }

    pub fn label(name: &str, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Label(Ident::new(name)),
            range,
        }
    }

    pub fn number(n: i64, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Number(n),
            range,
        }
    }

    pub fn apply(left: Expr, right: Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Apply(Box::new(left), Box::new(right)),
            range,
        }
    }

    pub fn codata(clauses: Vec<Clause>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Codata(clauses),
            range,
        }
    }

    pub fn let_(name: &str, value: Expr, body: Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Let(
                Ident {
                    name: name.to_owned(),
                },
                Box::new(value),
                Box::new(body),
            ),
            range,
        }
    }

    pub fn hole(params: Vec<Ident>, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Hole(params),
            range,
        }
    }

    pub fn function(params: Vec<Ident>, arg: Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Function(params, Box::new(arg)),
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

    pub fn fix(name: &str, body: Expr, range: Range) -> Expr {
        Expr {
            kind: ExprKind::Fix(
                Ident {
                    name: name.to_owned(),
                },
                Box::new(body),
            ),
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
    Let(Ident, Box<Expr>, Box<Expr>),
    // Used for desugaring
    Function(Vec<Ident>, Box<Expr>),
    // Used for desugaring
    Object(HashMap<Ident, Expr>),
    // Used for desugaring
    Case(Vec<Ident>, Vec<(Vec<Pat>, Expr)>),
    // Used for desugaring
    Hole(Vec<Ident>),
    // Used for desugaring
    Fix(Ident, Box<Expr>),
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
            ExprKind::Let(var, value, body) => {
                write!(f, "let ")?;
                write!(f, "{} = {}", var.name, value)?;
                write!(f, " in {}", body)
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
            ExprKind::Fix(name, body) => {
                write!(f, "(fix {} in {})", name.name, body)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    pub fn new(arg: &str) -> Ident {
        Ident {
            name: arg.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Clause {
    pub pattern: Pat,
    pub body: Expr,
}

impl Clause {
    pub fn new(pattern: Pat, body: Expr) -> Clause {
        Clause { pattern, body }
    }

    pub fn always_match(&self) -> bool {
        self.pattern.always_match()
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
    pub fn sequence(patterns: &[Pat], range: Range) -> Pat {
        Pat {
            kind: PatKind::Sequence(patterns.to_owned()),
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

    fn always_match(&self) -> bool {
        match &self.kind {
            PatKind::Sequence(ps) => match &ps[..] {
                [this, args @ ..] => {
                    this.kind == PatKind::This && args.iter().all(|p| p.kind.is_variable())
                }
                _ => false,
            },
            _ => false,
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
        matches!(self, PatKind::Variable(_))
    }
}
