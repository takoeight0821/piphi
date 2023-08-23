use crate::syntax::{Clause, Expr, ExprKind, Ident, Pat, PatKind};
use log::debug;
use std::{collections::HashMap, fmt::Display, rc::Rc};

#[cfg(test)]
mod tests;

/// Value
#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    pub kind: ValueKind,
}

impl Value {
    pub fn number(n: i64) -> Value {
        Value {
            kind: ValueKind::Number(n),
        }
    }

    pub fn function(captures: Rc<VarEnv>, args: Vec<Ident>, body: Expr) -> Value {
        Value {
            kind: ValueKind::Function(Function {
                captures,
                args,
                body,
            }),
        }
    }

    pub fn object(captures: Rc<VarEnv>, map: HashMap<Ident, Expr>) -> Value {
        Value {
            kind: ValueKind::Object(Object { captures, map }),
        }
    }

    pub fn accessor(x: Ident) -> Value {
        Value {
            kind: ValueKind::Accessor(x),
        }
    }

    pub fn primitive(name: &str) -> Value {
        Value {
            kind: ValueKind::Primitive(name.to_owned(), vec![]),
        }
    }

    /// Partially apply a primitive function
    pub fn partial_primitive(name: &str, args: &[Value], new_args: Vec<Value>) -> Value {
        let mut args = args.to_vec();
        args.extend(new_args);
        Value {
            kind: ValueKind::Primitive(name.to_owned(), args),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ValueKind::Number(n) => write!(f, "{}", n),
            ValueKind::Function(Function {
                captures,
                args,
                body,
            }) => {
                write!(
                    f,
                    "{{ {} -> {} | {} }}",
                    args.iter()
                        .map(|x| x.name.to_owned())
                        .collect::<Vec<_>>()
                        .join(" "),
                    body,
                    captures
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k.name, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ValueKind::Object(Object { captures, map }) => {
                write!(
                    f,
                    "{{ {} | {} }}",
                    map.iter()
                        .map(|(k, v)| format!("{}: {}", k.name, v))
                        .collect::<Vec<_>>()
                        .join(", "),
                    captures
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k.name, v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ValueKind::Accessor(x) => write!(f, "{}", x.name),
            ValueKind::Primitive(name, args) => write!(
                f,
                "{} {}",
                name,
                args.iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    /// A number
    Number(i64),
    /// A function
    Function(Function),
    /// A map
    /// TODO: Lazy evaluation
    Object(Object),
    /// An accessor
    Accessor(Ident),
    Primitive(String, Vec<Value>),
}

/// Function
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub captures: Rc<VarEnv>,
    pub args: Vec<Ident>,
    pub body: Expr,
}

/// Object
#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub captures: Rc<VarEnv>,
    pub map: HashMap<Ident, Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch {
    pub patterns: Vec<Pat>,
    // pub guards: Vec<Pat>,
    pub body: Expr,
}

impl Display for Branch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let patterns = self
            .patterns
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{} -> {}", patterns, self.body)
    }
}

fn build_branch(clause: Clause) -> Branch {
    match clause {
        Clause {
            pattern:
                Pat {
                    kind: PatKind::Sequence(ps),
                    ..
                },
            body,
        } => {
            let patterns = split_patterns(ps);
            Branch {
                patterns,
                body: flatten(body),
            }
        }
        _ => panic!("invalid pattern: {}", clause.pattern),
    }
}

fn split_patterns(ps: Vec<Pat>) -> Vec<Pat> {
    let mut patterns = vec![];
    let mut current = vec![];
    let mut ps = ps.into_iter().peekable();
    while let Some(p) = ps.next() {
        match p.kind {
            PatKind::Sequence(ps2) => {
                if !current.is_empty() {
                    current.push(Pat::this(p.range));
                    patterns.push(Pat::sequence(&current, p.range));
                    current = vec![];
                }
                patterns.extend(split_patterns(ps2));
            }
            _ => current.push(p.clone()),
        }
        if ps.peek().is_none() && !current.is_empty() {
            patterns.push(Pat::sequence(&current, p.range));
        }
    }
    patterns
}

fn pop_last(branches: &[Branch], context: &Expr) -> (Vec<Branch>, Expr) {
    let mut new_branches = vec![];
    let mut new_context = Expr::hole(vec![], context.range);
    for branch in branches {
        let last_pattern = branch.patterns.last().unwrap();
        let rest_patterns = &branch.patterns[..branch.patterns.len() - 1];
        debug!("last: {}", last_pattern);
        debug!(
            "rest: {}",
            rest_patterns
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(" | ")
        );
        if rest_patterns.is_empty() {
            new_context = merge_context(
                new_context,
                Expr::codata(
                    vec![Clause::new(last_pattern.to_owned(), branch.body.to_owned())],
                    context.range,
                ),
            );
        } else {
            new_context = merge_context(
                new_context,
                Expr::codata(
                    vec![Clause::new(
                        last_pattern.to_owned(),
                        Expr::hole(vec![], last_pattern.range),
                    )],
                    context.range,
                ),
            );
            new_branches.push(Branch {
                patterns: rest_patterns.to_vec(),
                body: branch.body.clone(),
            });
        }
    }
    (new_branches, apply_context(context, &new_context))
}

fn merge_context(left: Expr, right: Expr) -> Expr {
    if matches!(
        left,
        Expr {
            kind: ExprKind::Hole(_),
            ..
        }
    ) {
        right
    } else {
        match (left.kind, &right.kind) {
            (ExprKind::Codata(mut left_clauses), ExprKind::Codata(right_clauses)) => {
                if right_clauses.iter().any(|c| c.always_match()) {
                    return right;
                }
                left_clauses.extend(right_clauses.iter().cloned());
                Expr::codata(left_clauses, left.range)
            }
            _ => panic!("cannot merge context: {}", right),
        }
    }
}

fn apply_context(context: &Expr, arg: &Expr) -> Expr {
    match &context.kind {
        ExprKind::Hole(ps) if ps.is_empty() => arg.clone(),
        ExprKind::Hole(_) => todo!("guard"),
        ExprKind::Apply(e1, e2) => Expr::apply(
            apply_context(e1, arg),
            apply_context(e2, arg),
            context.range,
        ),
        ExprKind::Codata(clauses) => {
            let clauses = clauses
                .iter()
                .map(|c| Clause::new(c.pattern.to_owned(), apply_context(&c.body, arg)))
                .collect();
            Expr::codata(clauses, context.range)
        }
        ExprKind::Function(params, body) => {
            Expr::function(params.clone(), apply_context(body, arg), context.range)
        }
        ExprKind::Object(map) => {
            let map = map
                .iter()
                .map(|(k, v)| (k.clone(), apply_context(v, arg)))
                .collect();
            Expr::object(map, context.range)
        }
        ExprKind::Case(ss, branches) => {
            let branches = branches
                .iter()
                .map(|(ps, body)| (ps.clone(), apply_context(body, arg)))
                .collect();
            Expr::case(ss.clone(), branches, context.range)
        }
        ExprKind::Let(var, value, body) => Expr::let_(
            &var.name,
            apply_context(value, arg),
            apply_context(body, arg),
            context.range,
        ),
        _ => context.clone(),
    }
}

/// Preprocess an expression
pub fn flatten(expr: Expr) -> Expr {
    use ExprKind::*;

    match expr.kind {
        Codata(clauses) => {
            let mut branches: Vec<Branch> = clauses.into_iter().map(build_branch).collect();
            let mut contexts = Expr {
                kind: ExprKind::Hole(vec![]),
                range: expr.range,
            };

            loop {
                debug!(
                    "branches: {}",
                    branches
                        .iter()
                        .map(|b| format!("{}", b))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                debug!("contexts: {}", contexts);
                (branches, contexts) = pop_last(&branches, &contexts);
                if branches.is_empty() {
                    return contexts;
                }
            }
        }
        Apply(e1, e2) => {
            let e1 = flatten(*e1);
            let e2 = flatten(*e2);
            Expr::apply(e1, e2, expr.range)
        }
        Let(name, value, body) => {
            let value = flatten(*value);
            let body = flatten(*body);
            Expr::let_(&name.name, value, body, expr.range)
        }
        Fix(name, value) => {
            let value = flatten(*value);
            Expr::fix(&name.name, value, expr.range)
        }
        // Function, Object, Case includes Expr, but they are already flattened.
        _ => expr.clone(),
    }
}

/// Environment
type VarEnv = HashMap<Ident, Value>;

pub fn new_env() -> Rc<VarEnv> {
    let mut env = HashMap::new();
    env.insert(Ident::new("add"), Value::primitive("add"));
    Rc::new(env)
}

fn lookup(env: Rc<VarEnv>, x: Ident) -> Value {
    match env.get(&x) {
        // Force a delayed value (used in `fix`)
        Some(Value {
            kind:
                ValueKind::Function(Function {
                    captures: _,
                    args,
                    body,
                }),
        }) if args.is_empty() => eval(env.clone(), body.clone()),
        Some(x) => x.clone(),
        None => panic!("unbound variable: {}", x.name),
    }
}

/// Evaluate an expression
pub fn eval(env: Rc<VarEnv>, expr: Expr) -> Value {
    use ExprKind::*;
    match expr.kind {
        Variable(x) => lookup(env, x),
        Label(x) => Value::accessor(x.clone()),
        Number(n) => Value::number(n),
        Apply(f, x) => {
            let f = eval(env.clone(), *f);
            let x = eval(env.clone(), *x);
            apply(f, x)
        }
        Codata(mut clauses) if clauses.len() == 1 => eval_clause(env, clauses.remove(0)),
        Codata(clauses) => {
            let error = format!(
                "codata must be flattened: {}",
                clauses
                    .iter()
                    .map(|c| format!("{}", c))
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            let object: Option<Value> = clauses
                .into_iter()
                .map(|c| eval_clause(env.clone(), c))
                .reduce(|a, b| {
                    let mut map = HashMap::new();
                    match (a.kind, b.kind) {
                        (ValueKind::Object(a), ValueKind::Object(b)) => {
                            map.extend(a.map);
                            map.extend(b.map);
                        }
                        _ => panic!("{}", error),
                    }
                    Value::object(env.clone(), map)
                });
            object.unwrap()
        }
        Let(name, value, body) => {
            let value = eval(env.clone(), *value);
            let mut env = (*env).clone();
            env.insert(name.clone(), value);
            eval(Rc::new(env), *body)
        }
        Fix(name, value) => {
            let delayed = Value::function(Rc::new(HashMap::new()), vec![], *value.clone());
            let mut env = (*env).clone();
            env.insert(name.clone(), delayed);
            eval(Rc::new(env), *value)
        }

        _ => panic!("cannot evaluate: {}", expr),
    }
}

/// Turn a clause into a function.
/// Restriction: the clause must have a sequence of patterns that start with a `#` and other patterns must be variables.
fn eval_clause(env: Rc<VarEnv>, clause: Clause) -> Value {
    match clause {
        Clause {
            pattern:
                Pat {
                    kind: PatKind::Sequence(ps),
                    ..
                },
            body,
        } => {
            match &ps[..] {
                [this, ref args @ ..]
                    if this.kind == PatKind::This && args.iter().all(|p| p.kind.is_variable()) =>
                {
                    // Function
                    fn get_name(p: &Pat) -> Ident {
                        match &p.kind {
                            PatKind::Variable(x) => x.clone(),
                            _ => unreachable!(),
                        }
                    }

                    let args = args.iter().map(get_name).collect();
                    Value::function(env.clone(), args, body.clone())
                }
                [Pat {
                    kind: PatKind::Label(label),
                    ..
                }, Pat {
                    kind: PatKind::This,
                    ..
                }] => {
                    let mut map = HashMap::new();
                    map.insert(label.clone(), body.clone());
                    Value::object(env.clone(), map)
                }
                _ => panic!(
                    "nested pattern: {}",
                    ps.iter()
                        .map(|p| format!("{}", p))
                        .collect::<Vec<_>>()
                        .join(" ")
                ),
            }
        }
        _ => panic!("invalid pattern: {}", clause.pattern),
    }
}

fn apply(f: Value, x: Value) -> Value {
    match f {
        Value {
            kind:
                ValueKind::Function(Function {
                    captures,
                    args,
                    body,
                }),
        } => {
            if args.len() == 1 {
                let mut env = (*captures).clone();
                env.insert(args[0].clone(), x);
                eval(Rc::new(env), body)
            } else {
                let mut env = (*captures).clone();
                env.insert(args[0].clone(), x);
                Value::function(Rc::new(env), args[1..].to_vec(), body)
            }
        }
        Value {
            kind: ValueKind::Accessor(label),
        } => match x {
            Value {
                kind: ValueKind::Object(Object { captures, map }),
            } => {
                if let Some(body) = map.get(&label) {
                    eval(captures, body.to_owned())
                } else {
                    panic!("no such label: {}", label.name)
                }
            }
            _ => panic!("cannot apply accessor to non-object: {}", x),
        },
        Value {
            kind: ValueKind::Primitive(name, args),
        } => apply_primitive(name.as_str(), args, x),
        _ => panic!("cannot apply non-function: {}", f),
    }
}

fn apply_primitive(name: &str, args: Vec<Value>, x: Value) -> Value {
    match name {
        "add" => {
            if args.is_empty() {
                Value::partial_primitive(name, &args, vec![x])
            } else if args.len() == 1 {
                match (&args[0].kind, &x.kind) {
                    (ValueKind::Number(a), ValueKind::Number(b)) => Value::number(a + b),
                    _ => panic!("cannot add: {} {}", args[0], x),
                }
            } else {
                panic!("too many arguments: {}", args.len())
            }
        }
        _ => panic!("unknown primitive: {}", name),
    }
}
