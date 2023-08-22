use log::debug;

use crate::syntax::{Clause, Expr, ExprKind, Ident, Pat, PatKind};
use std::{collections::HashMap, fmt::Display, rc::Rc};

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

fn build_branch(clause: &Clause) -> Branch {
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

fn split_patterns(ps: &[Pat]) -> Vec<Pat> {
    let mut patterns = vec![];
    let mut current = vec![];
    for p in ps {
        match &p.kind {
            PatKind::Sequence(ps2) => {
                if !current.is_empty() {
                    current.push(Pat::this(p.range));
                    patterns.push(Pat::sequence(current, p.range));
                    current = vec![];
                }
                patterns.extend(split_patterns(ps2));
            }
            _ => current.push(p.clone()),
        }
    }
    if !current.is_empty() {
        patterns.push(Pat::sequence(current, ps.last().unwrap().range));
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
                Expr::codata(vec![Clause::new(last_pattern, &branch.body)], context.range),
            );
        } else {
            new_context = merge_context(
                new_context,
                Expr::codata(
                    vec![Clause::new(
                        last_pattern,
                        &Expr::hole(vec![], last_pattern.range),
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
            &apply_context(e1, arg),
            &apply_context(e2, arg),
            context.range,
        ),
        ExprKind::Codata(clauses) => {
            let clauses = clauses
                .iter()
                .map(|c| Clause::new(&c.pattern, &apply_context(&c.body, arg)))
                .collect();
            Expr::codata(clauses, context.range)
        }
        ExprKind::Function(params, body) => {
            Expr::function(params.clone(), &apply_context(body, arg), context.range)
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
        ExprKind::Let(ds, body) => {
            let ds = ds
                .iter()
                .map(|(x, e)| (x.clone(), apply_context(e, arg)))
                .collect();
            Expr::let_(ds, &apply_context(body, arg), context.range)
        }
        _ => context.clone(),
    }
}

/// Preprocess an expression
pub fn flatten(expr: &Expr) -> Expr {
    use ExprKind::*;

    match &expr.kind {
        Codata(clauses) => {
            let mut branches: Vec<Branch> = clauses.iter().map(build_branch).collect();
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
                        .join(" ")
                );
                debug!("contexts: {}", contexts);
                (branches, contexts) = pop_last(&branches, &contexts);
                if branches.is_empty() {
                    return contexts;
                }
            }
        }
        Apply(e1, e2) => {
            let e1 = flatten(e1);
            let e2 = flatten(e2);
            Expr::apply(&e1, &e2, expr.range)
        }
        // Function, Object, Case includes Expr, but they are already flattened.
        _ => expr.clone(),
    }
}

/// Environment
type VarEnv = HashMap<Ident, Value>;

pub struct Evaluator {
    uniq_supply: i64,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator { uniq_supply: 0 }
    }

    pub fn gensym(&mut self, hint: &str) -> Ident {
        let uniq = self.uniq_supply;
        self.uniq_supply += 1;
        Ident {
            name: format!("#{}_{}", hint, uniq),
        }
    }

    /// Evaluate an expression
    pub fn eval(&mut self, env: Rc<VarEnv>, expr: &Expr) -> Value {
        use ExprKind::*;
        match &expr.kind {
            Variable(x) => env
                .get(x)
                .cloned()
                .unwrap_or_else(|| panic!("unbound variable: {}", x.name)),
            Label(x) => Value::accessor(x.clone()),
            Number(n) => Value::number(*n),
            Apply(f, x) => {
                let f = self.eval(env.clone(), f);
                let x = self.eval(env.clone(), x);
                self.apply(f, x)
            }
            Codata(clauses) if clauses.len() == 1 => {
                self.eval_clause(env, clauses.first().unwrap())
            }
            Codata(clauses) => {
                let object: Option<Value> = clauses
                    .iter()
                    .map(|c| self.eval_clause(env.clone(), c))
                    .reduce(|a, b| {
                        let mut map = HashMap::new();
                        match (a.kind, b.kind) {
                            (ValueKind::Object(a), ValueKind::Object(b)) => {
                                map.extend(a.map);
                                map.extend(b.map);
                            }
                            _ => panic!("codata must be flattened"),
                        }
                        Value::object(env.clone(), map)
                    });
                object.unwrap()
            }
            _ => panic!("cannot evaluate: {}", expr),
        }
    }

    /// Turn a clause into a function.
    /// Restriction: the clause must have a sequence of patterns that start with a `#` and other patterns must be variables.
    fn eval_clause(&self, env: Rc<VarEnv>, clause: &Clause) -> Value {
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
                        if this.kind == PatKind::This
                            && args.iter().all(|p| p.kind.is_variable()) =>
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
                    _ => panic!("nested pattern: {}", clause.pattern),
                }
            }
            _ => panic!("invalid pattern: {}", clause.pattern),
        }
    }

    fn apply(&mut self, f: Value, x: Value) -> Value {
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
                    self.eval(Rc::new(env), &body)
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
                        self.eval(captures, body)
                    } else {
                        panic!("no such label: {}", label.name)
                    }
                }
                _ => panic!("cannot apply accessor to non-object: {:?}", x),
            },
            _ => panic!("cannot apply non-function: {:?}", f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::flatten,
        parser::{
            lexer::{remove_whitespace, tokenize},
            parse,
        },
    };
    use log::debug;
    use std::{collections::HashMap, rc::Rc};

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_simple() {
        eval_test("{ # x -> x } 1", super::Value::number(1));
    }

    #[test]
    fn test_record() {
        eval_test(".get { .get # -> 1 }", super::Value::number(1));
    }

    #[test]
    fn test_multi_args() {
        eval_test("{ # x y -> y } 1 2", super::Value::number(2));
    }

    #[test]
    fn test_flattened() {
        eval_test(
            ".get ({ # x y -> { .get # -> x } } 1 2)",
            super::Value::number(1),
        );
    }

    #[test]
    fn test_flattened2() {
        eval_test(
            ".get ({ # x -> { .get (# y) -> x } } 1 2)",
            super::Value::number(1),
        );
    }

    #[test]
    fn test_complex() {
        eval_test(".get ({ .get (# x y) -> x } 1 2)", super::Value::number(1));
    }

    #[test]
    fn test_nest_field() {
        eval_test(
            ".head (.tail { .head # -> 1, .head (.tail #) -> 2 })",
            super::Value::number(2),
        );
    }

    fn eval_test(src: &str, expected: super::Value) {
        init();
        let tokens = tokenize(src).unwrap();
        let ast = parse(remove_whitespace(&tokens)).unwrap();
        let mut evaluator = super::Evaluator::new();
        let ast = flatten(&ast);
        debug!("{}", ast);
        let value = evaluator.eval(Rc::new(HashMap::new()), &ast);
        assert_eq!(value, expected);
    }
}
