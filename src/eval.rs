use crate::syntax::{Clause, Expr, ExprKind, Ident, Pat, PatKind};
use anyhow::Result;
use std::{collections::HashMap, rc::Rc};

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

    pub fn function(captures: HashMap<Ident, Value>, args: Vec<Ident>, body: Expr) -> Value {
        Value {
            kind: ValueKind::Function(Function {
                captures,
                args,
                body,
            }),
        }
    }

    pub fn map(map: HashMap<Ident, Value>) -> Value {
        Value {
            kind: ValueKind::Map(map),
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
    Map(HashMap<Ident, Value>),
    /// An accessor
    Accessor(Ident),
}

/// Function
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub captures: VarEnv,
    pub args: Vec<Ident>,
    pub body: Expr,
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
    pub fn eval(&mut self, env: Rc<VarEnv>, expr: &Expr) -> Result<Value> {
        use ExprKind::*;
        match &expr.kind {
            Variable(x) => env
                .get(x)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("unbound variable: {}", x.name)),
            Label(x) => Ok(Value::accessor(x.clone())),
            Number(n) => Ok(Value::number(*n)),
            Apply(f, x) => {
                let f = self.eval(env.clone(), f)?;
                let x = self.eval(env.clone(), x)?;
                self.apply(env, f, x)
            }
            Codata(clauses) if clauses.len() == 1 => {
                self.to_function(env, clauses.first().unwrap())
            }
            Codata(clauses) => {
                anyhow::bail!("not implemented: codata with {} clauses", clauses.len())
            }
        }
    }

    /// Turn a clause into a function.
    /// Restriction: the clause must have a sequence of patterns that start with a `#` and other patterns must be variables.
    fn to_function(&self, env: Rc<VarEnv>, clause: &Clause) -> Result<Value> {
        match clause {
            Clause {
                pattern:
                    Pat {
                        kind: PatKind::Sequence(ps),
                        ..
                    },
                body,
            } => {
                if ps.first().is_some_and(|p| p.kind == PatKind::This)
                    && ps[1..].iter().all(|p| p.kind.is_variable())
                {
                    todo!()
                } else {
                    anyhow::bail!("cannot convert clause to function: {}", clause)
                }
            }
            _ => anyhow::bail!("cannot convert clause to function: {}", clause),
        }
    }

    fn apply(&self, env: Rc<VarEnv>, f: Value, x: Value) -> Result<Value> {
        todo!()
    }
}
