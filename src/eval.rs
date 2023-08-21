use crate::syntax::{Clause, Expr, ExprKind, Ident, Pat, PatKind};
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

    pub fn function(captures: Rc<VarEnv>, args: Vec<Ident>, body: Expr) -> Value {
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
    pub captures: Rc<VarEnv>,
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
                self.apply(env, f, x)
            }
            Codata(clauses) if clauses.len() == 1 => {
                self.to_function(env, clauses.first().unwrap())
            }
            Codata(clauses) => {
                todo!("not implemented: codata with {} clauses", clauses.len())
            }
        }
    }

    /// Turn a clause into a function.
    /// Restriction: the clause must have a sequence of patterns that start with a `#` and other patterns must be variables.
    fn to_function(&self, env: Rc<VarEnv>, clause: &Clause) -> Value {
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
                    fn get_name(p: &Pat) -> Ident {
                        match &p.kind {
                            PatKind::Variable(x) => x.clone(),
                            _ => unreachable!(),
                        }
                    }

                    let args: Vec<Ident> = ps[1..].iter().map(get_name).collect();
                    Value::function(env.clone(), args, body.clone())
                } else {
                    panic!("cannot convert clause to function: {}", clause)
                }
            }
            _ => panic!("cannot convert clause to function: {}", clause),
        }
    }

    fn apply(&mut self, env: Rc<VarEnv>, f: Value, x: Value) -> Value {
        match f {
            Value {
                kind:
                    ValueKind::Function(Function {
                        captures,
                        args,
                        body,
                    }),
            } => {
                if args.len() != 1 {
                    todo!(
                        "not implemented: partial application of function with {} arguments",
                        args.len()
                    )
                }
                let mut env = (*env).clone();
                env.extend(captures.iter().map(|(k, v)| (k.clone(), v.clone())));
                env.insert(args[0].clone(), x);
                self.eval(Rc::new(env), &body)
            }
            _ => panic!("cannot apply non-function: {:?}", f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{
        lexer::{remove_whitespace, tokenize},
        parse,
    };
    use std::{collections::HashMap, rc::Rc};

    #[test]
    fn test_simple() {
        let src = "{ # x -> x } 1";
        let tokens = tokenize(src).unwrap();
        let ast = parse(remove_whitespace(&tokens)).unwrap();
        let mut evaluator = super::Evaluator::new();
        let value = evaluator.eval(Rc::new(HashMap::new()), &ast);
        assert_eq!(value, super::Value::number(1));
    }

    #[test]
    fn test_record() {
        let src = ".get { .get # -> 1 }";
        let tokens = tokenize(src).unwrap();
        let ast = parse(remove_whitespace(&tokens)).unwrap();
        let mut evaluator = super::Evaluator::new();
        let value = evaluator.eval(Rc::new(HashMap::new()), &ast);
        assert_eq!(value, super::Value::number(1));
    }

    #[test]
    fn test_multi_args() {
        let src = "{ # x y -> x } 1 2";
        let tokens = tokenize(src).unwrap();
        let ast = parse(remove_whitespace(&tokens)).unwrap();
        let mut evaluator = super::Evaluator::new();
        let value = evaluator.eval(Rc::new(HashMap::new()), &ast);
        assert_eq!(value, super::Value::number(1));
    }

    #[test]
    fn test_complex() {
        let src = ".get ({ .get (# x y) -> x } 1 2)";
        let tokens = tokenize(src).unwrap();
        let ast = parse(remove_whitespace(&tokens)).unwrap();
        let mut evaluator = super::Evaluator::new();
        let value = evaluator.eval(Rc::new(HashMap::new()), &ast);
        assert_eq!(value, super::Value::number(1));
    }
}
