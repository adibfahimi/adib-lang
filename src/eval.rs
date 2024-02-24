use crate::parser::Expr;
use std::collections::HashMap;

fn std_print(args: Vec<Expr>) -> Expr {
    let mut output = String::new();
    for arg in args {
        match arg {
            Expr::Number(n) => output.push_str(&format!("{}", n)),
            Expr::Str(s) => output.push_str(&s),
            Expr::Bool(b) => output.push_str(&format!("{}", b)),
            _ => panic!("Invalid argument for print"),
        }
    }
    println!("{}", output);
    Expr::Str(output)
}

#[derive(Debug, Clone)]
pub struct Environment {
    mem: HashMap<String, Expr>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            mem: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Expr> {
        self.mem.get(key)
    }

    pub fn set(&mut self, key: String, value: Expr) {
        self.mem.insert(key, value);
    }
}

pub fn eval(expr: &Expr, env: &mut Environment) -> Expr {
    // println!("Evaluating {:?}", expr);
    match expr {
        Expr::Bool(b) => Expr::Bool(*b),
        Expr::Number(n) => Expr::Number(*n),
        Expr::Str(s) => Expr::Str(s.clone()),
        Expr::Identifier(s) => match env.get(s) {
            Some(v) => v.clone(),
            None => panic!("Variable {} not found", s),
        },
        Expr::Op(op, l, r) => {
            let l = eval(l, env);
            let r = eval(r, env);
            match (l, r) {
                (Expr::Number(l), Expr::Number(r)) => match op {
                    '+' => Expr::Number(l + r),
                    '-' => Expr::Number(l - r),
                    '*' => Expr::Number(l * r),
                    '/' => Expr::Number(l / r),
                    '>' => Expr::Bool(l > r),
                    '<' => Expr::Bool(l < r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Str(l), Expr::Str(r)) => match op {
                    '+' => Expr::Str(l + &r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Str(l), Expr::Number(r)) => match op {
                    '+' => Expr::Str(l + &r.to_string()),
                    _ => panic!("Invalid operator {}", op),
                },
                _ => panic!("Invalid operands for operator {}", op),
            }
        }
        Expr::Comparison(op, l, r) => {
            let l = eval(l, env);
            let r = eval(r, env);
            match (l, r) {
                (Expr::Number(l), Expr::Number(r)) => match op.as_str() {
                    "==" => Expr::Bool(l == r),
                    "!=" => Expr::Bool(l != r),
                    "<" => Expr::Bool(l < r),
                    ">" => Expr::Bool(l > r),
                    "<=" => Expr::Bool(l <= r),
                    ">=" => Expr::Bool(l >= r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Str(l), Expr::Str(r)) => match op.as_str() {
                    "==" => Expr::Bool(l == r),
                    "!=" => Expr::Bool(l != r),
                    "<" => Expr::Bool(l < r),
                    ">" => Expr::Bool(l > r),
                    "<=" => Expr::Bool(l <= r),
                    ">=" => Expr::Bool(l >= r),
                    _ => panic!("Invalid operator {}", op),
                },
                _ => panic!("Invalid operands for operator {}", op),
            }
        }
        Expr::Conditional(cond, then_block, else_block) => {
            let condition = eval(cond, env);
            match condition {
                Expr::Bool(b) => {
                    if b {
                        let mut result = None;
                        for then_expr in then_block {
                            result = Some(eval(then_expr, env));
                        }
                        result.expect("then_block cannot be empty")
                    } else {
                        for else_expr in else_block {
                            eval(else_expr, env);
                        }
                        Expr::Str("".to_string())
                    }
                }
                _ => panic!("Condition must be a boolean"),
            }
        }
        Expr::Var(name, value) => {
            let value = eval(value, env);

            match value {
                Expr::Number(_) | Expr::Str(_) | Expr::Bool(_) => {
                    env.set(name.clone(), value.clone());
                    value
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::SetVar(name, value) => {
            let value = eval(value, env);

            if env.get(name).is_none() {
                panic!("Variable {} not found", name);
            }

            match value {
                Expr::Number(_) | Expr::Str(_) | Expr::Bool(_) => {
                    env.set(name.clone(), value.clone());
                    value
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::FunctionCall(name, arguments) => {
            if name == "print" {
                let mut results = Vec::new();
                for arg in arguments {
                    results.push(eval(arg, env));
                }
                return std_print(results);
            }

            if name == "sqrt" {
                if arguments.len() != 1 {
                    panic!("sqrt expects exactly one argument");
                }
                let arg = eval(&arguments[0], env);
                if let Expr::Number(n) = arg {
                    return Expr::Number((n as f64).sqrt() as i64);
                }
            }

            let mut results = Vec::new();
            for arg in arguments {
                results.push(eval(arg, env));
            }
            match env.get(name) {
                Some(Expr::FunctionDef(_, params, body)) => {
                    let mut new_env = env.clone();
                    for (param, result) in params.iter().zip(results.iter()) {
                        new_env.set(param.to_string(), result.clone());
                    }
                    body.iter()
                        .map(|expr| eval(expr, &mut new_env))
                        .last()
                        .unwrap_or_else(|| Expr::Str("".to_string()))
                }
                _ => panic!("{} is not a function", name),
            }
        }
        Expr::FunctionDef(name, params, body) => {
            env.set(
                name.clone(),
                Expr::FunctionDef(name.clone(), params.clone(), body.clone()),
            );
            Expr::Str("".to_string())
        }
        Expr::Return(e) => eval(e, env),
        Expr::While(cond, block) => {
            let mut result = None;
            while eval(cond, env) == Expr::Bool(true) {
                for expr in block {
                    result = Some(eval(expr, env));
                }
            }

            result.unwrap_or_else(|| Expr::Str("".to_string()))
        }
        Expr::For(init, cond, step, block) => {
            let mut result = None;
            eval(init, env);

            while eval(cond, env) == Expr::Bool(true) {
                for expr in block {
                    result = Some(eval(expr, env));
                }
                eval(step, env);
            }

            result.unwrap_or_else(|| Expr::Str("".to_string()))
        }
    }
}
