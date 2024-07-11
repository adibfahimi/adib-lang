use crate::{lexer::Token, parser::Expr, std_functions};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    mem: HashMap<String, Expr>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn remove(&mut self, key: &str) {
        self.mem.remove(key);
    }
}

pub fn eval(expr: &Expr, env: &mut Environment) -> Expr {
    match expr {
        Expr::Nop => Expr::Int(0),
        Expr::Bool(b) => Expr::Bool(*b),
        Expr::Int(n) => Expr::Int(*n),
        Expr::Float(f) => Expr::Float(*f),
        Expr::Str(s) => Expr::Str(s.clone()),
        Expr::Object(list) => Expr::Object(list.clone()),
        Expr::Array(list) => Expr::Array(list.clone()),
        Expr::Identifier(s) => match env.get(s) {
            Some(v) => v.clone(),
            None => panic!("Variable {} not found", s),
        },
        Expr::ArrayIndex { name, index } => {
            let obj = env.get(name).expect("Object not found").clone();
            match obj {
                Expr::Array(list) => {
                    if let Expr::Int(i) = eval(index, env) {
                        return list[i as usize].clone();
                    }
                    panic!("Index must be a number");
                }
                _ => panic!("{} is not an array", name),
            }
        }
        Expr::Member { name, member } => {
            let obj = env.get(name).expect("Object not found").clone();
            match obj {
                Expr::Object(list) => {
                    for (key, value) in list {
                        if &key == member {
                            return *value.clone();
                        }
                    }

                    panic!("Member {} not found in object {}", member, name);
                }

                Expr::Array(list) => {
                    if member == "length" {
                        return Expr::Int(list.len() as i64);
                    }
                    panic!("Index must be a number");
                }

                _ => panic!("{} is not an object", name),
            }
        }
        Expr::Op { op, lhs, rhs } => {
            let l = eval(lhs, env);
            let r = eval(rhs, env);

            match (l, r) {
                (Expr::Int(l), Expr::Int(r)) => match op {
                    '+' => Expr::Int(l + r),
                    '-' => Expr::Int(l - r),
                    '*' => Expr::Int(l * r),
                    '/' => Expr::Int(l / r),
                    '>' => Expr::Bool(l > r),
                    '<' => Expr::Bool(l < r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Float(l), Expr::Float(r)) => match op {
                    '+' => Expr::Float(l + r),
                    '-' => Expr::Float(l - r),
                    '*' => Expr::Float(l * r),
                    '/' => Expr::Float(l / r),
                    '>' => Expr::Bool(l > r),
                    '<' => Expr::Bool(l < r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Int(l), Expr::Float(r)) => match op {
                    '+' => Expr::Float(l as f64 + r),
                    '-' => Expr::Float(l as f64 - r),
                    '*' => Expr::Float(l as f64 * r),
                    '/' => Expr::Float(l as f64 / r),
                    '>' => Expr::Bool(l as f64 > r),
                    '<' => Expr::Bool((l as f64) < r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Float(l), Expr::Int(r)) => match op {
                    '+' => Expr::Float(l + r as f64),
                    '-' => Expr::Float(l - r as f64),
                    '*' => Expr::Float(l * r as f64),
                    '/' => Expr::Float(l / r as f64),
                    '>' => Expr::Bool(l > r as f64),
                    '<' => Expr::Bool(l < r as f64),
                    _ => panic!("Invalid operator {}", op),
                },

                (Expr::Str(l), Expr::Str(r)) => match op {
                    '+' => Expr::Str(l + &r),
                    _ => panic!("Invalid operator {}", op),
                },
                (Expr::Str(l), Expr::Int(r)) => match op {
                    '+' => Expr::Str(l + &r.to_string()),
                    _ => panic!("Invalid operator {}", op),
                },
                _ => panic!("Invalid operands for operator {}", op),
            }
        }
        Expr::Comparison { op, lhs, rhs } => {
            let l = eval(lhs, env);
            let r = eval(rhs, env);

            match (l, r) {
                (Expr::Int(l), Expr::Int(r)) => match op.as_str() {
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
        Expr::Conditional {
            cond,
            then_block,
            else_block,
        } => {
            let condition = eval(cond, env);
            match condition {
                Expr::Bool(b) => {
                    if b {
                        for then_expr in then_block {
                            let res = eval(then_expr, env);
                            if let Expr::Return(_) = res {
                                return res;
                            }
                        }
                    } else {
                        for else_expr in else_block {
                            let res = eval(else_expr, env);
                            if let Expr::Return(_) = res {
                                return res;
                            }
                        }
                    }
                    Expr::Str("".to_string())
                }
                _ => panic!("Condition must be a boolean"),
            }
        }
        Expr::Variable { name, value } => {
            let value = eval(value, env);

            match value {
                Expr::Int(_)
                | Expr::Float(_)
                | Expr::Str(_)
                | Expr::Bool(_)
                | Expr::Object(_)
                | Expr::Array(_) => {
                    env.set(name.clone(), value.clone());
                    value
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::SetVariable { name, value } => {
            let value = eval(value, env);

            if env.get(name).is_none() {
                panic!("Variable {} not found", name);
            }

            match value {
                Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) | Expr::Object(_) | Expr::Array(_) => {
                    env.set(name.clone(), value.clone());
                    value
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::FunctionCall { name, args } => {
            let mut results = Vec::new();
            for arg in args {
                results.push(eval(arg, env));
            }

            match name.as_str() {
                "print" => std_functions::std_print(results),
                "sqrt" => std_functions::std_sqrt(results),
                "free" => std_functions::std_free(args, env),
                "panic" => std_functions::std_panic(results),
                _ => match env.get(name) {
                    Some(Expr::FunctionDef {
                        name: _,
                        args: argss,
                        body,
                    }) => {
                        let mut new_env = Environment::new();

                        for (arg, value) in argss.iter().zip(results.iter()) {
                            match arg {
                                Token::Identifier(name) => {
                                    new_env.set(name.clone(), value.clone());
                                }
                                _ => panic!("Invalid argument"),
                            }
                        }

                        for expr in body {
                            let res = eval(expr, &mut new_env);
                            if let Expr::Return(d) = res {
                                return eval(&d, &mut new_env);
                            }
                        }

                        Expr::Str("".to_string())
                    }
                    _ => panic!("{} is not a function", name),
                },
            }
        }
        Expr::FunctionDef { name, args, body } => {
            env.set(
                name.clone(),
                Expr::FunctionDef {
                    name: name.clone(),
                    args: args.clone(),
                    body: body.clone(),
                },
            );
            Expr::Str("".to_string())
        }
        Expr::Return(e) => Expr::Return(e.clone()),
        Expr::While { cond, block } => {
            let mut result = None;
            while eval(cond, env) == Expr::Bool(true) {
                for expr in block {
                    result = Some(eval(expr, env));
                }
            }

            result.unwrap_or_else(|| Expr::Str("".to_string()))
        }
        Expr::For {
            init,
            cond,
            step,
            block,
        } => {
            let mut result = None;
            eval(init, env);

            while eval(cond, env) == Expr::Bool(true) {
                for expr in block {
                    result = Some(eval(expr, env));
                }
                eval(step, env);
            }

            if let Expr::Variable { name, value: _ } = init.as_ref() {
                env.remove(name);
            }

            result.unwrap_or_else(|| Expr::Str("".to_string()))
        }
    }
}
