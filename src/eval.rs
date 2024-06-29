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

pub fn eval(expr: &Expr, env: &mut Environment) -> Result<Expr, Expr> {
    match expr {
        Expr::Nop => Ok(Expr::Int(0)),
        Expr::Bool(b) => Ok(Expr::Bool(*b)),
        Expr::Int(n) => Ok(Expr::Int(*n)),
        Expr::Float(f) => Ok(Expr::Float(*f)),
        Expr::Str(s) => Ok(Expr::Str(s.clone())),
        Expr::Object(list) => {
            let mut evaluated_list = Vec::new();
            for (key, value) in list {
                let evaluated_value = eval(value, env)?;
                evaluated_list.push((key.clone(), Box::new(evaluated_value)));
            }
            Ok(Expr::Object(evaluated_list))
        }
        Expr::Array(list) => {
            let evaluated_list = eval_array_contents(list, env)?;
            Ok(Expr::Array(evaluated_list))
        }
        Expr::Identifier(s) => match env.get(s) {
            Some(v) => Ok(v.clone()),
            None => panic!("Variable {} not found", s),
        },
        Expr::ArrayIndex { name, index } => {
            let obj = env.get(name).expect("Object not found").clone();
            match obj {
                Expr::Array(list) => {
                    if let Expr::Int(i) = eval(index, env)? {
                        Ok(list[i as usize].clone())
                    } else {
                        panic!("Index must be a number");
                    }
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
                            return Ok(*value.clone());
                        }
                    }
                    panic!("Member {} not found in object {}", member, name);
                }
                Expr::Array(list) => {
                    if member == "length" {
                        Ok(Expr::Int(list.len() as i64))
                    } else {
                        panic!("Index must be a number");
                    }
                }
                _ => panic!("{} is not an object", name),
            }
        }
        Expr::Op { op, lhs, rhs } => {
            let l = eval(lhs, env)?;
            let r = eval(rhs, env)?;

            Ok(match (l, r) {
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
            })
        }
        Expr::Comparison { op, lhs, rhs } => {
            let l = eval(lhs, env)?;
            let r = eval(rhs, env)?;

            Ok(match (l, r) {
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
            })
        }
        Expr::Conditional {
            cond,
            then_block,
            else_block,
        } => {
            let condition = eval(cond, env)?;
            match condition {
                Expr::Bool(b) => {
                    if b {
                        for then_expr in then_block {
                            match eval(then_expr, env) {
                                Ok(_) => continue,
                                Err(return_expr) => return Err(return_expr),
                            }
                        }
                    } else {
                        for else_expr in else_block {
                            match eval(else_expr, env) {
                                Ok(_) => continue,
                                Err(return_expr) => return Err(return_expr),
                            }
                        }
                    }
                    Ok(Expr::Str("".to_string()))
                }
                _ => panic!("Condition must be a boolean"),
            }
        }
        Expr::Variable { name, value } => {
            let value = eval(value, env)?;

            match value {
                Expr::Int(_)
                | Expr::Float(_)
                | Expr::Str(_)
                | Expr::Bool(_)
                | Expr::Object(_)
                | Expr::Array(_) => {
                    env.set(name.clone(), value.clone());
                    Ok(value)
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::SetVariable { name, value } => {
            let value = eval(value, env)?;

            if env.get(name).is_none() {
                panic!("Variable {} not found", name);
            }

            match value {
                Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) | Expr::Object(_) | Expr::Array(_) => {
                    env.set(name.clone(), value.clone());
                    Ok(value)
                }
                _ => panic!("Invalid value for variable {}", name),
            }
        }
        Expr::FunctionCall { name, args } => {
            let mut results = Vec::new();
            for arg in args {
                results.push(eval(arg, env)?);
            }

            match name.as_str() {
                "print" => Ok(std_functions::std_print(results)),
                "sqrt" => Ok(std_functions::std_sqrt(results)),
                "free" => Ok(std_functions::std_free(args, env)),
                "panic" => Ok(std_functions::std_panic(results)),
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
                            match eval(expr, &mut new_env) {
                                Ok(_) => continue,
                                Err(return_expr) => return Ok(return_expr),
                            }
                        }

                        Ok(Expr::Str("".to_string()))
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
            Ok(Expr::Str("".to_string()))
        }
        Expr::Return(e) => {
            let evaluated = eval(e, env)?;
            Err(evaluated)
        }
        Expr::While { cond, block } => {
            let mut result = Ok(Expr::Str("".to_string()));
            while eval(cond, env)? == Expr::Bool(true) {
                for expr in block {
                    match eval(expr, env) {
                        Ok(expr) => result = Ok(expr),
                        Err(return_expr) => return Err(return_expr),
                    }
                }
            }
            result
        }
        Expr::For {
            init,
            cond,
            step,
            block,
        } => {
            let mut result = Ok(Expr::Str("".to_string()));
            eval(init, env)?;

            while eval(cond, env)? == Expr::Bool(true) {
                for expr in block {
                    match eval(expr, env) {
                        Ok(expr) => result = Ok(expr),
                        Err(return_expr) => {
                            if let Expr::Variable { name, value: _ } = init.as_ref() {
                                env.remove(name);
                            }
                            return Err(return_expr);
                        }
                    }
                }
                eval(step, env)?;
            }

            if let Expr::Variable { name, value: _ } = init.as_ref() {
                env.remove(name);
            }

            result
        }
    }
}

fn eval_array_contents(list: &[Expr], env: &mut Environment) -> Result<Vec<Expr>, Expr> {
    let mut evaluated_list = Vec::new();
    for item in list {
        let evaluated_item = eval(item, env)?;
        evaluated_list.push(evaluated_item);
    }
    Ok(evaluated_list)
}
