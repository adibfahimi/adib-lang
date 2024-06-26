use crate::{eval::Environment, parser::Expr};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
pub fn my_println(s: &str) {
    println!("{}", s);
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[cfg(target_arch = "wasm32")]
pub fn my_println(s: &str) {
    log(s);
}

pub fn std_print(args: Vec<Expr>) -> Expr {
    let mut output = String::new();
    for arg in args {
        match arg {
            Expr::Int(n) => output.push_str(&format!("{}", n)),
            Expr::Float(f) => output.push_str(&format!("{}", f)),
            Expr::Str(s) => output.push_str(&s),
            Expr::Bool(b) => output.push_str(&format!("{}", b)),
            Expr::Object(list) => {
                output.push('{');
                for (key, value) in list {
                    output.push_str(&format!("{:?}: {:?}, ", key, value));
                }
                output.push('}');
            }
            Expr::Array(list) => {
                output.push('[');
                for value in list {
                    output.push_str(&format!("{:?}, ", value));
                }
                output.push(']');
            }
            _ => panic!("Invalid argument for print"),
        }
    }
    my_println(output.as_str());

    Expr::Str(output)
}

pub fn std_sqrt(args: Vec<Expr>) -> Expr {
    if args.len() != 1 {
        panic!("sqrt expects exactly one argument");
    }
    let arg = &args[0];
    if let Expr::Int(n) = arg {
        return Expr::Int((*n as f64).sqrt() as i64);
    }
    panic!("Invalid argument for sqrt");
}

pub fn std_free(args: &[Expr], env: &mut Environment) -> Expr {
    if args.len() != 1 {
        panic!("free expects exactly one argument");
    }
    let arg = &args[0];

    match arg {
        Expr::Identifier(i) => {
            env.remove(i);
            Expr::Str("".to_string())
        }

        _ => panic!("Invalid argument for free"),
    }
}

pub fn std_panic(args: Vec<Expr>) -> Expr {
    if args.len() != 1 {
        panic!("panic expects exactly one argument");
    }
    let arg = &args[0];
    if let Expr::Str(s) = arg {
        println!("Panic: {}", s);
        std::process::exit(1);
    }
    panic!("Invalid argument for panic");
}
