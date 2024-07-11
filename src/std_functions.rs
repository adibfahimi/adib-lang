use crate::parser::Expr;

pub fn std_print(args: Vec<Expr>) -> Expr {
    let mut output = String::new();
    for arg in args {
        match arg {
            Expr::Number(n) => output.push_str(&format!("{}", n)),
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
    println!("{}", output);
    Expr::Str(output)
}

pub fn std_sqrt(args: Vec<Expr>) -> Expr {
    if args.len() != 1 {
        panic!("sqrt expects exactly one argument");
    }
    let arg = &args[0];
    if let Expr::Number(n) = arg {
        return Expr::Number((*n as f64).sqrt() as i64);
    }
    panic!("Invalid argument for sqrt");
}

pub fn std_free(args: Vec<Expr>) -> Expr {
    if args.len() != 1 {
        panic!("free expects exactly one argument");
    }
    let arg = &args[0];
    if let Expr::Object(_) = arg {
        return Expr::Bool(true);
    }
    panic!("Invalid argument for free");
}
