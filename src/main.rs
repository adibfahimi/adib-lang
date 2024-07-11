mod eval;
mod lexer;
mod parser;

fn main() {
    let file_name = std::env::args().nth(1).expect("no file name given");
    let source = std::fs::read_to_string(file_name).expect("cannot read file");

    let tokens = match lexer::tokenize(source.as_str()) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Error: {:?}", e);
            std::process::exit(1);
        }
    };

    let asts = parser::parse(&tokens);
    let mut env = eval::Environment::new();

    for ast in asts {
        eval::eval(&ast, &mut env);
    }
}
