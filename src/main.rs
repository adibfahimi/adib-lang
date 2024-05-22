mod eval;
mod lexer;
mod parser;
mod std_functions;

fn main() {
    let file_name = std::env::args().nth(1).expect("no file name given");
    let source = std::fs::read_to_string(file_name).expect("cannot read file");

    let tokens = lexer::tokenize(source.as_str()).expect("cannot tokenize");

    let asts = parser::parse(&tokens);
    let mut env = eval::Environment::new();

    for ast in asts {
        eval::eval(&ast, &mut env);
    }
}
