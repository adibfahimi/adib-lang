use std::process;

mod interpreter;
mod lexer;
mod parser;

fn main() {
    let file_name = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Please provide a file name.");
        process::exit(1);
    });

    let file_content = std::fs::read_to_string(file_name).unwrap_or_else(|err| {
        eprintln!("Error reading file:\n{}", err);
        process::exit(1);
    });

    let mut lexer = lexer::Lexer::new(file_content.chars().collect());

    let mut parser = parser::Parser::new(&mut lexer);
    let program = parser.parse().unwrap_or_else(|err| {
        eprintln!("Error parsing program:\n{}", err);
        process::exit(1);
    });

    let mut interpreter = interpreter::Interpreter::new();
    interpreter.eval(&program).unwrap_or_else(|err| {
        eprintln!("Error evaluating program:\n{}", err);
        process::exit(1);
    });
}
