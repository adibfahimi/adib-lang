use adib_lang::{interpreter::Interpreter, lexer::Lexer, parser::Parser};
use std::env;
use std::fs;
use std::process;

const USAGE: &str = "\
adib-lang - a small JavaScript-flavored scripting language

USAGE:
    adib-lang <file.adib>

OPTIONS:
    -h, --help       Show this help message
    -V, --version    Show the current version
";

fn main() {
    let Some(file_name) = env::args().nth(1) else {
        eprintln!("{}", USAGE);
        process::exit(1);
    };

    match file_name.as_str() {
        "-h" | "--help" => {
            print!("{}", USAGE);
            return;
        }
        "-V" | "--version" => {
            println!("adib-lang {}", env!("CARGO_PKG_VERSION"));
            return;
        }
        _ => {}
    }

    let source = match fs::read_to_string(&file_name) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("could not read `{}`: {}", file_name, error);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::from_source(&source);
    let mut parser = match Parser::new(&mut lexer) {
        Ok(parser) => parser,
        Err(error) => {
            eprintln!("{}: {}", file_name, error);
            process::exit(1);
        }
    };

    let program = match parser.parse() {
        Ok(program) => program,
        Err(error) => {
            eprintln!("{}: {}", file_name, error);
            process::exit(1);
        }
    };

    let mut interpreter = Interpreter::new();
    let result = interpreter.eval(&program);
    print!("{}", interpreter.take_output());

    if let Err(error) = result {
        eprintln!("{}: {}", file_name, error);
        process::exit(1);
    }
}
