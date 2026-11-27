pub mod ast;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;

mod builtins;
mod environment;
mod value;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

/// Parse and execute a source string, returning everything the program printed.
///
/// ```no_run
/// let output = adib_lang::run_source("print(\"hi\")")?;
/// assert_eq!(output, "hi\n");
/// # Ok::<(), String>(())
/// ```
pub fn run_source(source: &str) -> Result<String, String> {
    let mut lexer = Lexer::from_source(source);
    let mut parser = Parser::new(&mut lexer).map_err(|error| error.render(source))?;
    let program = parser.parse().map_err(|error| error.render(source))?;

    let mut interpreter = Interpreter::new();
    interpreter
        .eval(&program)
        .map_err(|error| error.render(source))?;
    Ok(interpreter.take_output())
}

#[cfg(target_arch = "wasm32")]
mod wasm {
    use wasm_bindgen::prelude::*;

    /// Run a program from JavaScript and return its combined output.
    #[wasm_bindgen]
    pub fn run(source: &str) -> String {
        match crate::run_source(source) {
            Ok(output) => output,
            Err(error) => format!("error: {}", error),
        }
    }
}
