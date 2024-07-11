use super::{parse_expr, Expr};
use crate::lexer::Token;
use Token::*;

pub fn parse_object(tokens: &[Token]) -> (Expr, usize) {
    let mut i = 1;
    let mut obj: Vec<(String, Box<Expr>)> = vec![];

    while tokens[i] != Paren('}') {
        let key = match tokens[i] {
            Identifier(ref s) => s.clone(),
            _ => panic!("Expected identifier as key in object"),
        };

        i += 1; // skip key
        if tokens[i] != Colon {
            panic!("Expected : after key in object");
        }
        i += 1; // skip colon

        let mut end_of_line = i;

        while tokens[end_of_line] != Comma && tokens[end_of_line] != Paren('}') {
            end_of_line += 1;
        }

        let (value, _) = parse_expr(&tokens[i..end_of_line]);

        obj.push((key, Box::new(value)));

        i = end_of_line;

        if tokens[i] == Comma {
            i += 1; // skip comma
        }
    }

    // Skip the closing curly brace
    i += 1;

    (Expr::Object(obj), i)
}
