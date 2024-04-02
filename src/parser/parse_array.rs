use super::{parse_expr, Expr};
use crate::lexer::Token;
use Token::*;

pub fn parse_array(tokens: &[Token]) -> (Expr, usize) {
    let mut i = 1;
    let mut obj: Vec<Expr> = vec![];

    while tokens[i] != Paren(']') {
        let (value, _) = parse_expr(&tokens[i..]);
        obj.push(value);

        i += 1;

        if tokens[i] == Comma {
            i += 1; // skip comma
        }
    }

    // Skip the closing square bracket
    i += 1;

    (Expr::Array(obj), i)
}
