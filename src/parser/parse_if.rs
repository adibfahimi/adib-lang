use super::{parse_expr, Expr};
use crate::lexer::Token;

pub fn parse_if(tokens: &[Token]) -> (Expr, usize) {
    let mut end = 2; // skip if + (
    while end < tokens.len() && tokens[end] != Token::Paren(')') {
        end += 1;
    }

    let (cond, _) = parse_expr(&tokens[2..end]);

    let mut then_block = vec![];
    let mut i = end + 2;
    while i < tokens.len() && tokens[i] != Token::Paren('}') {
        let (expr, n) = parse_expr(&tokens[i..]);
        then_block.push(expr);
        i += n;
    }

    i += 1; // skip }

    let mut else_block = vec![];
    if i < tokens.len() && tokens[i] == Token::Keyword("else".to_string()) {
        i += 2; // skip else + {
        while i < tokens.len() && tokens[i] != Token::Paren('}') {
            let (expr, n) = parse_expr(&tokens[i..]);
            else_block.push(expr);
            i += n;
        }
        i += 1; // skip }
    }

    (
        Expr::Conditional {
            cond: Box::new(cond),
            then_block,
            else_block,
        },
        i,
    )
}
