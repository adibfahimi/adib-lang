use super::{parse_expr, Expr};
use crate::lexer::Token;

pub fn parse_operator(tokens: &[Token], op: char) -> (Expr, usize) {
    

    println!("tokens: {:?}", tokens);

    let (expr, n) = parse_expr(&tokens[1..]);
    (
        Expr::Op {
            op,
            lhs: Box::new(Expr::Nop),
            rhs: Box::new(expr),
        },
        n + 1,
    )
}
