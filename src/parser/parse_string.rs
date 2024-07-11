use super::{parse_expr, Expr};
use crate::lexer::Token;

use Token::*;

pub fn parse_string(tokens: &[Token], s: String) -> (Expr, usize) {
    
    if tokens.len() > 1 {
        match tokens[1] {
            Operator(op) => {
                
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::Op {
                        op,
                        lhs: Box::new(Expr::Str(s.clone())),
                        rhs: Box::new(expr),
                    },
                    n + 2,
                )
            }
            ComparisonOperator(ref op) => {
                
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::Comparison {
                        op: op.clone(),
                        lhs: Box::new(Expr::Str(s.clone())),
                        rhs: Box::new(expr),
                    },
                    n + 2,
                )
            }
            _ => {
                
                (Expr::Str(s.clone()), 1)
            }
        }
    } else {
        
        (Expr::Str(s.clone()), 1)
    }
}
