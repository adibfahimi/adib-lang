use super::{parse_expr, Expr};
use crate::lexer::Token;

use Token::*;

pub fn parse_number(tokens: &[Token], num: i64) -> (Expr, usize) {
    if tokens.len() > 1 {
        match tokens[1] {
            Operator(op) => {
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::Op {
                        op,
                        lhs: Box::new(Expr::Int(num)),
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
                        lhs: Box::new(Expr::Int(num)),
                        rhs: Box::new(expr),
                    },
                    n + 2,
                )
            }
            _ => (Expr::Int(num), 1),
        }
    } else {
        (Expr::Int(num), 1)
    }
}

pub fn parse_float(tokens: &[Token], num: f64) -> (Expr, usize) {
    if tokens.len() > 1 {
        match tokens[1] {
            Operator(op) => {
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::Op {
                        op,
                        lhs: Box::new(Expr::Float(num)),
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
                        lhs: Box::new(Expr::Float(num)),
                        rhs: Box::new(expr),
                    },
                    n + 2,
                )
            }
            _ => (Expr::Float(num), 1),
        }
    } else {
        (Expr::Float(num), 1)
    }
}
