use super::{parse_expr, Expr};
use crate::lexer::Token;
use Token::*;

pub fn parse_identifier(tokens: &[Token], s: String) -> (Expr, usize) {
    if tokens.len() > 1 {
        match tokens[1] {
            Paren('(') => {
                let mut i = 2; // skip identifier + (
                let mut args = vec![];
                while tokens[i] != Paren(')') {
                    let (expr, n) = parse_expr(&tokens[i..]);
                    args.push(expr);
                    i += n;
                    if tokens[i] == Comma {
                        i += 1; // skip comma
                    }
                }
                (
                    Expr::FunctionCall {
                        name: s.clone(),
                        args,
                    },
                    i + 1,
                )
            }
            Operator('=') => {
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::SetVariable {
                        name: s.clone(),
                        value: Box::new(expr),
                    },
                    n + 2,
                )
            }
            Operator(op) => {
                let (expr, n) = parse_expr(&tokens[2..]);
                (
                    Expr::Op {
                        op,
                        lhs: Box::new(Expr::Identifier(s.clone())),
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
                        lhs: Box::new(Expr::Identifier(s.clone())),
                        rhs: Box::new(expr),
                    },
                    n + 2,
                )
            }

            _ => (Expr::Identifier(s.clone()), 1),
        }
    } else {
        (Expr::Identifier(s.clone()), 1)
    }
}
