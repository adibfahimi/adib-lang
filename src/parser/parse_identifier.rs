use super::{parse_expr, Expr};
use crate::lexer::Token;
use Token::*;

pub fn parse_identifier(tokens: &[Token], s: String) -> (Expr, usize) {
    if tokens.len() > 1 {
        match tokens[1] {
            Dot => {
                let (expr, n) = parse_expr(&tokens[2..]);

                match expr {
                    Expr::Identifier(member) => (
                        Expr::Member {
                            name: s.clone(),
                            member,
                        },
                        n + 2,
                    ),
                    _ => panic!("Expected identifier after '.'"),
                }
            }
            Paren('[') => {
                let mut end = 2;
                while tokens[end] != Paren(']') {
                    end += 1;
                }
                let (expr, _) = parse_expr(&tokens[2..end]);

                let array_index = Expr::ArrayIndex {
                    name: s.clone(),
                    index: Box::new(expr),
                };
                if tokens.len() > end + 1 {
                    match tokens[end + 1] {
                        Operator(op) => {
                            let (expr, n) = parse_expr(&tokens[end + 2..]);
                            (
                                Expr::Op {
                                    op,
                                    lhs: Box::new(array_index.clone()),
                                    rhs: Box::new(expr),
                                },
                                n + end + 2,
                            )
                        }
                        ComparisonOperator(ref op) => {
                            let (expr, n) = parse_expr(&tokens[end + 2..]);
                            (
                                Expr::Comparison {
                                    op: op.clone(),
                                    lhs: Box::new(array_index.clone()),
                                    rhs: Box::new(expr),
                                },
                                n + end + 2,
                            )
                        }
                        _ => (array_index.clone(), end + 1),
                    }
                } else {
                    (array_index, end + 1)
                }
            }
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
