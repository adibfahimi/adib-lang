use crate::lexer::Token;
use core::panic;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Number(i64),
    Str(String),
    Identifier(String),

    Op {
        op: char,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Comparison {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Variable {
        name: String,
        value: Box<Expr>,
    },

    SetVariable {
        name: String,
        value: Box<Expr>,
    },

    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },

    FunctionDef {
        name: String,
        args: Vec<Token>,
        body: Vec<Expr>,
    },

    Return(Box<Expr>),

    Conditional {
        cond: Box<Expr>,
        then_block: Vec<Expr>,
        else_block: Vec<Expr>,
    },

    While {
        cond: Box<Expr>,
        block: Vec<Expr>,
    },

    For {
        init: Box<Expr>,
        cond: Box<Expr>,
        step: Box<Expr>,
        block: Vec<Expr>,
    },

    Nop, // TODO: remove this
}

use Token::*;

fn parse_expr(tokens: &[Token]) -> (Expr, usize) {
    // println!("parse_expr {:?}", tokens);
    match tokens[0] {
        Bool(b) => (Expr::Bool(b), 1),
        Num(a) => {
            if tokens.len() > 1 {
                match tokens[1] {
                    Operator(op) => {
                        let (expr, n) = parse_expr(&tokens[2..]);
                        (
                            Expr::Op {
                                op,
                                lhs: Box::new(Expr::Number(a)),
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
                                lhs: Box::new(Expr::Number(a)),
                                rhs: Box::new(expr),
                            },
                            n + 2,
                        )
                    }
                    _ => (Expr::Number(a), 1),
                }
            } else {
                (Expr::Number(a), 1)
            }
        }
        Str(ref s) => {
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
                    _ => (Expr::Str(s.clone()), 1),
                }
            } else {
                (Expr::Str(s.clone()), 1)
            }
        }
        Keyword(ref key) => match key.as_str() {
            "if" => {
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
            "return" => {
                let (expr, n) = parse_expr(&tokens[1..]);
                (Expr::Return(Box::new(expr)), n + 1)
            }
            "var" => {
                let name = match tokens[1] {
                    Identifier(ref s) => s.clone(),
                    _ => panic!("Expected identifier after let/var"),
                };

                let expr = match tokens[2] {
                    Operator('=') => parse_expr(&tokens[3..]),
                    _ => panic!("Expected = after {}", key),
                };

                (
                    Expr::Variable {
                        name,
                        value: Box::new(expr.0),
                    },
                    3 + expr.1,
                )
            }
            "function" => {
                let name = match tokens[1] {
                    Identifier(ref s) => s.clone(),
                    _ => panic!("Expected identifier after function"),
                };

                let mut i = 3; // skip fn + name + (
                let mut args = vec![];
                while tokens[i] != Paren(')') {
                    args.push(tokens[i].clone());
                    i += 1;
                    if tokens[i] == Comma {
                        i += 1; // skip comma
                    }
                }

                i += 2; // skip ) + {
                let mut body = vec![];
                while tokens[i] != Paren('}') {
                    let (expr, n) = parse_expr(&tokens[i..]);
                    body.push(expr);
                    i += n;
                }

                i += 1; // skip }

                (Expr::FunctionDef { name, args, body }, i)
            }
            "while" => {
                let mut end = 2; // skip while + (
                while end < tokens.len() && tokens[end] != Token::Paren(')') {
                    end += 1;
                }

                let (cond, _) = parse_expr(&tokens[2..end]);

                let mut block = vec![];
                let mut i = end + 2;
                while i < tokens.len() && tokens[i] != Token::Paren('}') {
                    let (expr, n) = parse_expr(&tokens[i..]);
                    block.push(expr);
                    i += n;
                }

                (
                    Expr::While {
                        cond: Box::new(cond),
                        block,
                    },
                    i + 1,
                )
            }
            "for" => {
                let mut end = 2;

                while end < tokens.len() && tokens[end] != Token::SemiColon {
                    end += 1;
                }

                let (init, _) = parse_expr(&tokens[2..end]);

                let mut end2 = end + 1;

                while end2 < tokens.len() && tokens[end2] != Token::SemiColon {
                    end2 += 1;
                }

                let (cond, _) = parse_expr(&tokens[end + 1..end2]);

                let mut end3 = end2;

                while end3 < tokens.len() && tokens[end3] != Token::Paren(')') {
                    end3 += 1;
                }

                let (step, _) = parse_expr(&tokens[end2 + 1..end3]);

                let mut block = vec![];
                let mut i = end3 + 2;

                while i < tokens.len() && tokens[i] != Token::Paren('}') {
                    let (expr, n) = parse_expr(&tokens[i..]);
                    block.push(expr);
                    i += n;
                }

                (
                    Expr::For {
                        init: Box::new(init),
                        cond: Box::new(cond),
                        step: Box::new(step),
                        block,
                    },
                    i + 1,
                ) // skip }
            }
            _ => panic!("Unexpected keyword: {:?}", key),
        },
        Operator(op) => {
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
        Identifier(ref s) => {
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

        SemiColon => (Expr::Nop, 1),
        _ => panic!("Unexpected token: {:?}", tokens[0]),
    }
}

pub fn parse(tokens: &[Token]) -> Vec<Expr> {
    let mut exprs = vec![];
    let mut i = 0;
    while i < tokens.len() {
        let (expr, n) = parse_expr(&tokens[i..]);
        exprs.push(expr);
        i += n;
    }
    exprs
}
