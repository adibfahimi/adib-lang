use super::{parse_expr, parse_if, Expr};
use crate::lexer::Token;
use Token::*;

pub fn parse_keyword(tokens: &[Token], key: &String) -> (Expr, usize) {
    match key.as_str() {
        "if" => parse_if::parse_if(tokens),
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
                i += n
            }

            (
                Expr::While {
                    cond: Box::new(cond),
                    block,
                },
                i + 1,
            )
        }

        _ => panic!("Unexpected keyword: {:?}", key),
    }
}
