use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Paren(char),
    Operator(char),
    Comma,
    ComparisonOperator(String),
    Keyword(String),
    Identifier(String),
    Num(i64),
    Str(String),
    Bool(bool),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Paren(c) => write!(f, "Paren({})", c),
            Token::Operator(c) => write!(f, "Operator({})", c),
            Token::Comma => write!(f, "Comma"),
            Token::ComparisonOperator(s) => write!(f, "ComparisonOperator({})", s),
            Token::Keyword(s) => write!(f, "Keyword({})", s),
            Token::Identifier(s) => write!(f, "Identifier({})", s),
            Token::Num(n) => write!(f, "Num({})", n),
            Token::Str(s) => write!(f, "Str({})", s),
            Token::Bool(b) => write!(f, "Bool({})", b),
        }
    }
}

use Token::*;

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut pos = 0;

    while pos < source.len() {
        let ch = source.chars().nth(pos).unwrap();

        match ch {
            '/' => {
                if pos + 1 < source.len() && source.chars().nth(pos + 1).unwrap() == '/' {
                    while pos < source.len() && source.chars().nth(pos).unwrap() != '\n' {
                        pos += 1;
                    }
                } else {
                    tokens.push(Operator('/'));
                    pos += 1;
                }
            }
            '(' | ')' | '{' | '}' | '[' | ']' => {
                tokens.push(Paren(ch));
                pos += 1;
            }
            '=' => {
                if pos + 1 < source.len() && source.chars().nth(pos + 1).unwrap() == '=' {
                    tokens.push(ComparisonOperator("==".to_string()));
                    pos += 2;
                } else {
                    tokens.push(Operator('='));
                    pos += 1;
                }
            }
            '"' => {
                let start = pos + 1;
                pos += 1;
                while pos < source.len() && source.chars().nth(pos).unwrap() != '"' {
                    pos += 1;
                }
                tokens.push(Str(source[start..pos].to_string()));
                pos += 1;
            }
            '!' => {
                if pos + 1 < source.len() && source.chars().nth(pos + 1).unwrap() == '=' {
                    tokens.push(ComparisonOperator("!=".to_string()));
                    pos += 2;
                } else {
                    tokens.push(Operator('!'));
                }
            }
            ',' => {
                tokens.push(Comma);
                pos += 1;
            }
            '+' | '-' | '*' | '%' | '^' | '<' | '>' => {
                tokens.push(Operator(ch));
                pos += 1;
            }

            _ if ch.is_ascii_digit() => {
                let start = pos;
                while pos < source.len() && source.chars().nth(pos).unwrap().is_ascii_digit() {
                    pos += 1;
                }
                let num = source[start..pos].parse().unwrap();
                tokens.push(Num(num));
            }
            _ if is_valid_identifier_char(ch) => {
                let start = pos;
                while pos < source.len()
                    && is_valid_identifier_char(source.chars().nth(pos).unwrap())
                {
                    pos += 1;
                }
                let identifier = &source[start..pos];
                match identifier {
                    "false" | "true" => {
                        tokens.push(Bool(identifier.parse().unwrap()));
                    }
                    "if" | "else" | "while" | "for" | "fn" | "var" | "return" => {
                        tokens.push(Keyword(identifier.to_string()));
                    }
                    _ => tokens.push(Identifier(identifier.to_string())),
                }
            }
            '\n' | '\t' | ' ' => {
                pos += 1;
            }
            _ => {
                println!("Unexpected character: {} in {}", ch, pos);
            }
        }
    }

    tokens
}

fn is_valid_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}
