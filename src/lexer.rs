#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Num(i64),
    Str(String),
    Bool(bool),

    Paren(char),
    Operator(char),
    ComparisonOperator(String),
    Keyword(String),

    Comma,
    SemiColon,
    Colon,
    Dot,
}

use Token::*;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Identifier(s) => write!(f, "Identifier({})", s),
            Num(n) => write!(f, "Num({})", n),
            Str(s) => write!(f, "Str({})", s),
            Bool(b) => write!(f, "Bool({})", b),
            Paren(c) => write!(f, "Paren({})", c),
            Operator(c) => write!(f, "Operator({})", c),
            ComparisonOperator(s) => write!(f, "ComparisonOperator({})", s),
            Keyword(s) => write!(f, "Keyword({})", s),
            Comma => write!(f, "Comma"),
            SemiColon => write!(f, "SemiColon"),
            Colon => write!(f, "Colom"),
            Dot => write!(f, "Dot"),
        }
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut pos = 0;

    while pos < source.len() {
        let ch = source
            .chars()
            .nth(pos)
            .ok_or(LexerError::UnexpectedEndOfInput)?;

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

            '\n' | '\t' | ' ' => {
                pos += 1;
            }

            '(' | ')' | '{' | '}' | '[' | ']' => {
                tokens.push(Paren(ch));
                pos += 1;
            }

            '=' | '!' => {
                if pos + 1 < source.len() && source.chars().nth(pos + 1).unwrap() == '=' {
                    tokens.push(ComparisonOperator(format!("{}=", ch)));
                    pos += 2;
                } else {
                    tokens.push(Operator(ch));
                    pos += 1;
                }
            }

            '<' | '>' => {
                if pos + 1 < source.len() && source.chars().nth(pos + 1).unwrap() == '=' {
                    tokens.push(ComparisonOperator(format!("{}=", ch)));
                    pos += 2;
                } else {
                    tokens.push(ComparisonOperator(ch.to_string()));
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

            ',' => {
                tokens.push(Comma);
                pos += 1;
            }

            ';' => {
                tokens.push(SemiColon);
                pos += 1;
            }

            ':' => {
                tokens.push(Colon);
                pos += 1;
            }
            '.' => {
                tokens.push(Dot);
                pos += 1;
            }

            '+' | '-' | '*' | '%' | '^' => {
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
                    "if" | "else" | "while" | "for" | "function" | "var" | "return" | "const"
                    | "let" => {
                        tokens.push(Keyword(identifier.to_string()));
                    }
                    _ => tokens.push(Identifier(identifier.to_string())),
                }
            }

            _ => {
                return Err(LexerError::UnexpectedCharacter(ch));
            }
        }
    }

    Ok(tokens)
}

fn is_valid_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[test]
fn test_tokenize() {
    let source = r#"
    // sum of two numbers
    function sum(a, b) {
        return a + b;
    }

    var a = 1;
    var b = 2;

    var c = sum(a, b);

    for (var i = 0; i < 10; i = i + 1) {
        print(i);
    }

    while (a < 10) {
        print(a);
        a = a + 1;
    }

    if (a > 10) {
        print("a is greater than 10");
    } else {
        print("a is less than 10");
    }
    "#;

    let tokens = tokenize(source).unwrap();
    let expected = vec![
        Keyword("function".to_string()),
        Identifier("sum".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        Comma,
        Identifier("b".to_string()),
        Paren(')'),
        Paren('{'),
        Keyword("return".to_string()),
        Identifier("a".to_string()),
        Operator('+'),
        Identifier("b".to_string()),
        SemiColon,
        Paren('}'),
        Keyword("var".to_string()),
        Identifier("a".to_string()),
        Operator('='),
        Num(1),
        SemiColon,
        Keyword("var".to_string()),
        Identifier("b".to_string()),
        Operator('='),
        Num(2),
        SemiColon,
        Keyword("var".to_string()),
        Identifier("c".to_string()),
        Operator('='),
        Identifier("sum".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        Comma,
        Identifier("b".to_string()),
        Paren(')'),
        SemiColon,
        Keyword("for".to_string()),
        Paren('('),
        Keyword("var".to_string()),
        Identifier("i".to_string()),
        Operator('='),
        Num(0),
        SemiColon,
        Identifier("i".to_string()),
        ComparisonOperator("<".to_string()),
        Num(10),
        SemiColon,
        Identifier("i".to_string()),
        Operator('='),
        Identifier("i".to_string()),
        Operator('+'),
        Num(1),
        Paren(')'),
        Paren('{'),
        Identifier("print".to_string()),
        Paren('('),
        Identifier("i".to_string()),
        Paren(')'),
        SemiColon,
        Paren('}'),
        Keyword("while".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        ComparisonOperator("<".to_string()),
        Num(10),
        Paren(')'),
        Paren('{'),
        Identifier("print".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        Paren(')'),
        SemiColon,
        Identifier("a".to_string()),
        Operator('='),
        Identifier("a".to_string()),
        Operator('+'),
        Num(1),
        SemiColon,
        Paren('}'),
        Keyword("if".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        ComparisonOperator(">".to_string()),
        Num(10),
        Paren(')'),
        Paren('{'),
        Identifier("print".to_string()),
        Paren('('),
        Str("a is greater than 10".to_string()),
        Paren(')'),
        SemiColon,
        Paren('}'),
        Keyword("else".to_string()),
        Paren('{'),
        Identifier("print".to_string()),
        Paren('('),
        Str("a is less than 10".to_string()),
        Paren(')'),
        SemiColon,
        Paren('}'),
    ];

    assert_eq!(tokens, expected);
}
