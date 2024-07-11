#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Int(i64),
    Float(f64),
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

fn is_valid_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            tokens: Vec::new(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
        self.advance();
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        while let Some(&ch) = self.peek() {
            match ch {
                '/' => self.handle_comment_or_div(),
                '\n' | '\t' | ' ' => {
                    self.advance();
                }
                '(' | ')' | '{' | '}' | '[' | ']' => self.push_token(Paren(ch)),
                '=' | '!' | '<' | '>' => self.handle_comparison_or_operator(ch),
                '"' => self.handle_string_literal(),
                ',' => self.push_token(Comma),
                ';' => self.push_token(SemiColon),
                ':' => self.push_token(Colon),
                '.' => self.push_token(Dot),
                '+' | '-' | '*' | '%' | '^' => self.push_token(Operator(ch)),
                _ if ch.is_ascii_digit() => self.handle_number(),
                _ if is_valid_identifier_char(ch) => self.handle_identifier(),
                _ => return Err(LexerError::UnexpectedCharacter(ch)),
            }
        }
        Ok(std::mem::take(&mut self.tokens))
    }

    fn handle_comment_or_div(&mut self) {
        self.advance();
        if let Some(&'/') = self.peek() {
            self.advance();
            while let Some(&next_ch) = self.peek() {
                if next_ch == '\n' {
                    break;
                }
                self.advance();
            }
        } else {
            self.tokens.push(Operator('/'));
        }
    }

    fn handle_comparison_or_operator(&mut self, ch: char) {
        self.advance();
        if let Some(&'=') = self.peek() {
            self.advance();
            self.tokens.push(ComparisonOperator(format!("{}=", ch)));
        } else {
            self.tokens.push(match ch {
                '<' | '>' => ComparisonOperator(ch.to_string()),
                _ => Operator(ch),
            });
        }
    }

    fn handle_string_literal(&mut self) {
        self.advance();
        let mut string_literal = String::new();
        while let Some(&next_ch) = self.peek() {
            if next_ch == '"' {
                break;
            }
            string_literal.push(next_ch);
            self.advance();
        }
        self.advance(); // consume closing quote
        self.tokens.push(Str(string_literal));
    }

    fn handle_number(&mut self) {
        let mut number = String::new();
        let mut is_float = false;
        while let Some(&next_ch) = self.peek() {
            if next_ch.is_ascii_digit() || next_ch == '.' {
                if next_ch == '.' {
                    is_float = true;
                }
                number.push(next_ch);
                self.advance();
            } else {
                break;
            }
        }
        if is_float {
            self.tokens.push(Float(number.parse().unwrap()));
        } else {
            self.tokens.push(Int(number.parse().unwrap()));
        }
    }

    fn handle_identifier(&mut self) {
        let mut identifier = String::new();
        while let Some(&next_ch) = self.peek() {
            if is_valid_identifier_char(next_ch) {
                identifier.push(next_ch);
                self.advance();
            } else {
                break;
            }
        }
        match identifier.as_str() {
            "false" | "true" => self.tokens.push(Bool(identifier.parse().unwrap())),
            "if" | "else" | "while" | "for" | "function" | "var" | "return" | "const" | "let" => {
                self.tokens.push(Keyword(identifier));
            }
            _ => self.tokens.push(Identifier(identifier)),
        }
    }
}

#[test]
fn test_tokenize() {
    let source = r#"
    const f = 3.5;
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

    let mut lexer = Lexer::new(source);

    let tokens = lexer.tokenize().unwrap();
    let expected = vec![
        Keyword("const".to_string()),
        Identifier("f".to_string()),
        Operator('='),
        Float(3.5),
        SemiColon,
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
        Int(1),
        SemiColon,
        Keyword("var".to_string()),
        Identifier("b".to_string()),
        Operator('='),
        Int(2),
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
        Int(0),
        SemiColon,
        Identifier("i".to_string()),
        ComparisonOperator("<".to_string()),
        Int(10),
        SemiColon,
        Identifier("i".to_string()),
        Operator('='),
        Identifier("i".to_string()),
        Operator('+'),
        Int(1),
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
        Int(10),
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
        Int(1),
        SemiColon,
        Paren('}'),
        Keyword("if".to_string()),
        Paren('('),
        Identifier("a".to_string()),
        ComparisonOperator(">".to_string()),
        Int(10),
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
