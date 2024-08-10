use phf::phf_map;
use std::process;

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "for" => TokenKind::For,
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "return" => TokenKind::Return,
    "while" => TokenKind::While,
    "const" => TokenKind::Const,
    "var" => TokenKind::Var,
    "and" => TokenKind::And,
    "or" => TokenKind::Or,
    "true" => TokenKind::True,
    "false" => TokenKind::False,
    "function" => TokenKind::Function,
    "as" => TokenKind::As,
    "match" => TokenKind::Match,
    "in" => TokenKind::In,
};

fn parse_keyword(keyword: &str) -> Option<TokenKind> {
    KEYWORDS.get(keyword).cloned()
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier,
    Number,
    String,

    Const,
    Var,
    For,
    If,
    Else,
    Return,
    While,
    And,
    Or,
    Function,
    As,
    Match,
    In,

    True,
    False,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Not,
    Plus,
    Minus,

    Assign,
    SemiColon,
    Comma,
    Dot,
    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

pub struct Lexer {
    buffer: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(buffer: Vec<char>) -> Self {
        Self {
            buffer,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_or_comment();

        if let Some(c) = self.peek() {
            match c {
                '(' => self.create_token(TokenKind::LParen),
                ')' => self.create_token(TokenKind::RParen),
                '{' => self.create_token(TokenKind::LBrace),
                '}' => self.create_token(TokenKind::RBrace),
                '[' => self.create_token(TokenKind::LBracket),
                ']' => self.create_token(TokenKind::RBracket),
                ',' => self.create_token(TokenKind::Comma),
                '.' => self.create_token(TokenKind::Dot),
                ';' => self.create_token(TokenKind::SemiColon),
                '+' => self.create_token(TokenKind::Plus),
                '-' => self.create_token(TokenKind::Minus),
                '*' => self.create_token(TokenKind::Mul),
                '/' => self.create_token(TokenKind::Div),
                '%' => self.create_token(TokenKind::Mod),
                '=' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.create_token(TokenKind::Eq)
                    } else {
                        self.create_token(TokenKind::Assign)
                    }
                }
                '!' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.create_token(TokenKind::NotEq)
                    } else {
                        self.create_token(TokenKind::Not)
                    }
                }
                '<' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.create_token(TokenKind::LtEq)
                    } else {
                        self.create_token(TokenKind::Lt)
                    }
                }
                '>' => {
                    self.advance();
                    if let Some('=') = self.peek() {
                        self.create_token(TokenKind::GtEq)
                    } else {
                        self.create_token(TokenKind::Gt)
                    }
                }
                '"' | '\'' => self.tokenize_string(),
                '0'..='9' => self.tokenize_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.tokenize_identifier_or_keyword(),
                _ => {
                    eprintln!(
                        "{}:{} | Error: Unexpected character: {}",
                        self.line, self.column, c
                    );
                    process::exit(1);
                }
            }
        } else {
            Token {
                kind: TokenKind::Eof,
                value: String::new(),
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.buffer.get(self.position).copied()
    }

    fn advance(&mut self) {
        self.position += 1;
        if self.peek() == Some('\n') {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    fn create_token(&mut self, kind: TokenKind) -> Token {
        let value = self.peek().unwrap().to_string();
        self.advance();
        Token { kind, value }
    }

    fn skip_whitespace_or_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else if c == '/' {
                if let Some(next) = self.peek() {
                    if next == '/' {
                        self.advance();
                        self.advance();
                        while let Some(c) = self.peek() {
                            if c == '\n' {
                                self.advance();
                                break;
                            }
                            self.advance();
                        }
                    } else if next == '*' {
                        self.advance();
                        self.advance();
                        loop {
                            if let Some(c) = self.peek() {
                                if c == '*' {
                                    self.advance();
                                    if let Some(c) = self.peek() {
                                        if c == '/' {
                                            self.advance();
                                            break;
                                        }
                                    }
                                }
                                self.advance();
                            } else {
                                eprintln!(
                                    "{}:{} | Error: Unterminated comment",
                                    self.line, self.column
                                );
                                process::exit(1);
                            }
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn tokenize_string(&mut self) -> Token {
        let quote = self.peek().unwrap();
        self.advance();
        let mut value = String::new();

        while let Some(c) = self.peek() {
            if c == quote {
                self.advance();
                return Token {
                    kind: TokenKind::String,
                    value,
                };
            }
            value.push(c);
            self.advance();
        }

        eprintln!("{}:{} | Error: Unterminated string", self.line, self.column);
        process::exit(1);
    }

    fn tokenize_number(&mut self) -> Token {
        let mut value = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '.' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Token {
            kind: TokenKind::Number,
            value,
        }
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Token {
        let mut value = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if let Some(token_kind) = parse_keyword(&value) {
            Token {
                kind: token_kind,
                value,
            }
        } else {
            Token {
                kind: TokenKind::Identifier,
                value,
            }
        }
    }
}
