use phf::phf_map;
use std::fs::File;
use std::io::{BufReader, Read};
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
    reader: BufReader<File>,
    buffer: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

const CHUNK_SIZE: usize = 16 * 1024; // 16KB

impl Lexer {
    pub fn new(file_path: &str) -> Result<Self, std::io::Error> {
        let file = File::open(file_path)?;
        let reader = BufReader::new(file);

        Ok(Self {
            reader,
            buffer: Vec::with_capacity(CHUNK_SIZE),
            position: 0,
            line: 1,
            column: 1,
        })
    }

    fn read_chunk(&mut self) -> Result<bool, std::io::Error> {
        let mut chunk = String::with_capacity(CHUNK_SIZE);
        let bytes_read = {
            let reader = &mut self.reader;
            reader.take(CHUNK_SIZE as u64).read_to_string(&mut chunk)?
        };

        if bytes_read == 0 {
            return Ok(false);
        }

        self.buffer.extend(chunk.chars());
        Ok(true)
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        if self.position >= self.buffer.len() && !self.read_chunk().map_err(|e| e.to_string())? {
            return Ok(Token {
                kind: TokenKind::Eof,
                value: String::new(),
            });
        }

        self.skip_whitespace_or_comment();

        if self.position >= self.buffer.len() {
            return Ok(Token {
                kind: TokenKind::Eof,
                value: String::new(),
            });
        }

        let c = self.buffer[self.position];
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
                if self.peek() == Some('=') {
                    self.create_token(TokenKind::Eq)
                } else {
                    self.create_token(TokenKind::Assign)
                }
            }
            '!' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.create_token(TokenKind::NotEq)
                } else {
                    self.create_token(TokenKind::Not)
                }
            }
            '<' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.create_token(TokenKind::LtEq)
                } else {
                    self.create_token(TokenKind::Lt)
                }
            }
            '>' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.create_token(TokenKind::GtEq)
                } else {
                    self.create_token(TokenKind::Gt)
                }
            }
            '"' | '\'' => self.tokenize_string(),
            '0'..='9' => self.tokenize_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.tokenize_identifier_or_keyword(),
            _ => Err(format!(
                "{}:{} | Error: Unexpected character: {}",
                self.line, self.column, c
            )),
        }
    }

    fn peek(&self) -> Option<char> {
        self.buffer.get(self.position).copied()
    }

    fn advance(&mut self) {
        if let Some(c) = self.buffer.get(self.position) {
            self.position += 1;
            if *c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    fn create_token(&mut self, kind: TokenKind) -> Result<Token, String> {
        let value = self.peek().unwrap().to_string();
        self.advance();
        Ok(Token { kind, value })
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

    fn tokenize_string(&mut self) -> Result<Token, String> {
        let quote = self.peek().unwrap();
        self.advance();
        let mut value = String::new();

        while let Some(c) = self.peek() {
            if c == quote {
                self.advance();
                return Ok(Token {
                    kind: TokenKind::String,
                    value,
                });
            }
            value.push(c);
            self.advance();
        }

        Err(format!(
            "{}:{} | Error: Unterminated string",
            self.line, self.column
        ))
    }

    fn tokenize_number(&mut self) -> Result<Token, String> {
        let mut value = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '.' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Ok(Token {
            kind: TokenKind::Number,
            value,
        })
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, String> {
        let mut value = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if let Some(token_kind) = KEYWORDS.get(&value) {
            Ok(Token {
                kind: token_kind.clone(),
                value,
            })
        } else {
            Ok(Token {
                kind: TokenKind::Identifier,
                value,
            })
        }
    }
}
