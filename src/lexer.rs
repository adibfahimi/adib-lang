use phf::phf_map;
use std::fs;

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
    "null" => TokenKind::Null,
    "function" => TokenKind::Function,
    "as" => TokenKind::As,
    "match" => TokenKind::Match,
    "in" => TokenKind::In,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Null,

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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
    pub column: usize,
}

pub struct Lexer {
    source: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(file_path: &str) -> Result<Self, String> {
        let source = fs::read_to_string(file_path)
            .map_err(|err| format!("could not read `{}`: {}", file_path, err))?;
        Ok(Self::from_source(&source))
    }

    pub fn from_source(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace_and_comments()?;

        let line = self.line;
        let column = self.column;

        let Some(c) = self.peek() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                value: String::new(),
                line,
                column,
            });
        };

        let kind = match c {
            '(' => self.single(TokenKind::LParen),
            ')' => self.single(TokenKind::RParen),
            '{' => self.single(TokenKind::LBrace),
            '}' => self.single(TokenKind::RBrace),
            '[' => self.single(TokenKind::LBracket),
            ']' => self.single(TokenKind::RBracket),
            ',' => self.single(TokenKind::Comma),
            '.' => self.single(TokenKind::Dot),
            ';' => self.single(TokenKind::SemiColon),
            '+' => self.single(TokenKind::Plus),
            '-' => self.single(TokenKind::Minus),
            '*' => self.single(TokenKind::Mul),
            '/' => self.single(TokenKind::Div),
            '%' => self.single(TokenKind::Mod),
            '=' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::Eq
                } else {
                    TokenKind::Assign
                }
            }
            '!' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::NotEq
                } else {
                    TokenKind::Not
                }
            }
            '<' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            '"' | '\'' => return self.tokenize_string(),
            '0'..='9' => return self.tokenize_number(),
            'a'..='z' | 'A'..='Z' | '_' => return self.tokenize_identifier_or_keyword(),
            _ => return Err(self.error(format!("unexpected character `{}`", c))),
        };

        Ok(Token {
            kind,
            value: c.to_string(),
            line,
            column,
        })
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.position).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.position + 1).copied()
    }

    fn advance(&mut self) {
        if let Some(&c) = self.source.get(self.position) {
            self.position += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    fn single(&mut self, kind: TokenKind) -> TokenKind {
        self.advance();
        kind
    }

    fn error(&self, message: impl Into<String>) -> String {
        format!("{}:{} | {}", self.line, self.column, message.into())
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), String> {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => self.advance(),
                Some('/') if self.peek_next() == Some('/') => {
                    self.advance();
                    self.advance();
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                Some('/') if self.peek_next() == Some('*') => {
                    self.advance();
                    self.advance();
                    let mut closed = false;
                    while let Some(c) = self.peek() {
                        if c == '*' && self.peek_next() == Some('/') {
                            self.advance();
                            self.advance();
                            closed = true;
                            break;
                        }
                        self.advance();
                    }
                    if !closed {
                        return Err(self.error("unterminated block comment"));
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn tokenize_string(&mut self) -> Result<Token, String> {
        let line = self.line;
        let column = self.column;
        let quote = self.peek().unwrap();
        self.advance();

        let mut value = String::new();
        while let Some(c) = self.peek() {
            if c == quote {
                self.advance();
                return Ok(Token {
                    kind: TokenKind::String,
                    value,
                    line,
                    column,
                });
            }

            if c == '\\' {
                self.advance();
                let Some(escaped) = self.peek() else {
                    return Err(self.error("unterminated string"));
                };
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '0' => value.push('\0'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    other => {
                        return Err(self.error(format!("unknown escape sequence `\\{}`", other)))
                    }
                }
                self.advance();
                continue;
            }

            value.push(c);
            self.advance();
        }

        Err(self.error("unterminated string"))
    }

    fn tokenize_number(&mut self) -> Result<Token, String> {
        let line = self.line;
        let column = self.column;
        let mut value = String::new();

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') && matches!(self.peek_next(), Some(c) if c.is_ascii_digit()) {
            value.push('.');
            self.advance();
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    value.push(c);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        Ok(Token {
            kind: TokenKind::Number,
            value,
            line,
            column,
        })
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, String> {
        let line = self.line;
        let column = self.column;
        let mut value = String::new();

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let kind = KEYWORDS
            .get(value.as_str())
            .cloned()
            .unwrap_or(TokenKind::Identifier);

        Ok(Token {
            kind,
            value,
            line,
            column,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::from_source(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().expect("token should lex");
            let done = token.kind == TokenKind::Eof;
            tokens.push(token);
            if done {
                break;
            }
        }
        tokens
    }

    #[test]
    fn tokenizes_division_and_comments() {
        let kinds: Vec<TokenKind> = tokens("8 / 2 // comment\n/* block */ 3")
            .into_iter()
            .map(|token| token.kind)
            .collect();

        assert_eq!(
            kinds,
            vec![
                TokenKind::Number,
                TokenKind::Div,
                TokenKind::Number,
                TokenKind::Number,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tracks_positions() {
        let tokens = tokens("var\n  x = 1");
        assert_eq!((tokens[0].line, tokens[0].column), (1, 1));
        assert_eq!((tokens[1].line, tokens[1].column), (2, 3));
        assert_eq!((tokens[2].line, tokens[2].column), (2, 5));
    }

    #[test]
    fn decodes_escapes() {
        let tokens = tokens(r#""a\nb\t\"c\"""#);
        assert_eq!(tokens[0].value, "a\nb\t\"c\"");
    }

    #[test]
    fn reports_unterminated_constructs() {
        let mut lexer = Lexer::from_source("\"oops");
        assert!(lexer
            .next_token()
            .unwrap_err()
            .contains("unterminated string"));

        let mut lexer = Lexer::from_source("/* oops");
        assert!(lexer
            .next_token()
            .unwrap_err()
            .contains("unterminated block comment"));
    }

    #[test]
    fn lexes_numbers_and_properties() {
        let tokens = tokens("12.5 nums.len");
        assert_eq!(tokens[0].value, "12.5");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Dot);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
    }
}
