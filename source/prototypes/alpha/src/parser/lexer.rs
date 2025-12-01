use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Export,
    Function,
    Return,
    If,
    Else,
    For,
    True,
    False,

    Identifier(String),
    IntegerLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    ColonEq,
    Semicolon,
    Arrow,

    Plus,
    PlusPlus,
    PlusEq,
    Minus,
    MinusMinus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Eq,
    EqEq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
    file: PathBuf,
}

impl Lexer {
    pub fn new(source: &str, file: PathBuf) -> Self {
        Lexer {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
            file,
        }
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }

    fn current(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.current();
        if let Some(ch) = c {
            self.pos += 1;
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        c
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if c.is_whitespace() {
                self.advance();
            } else if c == '/' && self.peek() == Some('/') {
                while let Some(c) = self.current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn make_token(&self, kind: TokenKind, start_line: usize, start_column: usize) -> Token {
        Token {
            kind,
            line: start_line,
            column: start_column,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let start_line = self.line;
        let start_column = self.column;

        let Some(c) = self.current() else {
            return Ok(self.make_token(TokenKind::Eof, start_line, start_column));
        };

        let token = match c {
            '(' => {
                self.advance();
                self.make_token(TokenKind::LeftParen, start_line, start_column)
            }
            ')' => {
                self.advance();
                self.make_token(TokenKind::RightParen, start_line, start_column)
            }
            '{' => {
                self.advance();
                self.make_token(TokenKind::LeftBrace, start_line, start_column)
            }
            '}' => {
                self.advance();
                self.make_token(TokenKind::RightBrace, start_line, start_column)
            }
            ',' => {
                self.advance();
                self.make_token(TokenKind::Comma, start_line, start_column)
            }
            ':' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::ColonEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Colon, start_line, start_column)
                }
            }
            ';' => {
                self.advance();
                self.make_token(TokenKind::Semicolon, start_line, start_column)
            }
            '+' => {
                self.advance();
                if self.current() == Some('+') {
                    self.advance();
                    self.make_token(TokenKind::PlusPlus, start_line, start_column)
                } else if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::PlusEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Plus, start_line, start_column)
                }
            }
            '*' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::StarEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Star, start_line, start_column)
                }
            }
            '/' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::SlashEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Slash, start_line, start_column)
                }
            }
            '-' => {
                self.advance();
                if self.current() == Some('>') {
                    self.advance();
                    self.make_token(TokenKind::Arrow, start_line, start_column)
                } else if self.current() == Some('-') {
                    self.advance();
                    self.make_token(TokenKind::MinusMinus, start_line, start_column)
                } else if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::MinusEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Minus, start_line, start_column)
                }
            }
            '=' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::EqEq, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Eq, start_line, start_column)
                }
            }
            '!' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::Ne, start_line, start_column)
                } else {
                    return Err(LexError {
                        message: format!("Unexpected character: '{}'", c),
                        file: self.file.clone(),
                        line: start_line,
                        column: start_column,
                    });
                }
            }
            '<' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::Le, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Lt, start_line, start_column)
                }
            }
            '>' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    self.make_token(TokenKind::Ge, start_line, start_column)
                } else {
                    self.make_token(TokenKind::Gt, start_line, start_column)
                }
            }
            '"' => self.scan_string(start_line, start_column)?,
            _ if c.is_ascii_digit() => self.scan_number(start_line, start_column)?,
            _ if c.is_alphabetic() || c == '_' => self.scan_identifier(start_line, start_column),
            _ => {
                return Err(LexError {
                    message: format!("Unexpected character: '{}'", c),
                    file: self.file.clone(),
                    line: start_line,
                    column: start_column,
                });
            }
        };

        Ok(token)
    }

    fn scan_string(&mut self, start_line: usize, start_column: usize) -> Result<Token, LexError> {
        self.advance();
        let mut value = String::new();

        loop {
            match self.current() {
                None => {
                    return Err(LexError {
                        message: "Unterminated string literal".to_string(),
                        file: self.file.clone(),
                        line: start_line,
                        column: start_column,
                    });
                }
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.current() {
                        Some('n') => {
                            value.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            value.push('\t');
                            self.advance();
                        }
                        Some('r') => {
                            value.push('\r');
                            self.advance();
                        }
                        Some('\\') => {
                            value.push('\\');
                            self.advance();
                        }
                        Some('"') => {
                            value.push('"');
                            self.advance();
                        }
                        Some(c) => {
                            return Err(LexError {
                                message: format!("Invalid escape sequence: \\{}", c),
                                file: self.file.clone(),
                                line: self.line,
                                column: self.column,
                            });
                        }
                        None => {
                            return Err(LexError {
                                message: "Unterminated string literal".to_string(),
                                file: self.file.clone(),
                                line: start_line,
                                column: start_column,
                            });
                        }
                    }
                }
                Some(c) => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        Ok(self.make_token(TokenKind::StringLiteral(value), start_line, start_column))
    }

    fn scan_number(&mut self, start_line: usize, start_column: usize) -> Result<Token, LexError> {
        let mut value = String::new();
        let mut is_float = false;

        while let Some(c) = self.current() {
            if c.is_ascii_digit() {
                value.push(c);
                self.advance();
            } else if c == '.' && !is_float {
                if let Some(next) = self.peek() {
                    if next.is_ascii_digit() {
                        is_float = true;
                        value.push(c);
                        self.advance();
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

        if is_float {
            let _: f64 = value.parse().map_err(|_| LexError {
                message: format!("Invalid float literal: {}", value),
                file: self.file.clone(),
                line: start_line,
                column: start_column,
            })?;
            Ok(self.make_token(TokenKind::FloatLiteral(value), start_line, start_column))
        } else {
            let _: i128 = value.parse().map_err(|_| LexError {
                message: format!("Integer literal too large: {}", value),
                file: self.file.clone(),
                line: start_line,
                column: start_column,
            })?;
            Ok(self.make_token(TokenKind::IntegerLiteral(value), start_line, start_column))
        }
    }

    fn scan_identifier(&mut self, start_line: usize, start_column: usize) -> Token {
        let mut value = String::new();

        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let kind = match value.as_str() {
            "export" => TokenKind::Export,
            "function" => TokenKind::Function,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(value),
        };

        self.make_token(kind, start_line, start_column)
    }

    pub fn tokenize_all(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.file.display(),
            self.line,
            self.column,
            self.message
        )
    }
}
