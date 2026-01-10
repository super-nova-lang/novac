#![allow(dead_code)]

use token::{Span, Token, TokenKind};

pub mod token;

pub struct Lexer {
    file: String,
    input: String,
    position: usize,
    read_position: usize,
    current_char: Option<char>,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(file: &str, input: &str) -> Self {
        Lexer {
            file: file.to_string(),
            input: input.to_string(),
            position: 0,
            read_position: 0,
            current_char: None,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        self.read_char();

        while let Some(_) = self.current_char {
            self.skip_whitespace();

            if self.current_char.is_none() {
                break;
            }

            let ch = self.current_char.unwrap();

            // Identifiers and keywords
            if ch.is_ascii_alphabetic() || ch == '_' {
                let ident = self.read_identifier();
                tokens.push(self.lookup_ident(&ident));
                continue;
            }

            // Numbers
            if ch.is_ascii_digit() {
                let number = self.read_number();
                tokens.push(self.token(TokenKind::Number(number)));
                continue;
            }

            // Strings
            if ch == '"' {
                let value = self.read_string();
                tokens.push(self.token(TokenKind::String(value)));
                continue;
            }

            // Char literals
            if ch == '\'' {
                match self.read_char_literal() {
                    Some(c) => tokens.push(self.token(TokenKind::Char(c))),
                    _ => tokens.push(self.token(TokenKind::Unknown('\''))),
                }
                continue;
            }

            // Comments (regular and doc)
            if ch == '(' && self.peek_char() == Some('*') {
                if let Some(doc) = self.read_comment() {
                    tokens.push(doc);
                }
                continue;
            }

            let kind = match ch {
                ':' => {
                    if self.peek_char() == Some(':') {
                        self.read_char();
                        self.read_char();
                        TokenKind::DoubleColon
                    } else if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::Walrus
                    } else {
                        self.read_char();
                        TokenKind::Colon
                    }
                }
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::DoubleEql
                    } else {
                        self.read_char();
                        TokenKind::Eql
                    }
                }
                '!' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::NotEql
                    } else {
                        self.read_char();
                        TokenKind::Bang
                    }
                }
                '<' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::LesserEql
                    } else if self.peek_char() == Some('-') {
                        self.read_char();
                        self.read_char();
                        TokenKind::BackArrow
                    } else if self.peek_char() == Some('<') {
                        self.read_char();
                        self.read_char();
                        TokenKind::ShiftLeft
                    } else {
                        self.read_char();
                        TokenKind::Lesser
                    }
                }
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::GreaterEql
                    } else if self.peek_char() == Some('>') {
                        self.read_char();
                        self.read_char();
                        TokenKind::ShiftRight
                    } else {
                        self.read_char();
                        TokenKind::Greater
                    }
                }
                '+' => {
                    if self.peek_char() == Some('+') {
                        self.read_char();
                        self.read_char();
                        TokenKind::PlusPlus
                    } else if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::PlusEql
                    } else {
                        self.read_char();
                        TokenKind::Plus
                    }
                }
                '-' => {
                    if self.peek_char() == Some('-') {
                        self.read_char();
                        self.read_char();
                        TokenKind::MinusMinus
                    } else if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::MinusEql
                    } else if self.peek_char() == Some('>') {
                        self.read_char();
                        self.read_char();
                        TokenKind::SkinnyArrow
                    } else {
                        self.read_char();
                        TokenKind::Dash
                    }
                }
                '|' => {
                    if self.peek_char() == Some('>') {
                        self.read_char();
                        self.read_char();
                        TokenKind::Pipe
                    } else if self.peek_char() == Some('|') {
                        self.read_char();
                        self.read_char();
                        TokenKind::Or
                    } else if self.peek_char() == Some('.') {
                        self.read_char();
                        self.read_char();
                        TokenKind::BitwiseOr
                    } else {
                        self.read_char();
                        TokenKind::Bar
                    }
                }
                '&' => {
                    if self.peek_char() == Some('&') {
                        self.read_char();
                        self.read_char();
                        TokenKind::And
                    } else if self.peek_char() == Some('.') {
                        self.read_char();
                        self.read_char();
                        TokenKind::BitwiseAnd
                    } else {
                        self.read_char();
                        TokenKind::Amper
                    }
                }
                '.' => {
                    if self.peek_char() == Some('.') && self.peek_n(2) == Some('.') {
                        self.read_char();
                        self.read_char();
                        self.read_char();
                        TokenKind::Ellipsis
                    } else {
                        self.read_char();
                        TokenKind::Dot
                    }
                }
                '~' => {
                    self.read_char();
                    TokenKind::Concat
                }
                '*' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::StarEql
                    } else {
                        self.read_char();
                        TokenKind::Star
                    }
                }
                '/' => {
                    if self.peek_char() == Some('=') {
                        self.read_char();
                        self.read_char();
                        TokenKind::SlashEql
                    } else {
                        self.read_char();
                        TokenKind::ForwardSlash
                    }
                }
                '`' => {
                    self.read_char();
                    TokenKind::BackTick
                }
                '@' => {
                    self.read_char();
                    TokenKind::At
                }
                '#' => {
                    self.read_char();
                    TokenKind::Hash
                }
                '$' => {
                    self.read_char();
                    TokenKind::Dollar
                }
                '%' => {
                    self.read_char();
                    TokenKind::Mod
                }
                '^' => {
                    self.read_char();
                    TokenKind::Carrot
                }
                '(' => {
                    self.read_char();
                    TokenKind::OpenParen
                }
                ')' => {
                    self.read_char();
                    TokenKind::CloseParen
                }
                '{' => {
                    self.read_char();
                    TokenKind::OpenBrack
                }
                '}' => {
                    self.read_char();
                    TokenKind::CloseBrack
                }
                '[' => {
                    self.read_char();
                    TokenKind::OpenSquare
                }
                ']' => {
                    self.read_char();
                    TokenKind::CloseSquare
                }
                '_' => {
                    self.read_char();
                    TokenKind::LowDash
                }
                '?' => {
                    self.read_char();
                    TokenKind::Question
                }
                ',' => {
                    self.read_char();
                    TokenKind::Comma
                }
                ';' => {
                    self.read_char();
                    TokenKind::SemiColon
                }
                '\\' => {
                    self.read_char();
                    TokenKind::BackSlash
                }
                _ => {
                    self.read_char();
                    TokenKind::Unknown(ch)
                }
            };

            tokens.push(self.token(kind));
        }

        tokens.push(self.token(TokenKind::Eof));
        tokens
    }

    fn read_char(&mut self) {
        if let Some(ch) = self.input.chars().nth(self.read_position) {
            self.current_char = Some(ch);
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        } else {
            self.current_char = None;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn peek_n(&self, offset: usize) -> Option<char> {
        self.input.chars().nth(self.read_position + offset)
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.current_char, Some(ch) if ch.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.read_char();
            } else {
                break;
            }
        }

        ident
    }

    fn read_number(&mut self) -> i32 {
        let mut digits = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                digits.push(ch);
                self.read_char();
            } else {
                break;
            }
        }

        digits.parse().unwrap_or(0)
    }

    fn read_string(&mut self) -> String {
        // Skip opening quote
        self.read_char();

        let mut value = String::new();

        while let Some(ch) = self.current_char {
            if ch == '"' {
                break;
            }

            if ch == '\\' {
                if let Some(escaped) = self.read_escape_sequence() {
                    value.push(escaped);
                }
            } else {
                value.push(ch);
            }

            self.read_char();
        }

        // Skip closing quote if present
        if self.current_char == Some('"') {
            self.read_char();
        }

        value
    }

    fn read_char_literal(&mut self) -> Option<char> {
        // Skip opening quote
        self.read_char();

        let c = match self.current_char {
            Some('\\') => {
                let escape = self.read_escape_sequence();
                self.read_char();
                escape
            }
            Some(ch) => {
                let result = Some(ch);
                self.read_char();
                result
            }
            _ => None,
        }?;

        if self.current_char == Some('\'') {
            self.read_char();
            Some(c)
        } else {
            None
        }
    }

    fn read_escape_sequence(&mut self) -> Option<char> {
        // Move to the escaped character
        self.read_char();

        match self.current_char {
            Some('n') => Some('\n'),
            Some('t') => Some('\t'),
            Some('r') => Some('\r'),
            Some('"') => Some('"'),
            Some('\\') => Some('\\'),
            Some('\'') => Some('\''),
            Some(other) => Some(other),
            _ => None,
        }
    }

    fn read_comment(&mut self) -> Option<Token> {
        // At this point current_char is '(' and next is '*'
        let is_doc = self.peek_n(1) == Some('!');

        self.read_char(); // move to '*'
        self.read_char(); // move past '*'

        if is_doc {
            self.read_char(); // consume '!'
        }

        let mut depth = 1usize;
        let mut content = String::new();

        while let Some(ch) = self.current_char {
            if ch == '(' && self.peek_char() == Some('*') {
                depth += 1;
                self.read_char();
                self.read_char();
                continue;
            }

            if ch == '*' && self.peek_char() == Some(')') {
                self.read_char();
                self.read_char();
                depth -= 1;

                if depth == 0 {
                    break;
                }

                continue;
            }

            if is_doc && depth == 1 {
                content.push(ch);
            }

            self.read_char();
        }

        if is_doc {
            Some(self.token(TokenKind::DocComment(content.trim().to_string())))
        } else {
            None
        }
    }

    fn token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span {
                file: self.file.clone(),
                line: self.line,
                column: self.column,
            },
        }
    }

    fn lookup_ident(&self, ident: &str) -> Token {
        let kind = match ident {
            "let" => TokenKind::Let,
            "open" => TokenKind::Open,
            "import" => TokenKind::Import,
            "module" => TokenKind::Module,
            "export" => TokenKind::Export,
            "match" => TokenKind::Match,
            "with" => TokenKind::With,
            "as" => TokenKind::As,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "in" => TokenKind::In,
            "else" => TokenKind::Else,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "derive" => TokenKind::Derive,
            "mut" => TokenKind::Mut,
            "defer" => TokenKind::Defer,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(ident.to_string()),
        };
        self.token(kind)
    }
}
