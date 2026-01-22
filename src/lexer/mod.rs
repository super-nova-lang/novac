#![allow(dead_code, unused_assignments)]

use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use thiserror::Error;
use token::*;

pub mod token;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token '{token}'")]
pub struct SingleTokenError {
    #[source_code]
    src: String,

    pub token: char,

    #[label = "this input character"]
    err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringTerminationError {
    #[source_code]
    src: String,

    #[label = "this string literal"]
    err_span: SourceSpan,
}

impl StringTerminationError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated comment")]
pub struct CommentTerminationError {
    #[source_code]
    src: String,

    #[label = "this comment"]
    err_span: SourceSpan,
}

impl CommentTerminationError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Debug)]
pub struct Lexer<'de> {
    pub whole: &'de str,
    pub rest: &'de str,
    pub byte: usize,
    pub peeked: Option<Result<Token<'de>, miette::Error>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }
}

impl<'de> Lexer<'de> {
    pub fn expect(
        &mut self,
        expected: TokenKind,
        unexpected: &str,
    ) -> Result<Token<'de>, miette::Error> {
        self.expect_where(|next| next.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'de>, miette::Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                ],
                help = format!("Expected {token:?}"),
                "{unexpected}",
            }
            .with_source_code(self.whole.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(Eof.into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, miette::Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        loop {
            // NOTE: this must be in the loop for the indices to match up with c_onwards
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                Ident,
                Less,
                Number,
                Slash,
                String,
                Char,
                IfCharElse(char, TokenKind, TokenKind),
            }

            let just = move |kind: TokenKind| {
                Some(Ok::<Token<'_>, Error>(Token {
                    kind,
                    offset: c_at,
                    origin: c_str,
                }))
            };

            let started = match c {
                ':' => Started::IfCharElse(':', TokenKind::DoubleColon, TokenKind::Colon),
                '=' => Started::IfCharElse('=', TokenKind::DoubleEqual, TokenKind::Equal),
                '>' => Started::IfCharElse('=', TokenKind::GrtrEqual, TokenKind::GrtrThan),
                '!' => Started::IfCharElse('=', TokenKind::BangEqual, TokenKind::Bang),
                '|' => Started::IfCharElse('>', TokenKind::Pipe, TokenKind::Bar),
                '-' => Started::IfCharElse('>', TokenKind::RArrow, TokenKind::Minus),
                '.' => Started::IfCharElse('.', TokenKind::Spread, TokenKind::Dot),
                '<' => Started::Less,
                '"' => Started::String,
                '\'' => Started::Char,
                '/' => Started::Slash,
                '@' => return just(TokenKind::At),
                '#' => return just(TokenKind::Pound),
                '^' => return just(TokenKind::Carrot),
                ',' => return just(TokenKind::Comma),
                '{' => return just(TokenKind::LeftBrace),
                '(' => return just(TokenKind::LeftParen),
                '[' => return just(TokenKind::LeftSquare),
                '%' => return just(TokenKind::Perc),
                '+' => return just(TokenKind::Plus),
                '}' => return just(TokenKind::RightBrace),
                ')' => return just(TokenKind::RightParen),
                ']' => return just(TokenKind::RightSquare),
                ';' => return just(TokenKind::SemiColon),
                '*' => return just(TokenKind::Star),
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()));
                }
            };

            break match started {
                Started::String => {
                    if let Some(end) = self.rest.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        Some(Ok(Token {
                            origin: literal,
                            offset: c_at,
                            kind: TokenKind::StringLit,
                        }))
                    } else {
                        let err = StringTerminationError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        };

                        // swallow the remainder of input as being a string
                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];

                        return Some(Err(err.into()));
                    }
                }
                Started::Char => {
                    if let Some(end) = self.rest.find('\'') {
                        let literal = &c_onwards[..end + 1 + 1];
                        let char_content = &literal[1..literal.len() - 1];
                        let ch = if char_content.len() == 1 {
                            char_content.chars().next().unwrap()
                        } else {
                            // Handle escape sequences if needed
                            char_content.chars().next().unwrap()
                        };
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        Some(Ok(Token {
                            origin: literal,
                            offset: c_at,
                            kind: TokenKind::CharLit(ch),
                        }))
                    } else {
                        // Unterminated char literal
                        let err = StringTerminationError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(c_at..self.whole.len()),
                        };
                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(err.into()));
                    }
                }
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        // this is a comment!
                        let line_end = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.byte += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    } else if self.rest.starts_with('*') {
                        /* this is also a comment! */
                        // consume the '*'
                        self.rest = &self.rest[1..];
                        self.byte += 1;

                        // find the closing "*/"
                        if let Some(end) = self.rest.find("*/") {
                            // skip to after "*/"
                            self.byte += end + 2;
                            self.rest = &self.rest[end + 2..];
                            continue;
                        } else {
                            // unterminated comment
                            let err = CommentTerminationError {
                                src: self.whole.to_string(),
                                err_span: SourceSpan::from(c_at..self.whole.len()),
                            };

                            // swallow the remainder of input as being a comment
                            self.byte += self.rest.len();
                            self.rest = &self.rest[self.rest.len()..];

                            return Some(Err(err.into()));
                        }
                    } else {
                        Some(Ok(Token {
                            origin: c_str,
                            offset: c_at,
                            kind: TokenKind::Slash,
                        }))
                    }
                }
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or(c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "elif" => TokenKind::Elif,
                        "else" => TokenKind::Else,
                        "enum" => TokenKind::Enum,
                        "if" => TokenKind::If,
                        "let" => TokenKind::Let,
                        "or" => TokenKind::Or,
                        "return" => TokenKind::Return,
                        "struct" => TokenKind::Struct,
                        "with" => TokenKind::With,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        "nil" => TokenKind::Nil,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token {
                        origin: literal,
                        offset: c_at,
                        kind,
                    }));
                }
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or(c_onwards.len());

                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            literal = &literal[..one.len()];
                        }
                        _ => {
                            // leave literal as-is
                        }
                    }
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric literal"),
                                ],
                                "{e}",
                            }.with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        origin: literal,
                        offset: c_at,
                        kind: TokenKind::NumberLit(n),
                    }));
                }
                Started::IfCharElse(check, yes, no) => {
                    if self.rest.starts_with(check) {
                        let span = &c_onwards[..c.len_utf8() + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        Some(Ok(Token {
                            origin: span,
                            offset: c_at,
                            kind: yes,
                        }))
                    } else {
                        Some(Ok(Token {
                            origin: c_str,
                            offset: c_at,
                            kind: no,
                        }))
                    }
                }
                Started::Less => {
                    // Check the immediate next character without skipping whitespace.
                    if let Some(next_ch) = self.rest.chars().next() {
                        if next_ch == '>' {
                            // concat operator "<>"
                            let span = &c_onwards[..c.len_utf8() + next_ch.len_utf8()];
                            self.rest = &self.rest[next_ch.len_utf8()..];
                            self.byte += next_ch.len_utf8();
                            Some(Ok(Token {
                                origin: span,
                                offset: c_at,
                                kind: TokenKind::Concat,
                            }))
                        } else if next_ch == '=' {
                            // <= operator
                            let span = &c_onwards[..c.len_utf8() + next_ch.len_utf8()];
                            self.rest = &self.rest[next_ch.len_utf8()..];
                            self.byte += next_ch.len_utf8();
                            Some(Ok(Token {
                                origin: span,
                                offset: c_at,
                                kind: TokenKind::LessEqual,
                            }))
                        } else {
                            // simple less-than
                            Some(Ok(Token {
                                origin: c_str,
                                offset: c_at,
                                kind: TokenKind::LessThan,
                            }))
                        }
                    } else {
                        Some(Ok(Token {
                            origin: c_str,
                            offset: c_at,
                            kind: TokenKind::LessThan,
                        }))
                    }
                }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ident_and_symbols() {
        let input = "let add :: a, b = a + b";
        let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Let,
                TokenKind::Ident,
                TokenKind::DoubleColon,
                TokenKind::Ident,
                TokenKind::Comma,
                TokenKind::Ident,
                TokenKind::Equal,
                TokenKind::Ident,
                TokenKind::Plus,
                TokenKind::Ident,
            ]
        );
        assert_eq!(tokens[1].origin, "add");
        assert_eq!(tokens[3].origin, "a");
        assert_eq!(tokens[5].origin, "b");
    }

    #[test]
    fn test_string_and_char_literals() {
        let input = "\"hello\" 'c'";
        let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
        assert_eq!(tokens[1].kind, TokenKind::CharLit('c'));
        let unescaped = token::Token::unescape(tokens[0].origin);
        assert_eq!(unescaped, "hello");
    }

    #[test]
    fn test_number_literals() {
        let input = "42 0 12345";
        let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::NumberLit(42));
        assert_eq!(tokens[1].kind, TokenKind::NumberLit(0));
        assert_eq!(tokens[2].kind, TokenKind::NumberLit(12345));
    }

    #[test]
    fn test_comments_are_skipped() {
        let input = "// line comment\nlet x = 1 /* block comment */ let y = 2";
        let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
        // Ensure comments are skipped: we expect two `let` keywords and identifiers `x` and `y`.
        let let_count = tokens.iter().filter(|t| t.kind == TokenKind::Let).count();
        assert_eq!(let_count, 2);
        assert!(tokens.iter().any(|t| t.origin == "x"));
        assert!(tokens.iter().any(|t| t.origin == "y"));
    }

    #[test]
    fn test_multichar_tokens() {
        let input = ":: == >= |> -> .. <> <= !=";
        let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::DoubleColon,
                TokenKind::DoubleEqual,
                TokenKind::GrtrEqual,
                TokenKind::Pipe,
                TokenKind::RArrow,
                TokenKind::Spread,
                TokenKind::Concat,
                TokenKind::LessEqual,
                TokenKind::BangEqual,
            ]
        );
    }

    #[test]
    fn test_peek_and_expect() {
        let mut l = Lexer::new("let a");
        let p = l.peek().unwrap().as_ref().unwrap();
        assert_eq!(p.kind, TokenKind::Let);
        let tok = l.expect(TokenKind::Let, "let").unwrap();
        assert_eq!(tok.kind, TokenKind::Let);
        let next = l.next().unwrap().unwrap();
        assert_eq!(next.kind, TokenKind::Ident);
    }

    #[test]
    fn test_unterminated_string_error() {
        let mut l = Lexer::new("\"no end");
        match l.next() {
            Some(Err(e)) => assert!(format!("{}", e).contains("Unterminated string")),
            _ => panic!("expected unterminated string error"),
        }
    }

    #[test]
    fn test_unterminated_comment_error() {
        let mut l = Lexer::new("/* unfinished");
        // First character is '/', so next() will return error for unterminated comment
        match l.next() {
            Some(Err(e)) => assert!(format!("{}", e).contains("Unterminated comment")),
            _ => panic!("expected unterminated comment error"),
        }
    }
}
