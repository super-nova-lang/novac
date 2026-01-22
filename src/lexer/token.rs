use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub origin: &'de str,
    pub offset: usize,
    pub kind: TokenKind,
}

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> Cow<'de, str> {
        // TODO: omg this sucks
        Cow::Borrowed(s.trim_matches('"'))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Keywords
    And,    // and
    Elif,   // elif
    Else,   // else
    Enum,   // enum
    If,     // if
    Let,    // let
    Or,     // or
    Return, // return
    Struct, // struct
    With,   // with
    True,   // true
    False,  // false
    Nil,    // nil
    // Tokens
    At,          // @
    Bang,        // !
    BangEqual,   // !=
    Bar,         // |
    Carrot,      // ^
    Colon,       // :
    Comma,       // ,
    Concat,      // <>
    Dot,         // .
    DoubleColon, // ::
    DoubleEqual, // ==
    Equal,       // =
    GrtrEqual,   // >=
    GrtrThan,    // >
    LeftBrace,   // {
    LeftParen,   // (
    LeftSquare,  // [
    LessEqual,   // <=
    LessThan,    // <
    Minus,       // -
    Perc,        // %
    Pipe,        // |>
    Plus,        // +
    Pound,       // #
    RArrow,      // ->
    RightBrace,  // }
    RightParen,  // )
    RightSquare, // ]
    SemiColon,   // ;
    Slash,       // /
    Star,        // *
    Spread,      // ..
    // Literals
    Ident,
    NumberLit(u64),
    // FloatLit(f64),
    StringLit,
    CharLit(char),
}
