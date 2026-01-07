#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords
    Let,    // let
    Open,   // open
    Import, // import
    Module, // module
    Export, // export
    Match,  // match
    With,   // with
    As,     // as
    Return, // return
    If,     // if
    While,  // while
    For,    // for
    In,     // in
    Else,   // else
    Struct, // struct
    Enum,   // enum
    Derive, // derive
    Mut,    // mut
    Defer,  // defer
    True,   // true
    False,  // false
    // Double character tokens
    DoubleColon, // ::
    FatArrow,    // =>
    DoubleEql,   // ==
    NotEql,      // !=
    ShiftLeft,   // <<
    ShiftRight,  // >>
    PlusPlus,    // ++
    MinusMinus,  // --
    SkinnyArrow, // ->
    BackArrow,   // <-
    Pipe,        // |>
    Or,          // ||
    BitwiseOr,   // |.
    And,         // &&
    BitwiseAnd,  // &.
    LesserEql,   // <=
    GreaterEql,  // >=
    Ellipsis,    // ...
    Walrus,      // :=
    Concat,      // ~
    PlusEql,     // +=
    MinusEql,    // -=
    StarEql,     // *=
    SlashEql,    // /=
    // Single character tokens
    Tilde,              // ~
    BackTick,           // `
    Bang,               // !
    At,                 // @
    Hash,               // #
    Dollar,             // $
    Mod,                // %
    Carrot,             // ^
    Amper,              // &
    Star,               // *
    OpenParen,          // (
    CloseParen,         // )
    OpenBrack,          // {
    CloseBrack,         // }
    OpenSquare,         // [
    CloseSquare,        // ]
    LowDash,            // _
    Dash,               // -
    Plus,               // +
    Eql,                // =
    Bar,                // |
    BackSlash,          // \
    Colon,              // :
    SemiColon,          // ;
    Lesser,             // <
    Comma,              // ,
    Greater,            // >
    Dot,                // .
    Question,           // ?
    ForwardSlash,       // /
    String(String),     // "string"
    Char(char),         // 'c'
    DocComment(String), // (*! comment *)
    Ident(String),      // identifier
    Number(i32),        // 1234
    Unknown(char),      // unknown character
    Eof,                // end of file
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
