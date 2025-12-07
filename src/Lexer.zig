const std = @import("std");

path: []const u8,
content: []const u8,
cur: usize = 0,

pub const Token = union(enum) {
    // Keywords
    Const, // const
    Let, // let
    Fn, // fn
    Struct, // struct
    Enum, // enum
    Contract, // contract
    Impl, // impl
    Return, // return
    Priv, // priv
    Pub, // pub

    // Tokens
    Arrow, // ->
    FArrow, // =>

    FColon, // ::
    Colon, // :
    Semi, // ;

    Dot, // .
    Commma, // ,

    Eql, // =
    DubEql, // ==
    NotEql, // !=

    ShapeEql, // <>
    ShapeNotEql, // <!>

    Less, // <
    LessEql, // <=
    Greater, // >
    GreaterEql, // >=

    Plus, // +
    PlusEql, // +=
    Minus, // -
    MinusEql, // -=
    Star, // *
    StarEql, // *=
    Slash, // /
    SlashEql, // /=
    Pow, // ^
    PowEql, // ^=

    OParen, // (
    CParen, // )
    OSquar, // [
    CSquar, // ]
    OBrace, // {
    CBrace, // }

    Bang, // !
    Quest, // ?

    And, // &&

    // Variadic
    Ident: []const u8,
    Number: u32,
    String: []const u8,

    Eof,
};

const Self = @This();

pub fn next(_: *Self) !Token {
    return error.unimplemented;
}
