#![allow(dead_code)]
use std::borrow::Cow;

/// Top-level program structure
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'de> {
    pub items: Vec<TopLevelItem<'de>>,
}

/// Top-level items in a program
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelItem<'de> {
    Function(Function<'de>),
    VariableDecl(VariableDecl<'de>),
    TypeDecl(TypeDecl<'de>),
}

/// Function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Function<'de> {
    pub name: Cow<'de, str>,
    pub generics: Vec<Cow<'de, str>>,
    pub params: Vec<FunctionParam<'de>>,
    pub return_type: Option<Type<'de>>,
    pub body: ExprList<'de>,
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam<'de> {
    pub name: Cow<'de, str>,
    pub type_annotation: Option<Type<'de>>,
}

/// Variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl<'de> {
    pub name: Cow<'de, str>,
    pub type_annotation: Option<Type<'de>>,
    pub value: Box<Expr<'de>>,
}

/// Type declaration (struct or enum)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl<'de> {
    pub attrs: Vec<Attr<'de>>,
    pub name: Cow<'de, str>,
    pub decl: TypeDeclKind<'de>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclKind<'de> {
    Struct(StructDecl<'de>),
    Enum(EnumDecl<'de>),
}

/// Attribute
#[derive(Debug, Clone, PartialEq)]
pub struct Attr<'de> {
    pub name: Cow<'de, str>,
    pub value: Option<Box<Expr<'de>>>,
}

/// Struct declaration
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl<'de> {
    pub fields: Vec<StructField<'de>>,
    pub impl_block: Option<Vec<Function<'de>>>,
}

/// Struct field
#[derive(Debug, Clone, PartialEq)]
pub struct StructField<'de> {
    pub name: Cow<'de, str>,
    pub type_: Type<'de>,
}

/// Enum declaration
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl<'de> {
    pub variants: Vec<EnumVariant<'de>>,
}

/// Enum variant
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant<'de> {
    pub attrs: Vec<Attr<'de>>,
    pub name: Cow<'de, str>,
    pub type_: Option<Type<'de>>,
}

/// Type
#[derive(Debug, Clone, PartialEq)]
pub enum Type<'de> {
    Primitive(PrimitiveType),
    Named(Cow<'de, str>),
    Generic {
        base: Box<Type<'de>>,
        args: Vec<Type<'de>>,
    },
    Function {
        params: Vec<Type<'de>>,
    },
    Tuple(Vec<Type<'de>>),
}

/// Primitive type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    Str,
    Char,
    Nil,
    List,
}

/// Expression list (single expression or block)
#[derive(Debug, Clone, PartialEq)]
pub enum ExprList<'de> {
    Single(Box<Expr<'de>>),
    Block(Vec<Stmt<'de>>),
}

/// Statement
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'de> {
    VariableDecl(VariableDecl<'de>),
    Expr(Box<Expr<'de>>),
    Return(Option<Box<Expr<'de>>>),
}

/// Expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'de> {
    Binary {
        left: Box<Expr<'de>>,
        op: BinOp,
        right: Box<Expr<'de>>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr<'de>>,
    },
    Literal(Literal<'de>),
    Ident(Cow<'de, str>),
    Call {
        callee: Box<Expr<'de>>,
        args: Vec<Expr<'de>>,
    },
    MethodCall {
        receiver: Box<Expr<'de>>,
        method: Cow<'de, str>,
        args: Vec<Expr<'de>>,
    },
    Member {
        object: Box<Expr<'de>>,
        field: MemberField<'de>,
    },
    StructLit {
        type_: Option<Type<'de>>,
        fields: Vec<StructFieldInit<'de>>,
    },
    EnumVariant {
        name: Cow<'de, str>,
        value: Option<EnumVariantValue<'de>>,
    },
    AnonFn {
        params: Vec<Cow<'de, str>>,
        body: ExprList<'de>,
    },
    Match {
        expr: Box<Expr<'de>>,
        arms: Vec<MatchArm<'de>>,
    },
    If {
        condition: Box<Expr<'de>>,
        then_block: Vec<Stmt<'de>>,
        elif_blocks: Vec<ElifBlock<'de>>,
        else_block: Option<Vec<Stmt<'de>>>,
    },
    List(Vec<Expr<'de>>),
    Tuple(Vec<Expr<'de>>),
    Paren(Box<Expr<'de>>),
}

/// Binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    // Comparison
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    // String
    Concat,
    // Logical
    And,
    Or,
}

/// Unary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// Member field access (by name or index)
#[derive(Debug, Clone, PartialEq)]
pub enum MemberField<'de> {
    Name(Cow<'de, str>),
    Index(u64),
}

/// Struct field initialization
#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldInit<'de> {
    pub name: Cow<'de, str>,
    pub value: Box<Expr<'de>>,
}

/// Enum variant value
#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantValue<'de> {
    Single(Box<Expr<'de>>),
    Tuple(Vec<Expr<'de>>),
}

/// Match arm
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm<'de> {
    pub patterns: Vec<Pattern<'de>>,
    pub guard: Option<Box<Expr<'de>>>,
    pub body: Box<Expr<'de>>,
}

/// Elif block
#[derive(Debug, Clone, PartialEq)]
pub struct ElifBlock<'de> {
    pub condition: Box<Expr<'de>>,
    pub body: Vec<Stmt<'de>>,
}

/// Pattern
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<'de> {
    Literal(Literal<'de>),
    Ident(Cow<'de, str>),
    Wildcard,
    List(ListPattern<'de>),
    Str(StrPattern<'de>),
    Struct(StructPattern<'de>),
    Enum(EnumPattern<'de>),
    Tuple(Vec<Pattern<'de>>),
}

/// List pattern
#[derive(Debug, Clone, PartialEq)]
pub enum ListPattern<'de> {
    Empty,
    Single(Box<Pattern<'de>>),
    HeadRest {
        head: Box<Pattern<'de>>,
    },
    RestTail {
        tail: Box<Pattern<'de>>,
    },
    HeadRestTail {
        head: Box<Pattern<'de>>,
        tail: Box<Pattern<'de>>,
    },
    Exact(Vec<Pattern<'de>>),
}

/// String pattern
#[derive(Debug, Clone, PartialEq)]
pub enum StrPattern<'de> {
    Ident(Cow<'de, str>),
    Segments {
        head: Cow<'de, str>,
        delimiter: char,
        tail: Cow<'de, str>,
    },
}

/// Struct pattern
#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern<'de> {
    pub type_: Option<Cow<'de, str>>,
    pub fields: Vec<StructFieldPat<'de>>,
}

/// Struct field pattern
#[derive(Debug, Clone, PartialEq)]
pub enum StructFieldPat<'de> {
    Named {
        name: Cow<'de, str>,
        pattern: Box<Pattern<'de>>,
    },
    Shorthand(Cow<'de, str>),
}

/// Enum pattern
#[derive(Debug, Clone, PartialEq)]
pub struct EnumPattern<'de> {
    pub name: Cow<'de, str>,
    pub value: Option<EnumPatternValue<'de>>,
}

/// Enum pattern value
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPatternValue<'de> {
    Single(Box<Pattern<'de>>),
    Tuple(Vec<Pattern<'de>>),
}

/// Literal
#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'de> {
    Number(u64),
    String(Cow<'de, str>),
    Char(char),
    Boolean(bool),
    Nil,
    BuiltinCall(BuiltinCall<'de>),
}

/// Builtin call
#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinCall<'de> {
    pub name: Cow<'de, str>,
    pub args: Option<Vec<Expr<'de>>>,
}
