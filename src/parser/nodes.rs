#![allow(dead_code)]

use crate::lexer::token::Span;

#[derive(Clone, Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Box<Expression>),
    Error(String, Span),
}

/* STATEMENT */
#[derive(Clone, Debug)]
pub enum Statement {
    Open(OpenStmt),
    Decl(DeclStmt),
    Return(ReturnStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Expression(Box<Expression>),
}

#[derive(Clone, Debug)]
pub struct OpenStmt {
    pub mods: Vec<String>,
    pub elements: Vec<OpenStmtElement>,
}

#[derive(Clone, Debug)]
pub struct OpenStmtElement {
    pub path: Vec<String>,
    pub alias: Option<String>,
}

#[derive(Clone, Debug)]
pub enum ReturnStmt {
    WithExpr(Box<Expression>),
    Naked,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Box<Expression>,
    pub body: Vec<Node>,
    pub elif: ElseStmt,
}

#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub cond: Box<Expression>,
    pub body: Vec<Node>,
}

#[derive(Clone, Debug)]
pub enum ForStmt {
    ForIter(ForIterStmt),
    ForC(ForCStmt),
    ForTuple(ForTupleStmt),
}

#[derive(Clone, Debug)]
pub struct ForIterStmt {
    pub var: Ident,
    pub iterable: Box<Expression>,
    pub body: Vec<Node>,
}

#[derive(Clone, Debug)]
pub struct ForCStmt {
    pub var: Ident,
    pub init: Box<Expression>,
    pub cond: Box<Expression>,
    pub update: Box<Expression>,
    pub body: Vec<Node>,
}

#[derive(Clone, Debug)]
pub struct ForTupleStmt {
    pub vars: Vec<Ident>,
    pub iterable: Box<Expression>,
    pub body: Vec<Node>,
}

#[derive(Clone, Debug)]
pub enum ElseStmt {
    ElseIf(Box<Expression>, Vec<Node>, Box<ElseStmt>),
    Else(Vec<Node>),
    Nope,
}

#[derive(Clone, Debug)]
pub enum DeclStmt {
    Decl {
        doc: Option<String>,
        tags: Vec<Tag>,
        name: Ident,
        generics: Vec<Ident>,
        params: Vec<DeclParam>,
        explicit_ret: Option<Type>,
        body: DeclBody,
    },
    CurryDecl {
        doc: Option<String>,
        tags: Vec<Tag>,
        name: Ident,
        curried: Ident,
        input: Vec<Expression>,
    },
    ImportDecl {
        doc: Option<String>,
        name: Ident,
        calling_conf: String,
        link_name: String,
    },
    ModuleDecl {
        doc: Option<String>,
        name: Ident,
        exports: Vec<ExportStmt>,
        body: Vec<Node>,
    },
    ExportStmt(ExportStmt),
}

#[derive(Clone, Debug)]
pub enum ExportStmt {
    ExportIdent(Ident),
    ExportRename(Ident, Ident),
}

pub type DeclBody = (Vec<Statement>, Option<Box<Expression>>);

#[derive(Clone, Debug)]
pub enum DeclParam {
    Untyped(Ident),
    Typed(Ident, Type),
    OptionalTyped(Ident, Type, Box<Expression>),
    OptionalUntyped(Ident, Box<Expression>),
    Variadic(Ident, Option<Type>),
}

#[derive(Clone, Debug)]
pub enum Tag {
    TagName(Ident),
    TagCall(CallExpr),
}

/* EXPRESSION */
#[derive(Clone, Debug)]
pub enum Expression {
    CallExpr(CallExpr),
    RelationalExpr(RelationalExpr),
    AssignmentExpr(AssignmentExpr),
    ListExpr(Vec<Expression>),
    MatchExpr(MatchExpr),
    StructExpr(Vec<StructField>, Option<WithBlock>),
    EnumExpr(Vec<EnumVariant>, Option<WithBlock>),
    DeriveExpr(Vec<Node>),
}

#[derive(Clone, Debug)]
pub enum AssignmentExpr {
    AddAssign(Ident, Box<AdditiveExpr>),
    SubAssign(Ident, Box<AdditiveExpr>),
    MulAssign(Ident, Box<AdditiveExpr>),
    DivAssign(Ident, Box<AdditiveExpr>),
}

pub type WithBlock = Vec<Node>;
pub type StructField = (Ident, Type, Option<Box<Expression>>);
pub type EnumVariant = (Ident, Option<VariantBody>);

#[derive(Clone, Debug)]
pub enum VariantBody {
    StructBody(Vec<StructField>),
    TypeBody(Type),
}

#[derive(Clone, Debug)]
pub enum RelationalExpr {
    Eql(Box<AdditiveExpr>, Box<AdditiveExpr>),
    Neq(Box<AdditiveExpr>, Box<AdditiveExpr>),
    Lt(Box<AdditiveExpr>, Box<AdditiveExpr>),
    Gt(Box<AdditiveExpr>, Box<AdditiveExpr>),
    Leq(Box<AdditiveExpr>, Box<AdditiveExpr>),
    Geq(Box<AdditiveExpr>, Box<AdditiveExpr>),
    RelationalVal(Box<AdditiveExpr>),
}

#[derive(Clone, Debug)]
pub enum AdditiveExpr {
    Add(Box<AdditiveExpr>, Box<MultiplicativeExpr>),
    Sub(Box<AdditiveExpr>, Box<MultiplicativeExpr>),
    AdditiveVal(Box<MultiplicativeExpr>),
}

#[derive(Clone, Debug)]
pub enum MultiplicativeExpr {
    Mul(Box<MultiplicativeExpr>, Box<UnaryExpr>),
    Div(Box<MultiplicativeExpr>, Box<UnaryExpr>),
    Mod(Box<MultiplicativeExpr>, Box<UnaryExpr>),
    Pow(Box<MultiplicativeExpr>, Box<UnaryExpr>),
    MultiplicativeVal(Box<UnaryExpr>),
}

#[derive(Clone, Debug)]
pub enum UnaryExpr {
    Neg(Box<UnaryExpr>),
    Not(Box<UnaryExpr>),
    UnaryMember(Box<UnaryExpr>, Ident),
    UnaryCall(CallExpr),
    UnaryVal(Atom),
}

/* call */
#[derive(Clone, Debug)]
pub enum CallExpr {
    DeclCall(Box<Expression>, Vec<CallParam>),
}

#[derive(Clone, Debug)]
pub enum CallParam {
    Named(Ident, Box<Expression>),
    Positional(Box<Expression>),
}

/* match */
pub type MatchExpr = (Box<Expression>, Vec<MatchArm>);
pub type MatchArm = (MatchParam, Option<MatchIf>, MatchArmBody);
pub type MatchArmBody = (Vec<Statement>, Option<Box<Expression>>);

#[derive(Clone, Debug)]
pub enum MatchParam {
    PatWildcard,
    PatInt(i32),
    PatBool(bool),
    PatString(String),
    PatIdent(Ident),
    PatEnum(Ident, Ident, Vec<MatchParam>),
    PatTuple(Vec<MatchParam>),
    PatStruct(Vec<(Ident, MatchParam)>),
}

pub type MatchIf = Box<Expression>;

/* atoms */
#[derive(Clone, Debug)]
pub enum Atom {
    String(String),
    Bool(bool),
    Char(char),
    Int(i32),
    Ident(Ident),
    ImplicitMember(Ident),
    Grouping(Box<Expression>),
    UnitVal,
}

/* primitives */
pub type Ident = String;

#[derive(Clone, Debug)]
pub enum Type {
    User(Ident),
    Generic(Ident, Vec<Type>),
    TypeVar(Ident),
    Builtin(Ident),
    UnitTyp,
    ListTyp(Box<Type>),
}

impl DeclStmt {
    pub fn generics(&self) -> Option<Vec<Ident>> {
        match self {
            DeclStmt::Decl { generics, .. } => {
                if generics.is_empty() {
                    None
                } else {
                    Some(generics.clone())
                }
            }
            _ => None,
        }
    }
}
