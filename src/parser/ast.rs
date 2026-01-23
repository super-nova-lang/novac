#![allow(dead_code)]
use std::borrow::Cow;
use std::fmt;

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
    AnonymousStruct(Vec<StructField<'de>>),
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
    Bool,
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
    Wildcard,
    Literal(Literal<'de>),
    Ident(Cow<'de, str>),
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

// Display implementations

impl<'de> fmt::Display for Program<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl<'de> fmt::Display for TopLevelItem<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TopLevelItem::Function(func) => write!(f, "{}", func),
            TopLevelItem::VariableDecl(var) => write!(f, "{}", var),
            TopLevelItem::TypeDecl(ty) => write!(f, "{}", ty),
        }
    }
}

impl<'de> fmt::Display for Function<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", generic)?;
            }
            write!(f, ">")?;
        }
        write!(f, " :: ")?;
        if self.params.is_empty() {
            write!(f, "()")?;
        } else {
            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", param)?;
            }
        }
        if let Some(return_type) = &self.return_type {
            write!(f, " -> {}", return_type)?;
        }
        write!(f, " = {}", self.body)
    }
}

impl<'de> fmt::Display for FunctionParam<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(ty) = &self.type_annotation {
            write!(f, ": {}", ty)?;
        }
        Ok(())
    }
}

impl<'de> fmt::Display for VariableDecl<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {}", self.name)?;
        if let Some(ty) = &self.type_annotation {
            write!(f, ": {} = ", ty)?;
        } else {
            write!(f, " := ")?;
        }
        write!(f, "{}", self.value)
    }
}

impl<'de> fmt::Display for TypeDecl<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in &self.attrs {
            write!(f, "{}", attr)?;
        }
        write!(f, "let {} := ", self.name)?;
        match &self.decl {
            TypeDeclKind::Struct(s) => write!(f, "{}", s),
            TypeDeclKind::Enum(e) => write!(f, "{}", e),
        }
    }
}

impl<'de> fmt::Display for Attr<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[{}", self.name)?;
        if let Some(value) = &self.value {
            write!(f, " := {}", value)?;
        }
        write!(f, "]")
    }
}

impl<'de> fmt::Display for StructDecl<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "struct {{")?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, "}}")?;
        if let Some(impl_block) = &self.impl_block {
            write!(f, " with {{")?;
            for func in impl_block {
                write!(f, "\n    {}", func)?;
            }
            write!(f, "\n}}")?;
        }
        Ok(())
    }
}

impl<'de> fmt::Display for StructField<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
    }
}

impl<'de> fmt::Display for EnumDecl<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {{")?;
        for (i, variant) in self.variants.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", variant)?;
        }
        write!(f, "}}")
    }
}

impl<'de> fmt::Display for EnumVariant<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in &self.attrs {
            write!(f, "{}", attr)?;
        }
        write!(f, "{}", self.name)?;
        if let Some(ty) = &self.type_ {
            write!(f, ": {}", ty)?;
        }
        Ok(())
    }
}

impl<'de> fmt::Display for Type<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Primitive(p) => write!(f, "{}", p),
            Type::Named(n) => write!(f, "{}", n),
            Type::Generic { base, args } => {
                write!(f, "{}<", base)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Type::Function { params } => {
                write!(f, "|")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "|")
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            Type::AnonymousStruct(fields) => {
                write!(f, "struct {{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::Isize => write!(f, "isize"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::Usize => write!(f, "usize"),
            PrimitiveType::Str => write!(f, "str"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Nil => write!(f, "nil"),
            PrimitiveType::List => write!(f, "list"),
        }
    }
}

impl<'de> fmt::Display for ExprList<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprList::Single(expr) => write!(f, "{}", expr),
            ExprList::Block(stmts) => {
                write!(f, "{{")?;
                for stmt in stmts {
                    write!(f, "\n    {}", stmt)?;
                }
                write!(f, "\n}}")
            }
        }
    }
}

impl<'de> fmt::Display for Stmt<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::VariableDecl(var) => write!(f, "{}", var),
            Stmt::Expr(expr) => write!(f, "{}", expr),
            Stmt::Return(Some(expr)) => write!(f, "return {}", expr),
            Stmt::Return(None) => write!(f, "return"),
        }
    }
}

impl<'de> fmt::Display for Expr<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            Expr::Unary { op, expr } => {
                write!(f, "{}{}", op, expr)
            }
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::Call { callee, args } => {
                write!(f, "{}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                write!(f, "{}:{}(", receiver, method)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Member { object, field } => {
                write!(f, "{}.{}", object, field)
            }
            Expr::StructLit { type_, fields } => {
                if let Some(ty) = type_ {
                    write!(f, "{}", ty)?;
                } else {
                    write!(f, ".")?;
                }
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
            Expr::EnumVariant { name, value } => {
                write!(f, ".{}", name)?;
                if let Some(val) = value {
                    match val {
                        EnumVariantValue::Single(expr) => write!(f, "({})", expr),
                        EnumVariantValue::Tuple(exprs) => {
                            write!(f, "(")?;
                            for (i, expr) in exprs.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{}", expr)?;
                            }
                            write!(f, ")")
                        }
                    }
                } else {
                    Ok(())
                }
            }
            Expr::AnonFn { params, body } => {
                write!(f, "|")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "| {}", body)
            }
            Expr::Match { expr, arms } => {
                write!(f, "match {} {{", expr)?;
                for arm in arms {
                    write!(f, "\n    {}", arm)?;
                }
                write!(f, "\n}}")
            }
            Expr::If {
                condition,
                then_block,
                elif_blocks,
                else_block,
            } => {
                write!(f, "if {} {{", condition)?;
                for stmt in then_block {
                    write!(f, "\n    {}", stmt)?;
                }
                write!(f, "\n}}")?;
                for elif_block in elif_blocks {
                    write!(f, " elif {} {{", elif_block.condition)?;
                    for stmt in &elif_block.body {
                        write!(f, "\n    {}", stmt)?;
                    }
                    write!(f, "\n}}")?;
                }
                if let Some(else_block) = else_block {
                    write!(f, " else {{")?;
                    for stmt in else_block {
                        write!(f, "\n    {}", stmt)?;
                    }
                    write!(f, "\n}}")?;
                }
                Ok(())
            }
            Expr::List(exprs) => {
                write!(f, "[")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, "]")
            }
            Expr::Tuple(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            Expr::Paren(expr) => write!(f, "({})", expr),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Rem => write!(f, "%"),
            BinOp::Pow => write!(f, "^"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Le => write!(f, "<="),
            BinOp::Ge => write!(f, ">="),
            BinOp::Concat => write!(f, "<>"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl<'de> fmt::Display for MemberField<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberField::Name(name) => write!(f, "{}", name),
            MemberField::Index(idx) => write!(f, "{}", idx),
        }
    }
}

impl<'de> fmt::Display for StructFieldInit<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{} = {}", self.name, self.value)
    }
}

impl<'de> fmt::Display for MatchArm<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|")?;
        for (i, pattern) in self.patterns.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", pattern)?;
        }
        if let Some(guard) = &self.guard {
            write!(f, " if {}", guard)?;
        }
        write!(f, " -> {}", self.body)
    }
}

impl<'de> fmt::Display for Pattern<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Literal(lit) => write!(f, "{}", lit),
            Pattern::Ident(name) => write!(f, "{}", name),
            Pattern::Wildcard => write!(f, "_"),
            Pattern::List(pat) => write!(f, "{}", pat),
            Pattern::Str(pat) => write!(f, "{}", pat),
            Pattern::Struct(pat) => write!(f, "{}", pat),
            Pattern::Enum(pat) => write!(f, "{}", pat),
            Pattern::Tuple(pats) => {
                write!(f, "(")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pat)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'de> fmt::Display for ListPattern<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ListPattern::Empty => write!(f, "[]"),
            ListPattern::Single(pat) => write!(f, "[{}]", pat),
            ListPattern::HeadRest { head } => write!(f, "[{}, ..]", head),
            ListPattern::RestTail { tail } => write!(f, "[..{}]", tail),
            ListPattern::HeadRestTail { head, tail } => {
                write!(f, "[{}, ..{}]", head, tail)
            }
            ListPattern::Exact(pats) => {
                write!(f, "[")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pat)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl<'de> fmt::Display for StrPattern<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StrPattern::Ident(name) => write!(f, "{}", name),
            StrPattern::Segments {
                head,
                delimiter,
                tail,
            } => {
                write!(f, "{} :: '{}' :: {}", head, delimiter, tail)
            }
        }
    }
}

impl<'de> fmt::Display for StructPattern<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ty) = &self.type_ {
            write!(f, "{}", ty)?;
        } else {
            write!(f, ".")?;
        }
        write!(f, "{{")?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, "}}")
    }
}

impl<'de> fmt::Display for StructFieldPat<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructFieldPat::Named { name, pattern } => {
                write!(f, ".{} = {}", name, pattern)
            }
            StructFieldPat::Shorthand(name) => write!(f, "{}", name),
        }
    }
}

impl<'de> fmt::Display for EnumPattern<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{}", self.name)?;
        if let Some(value) = &self.value {
            match value {
                EnumPatternValue::Single(pat) => write!(f, "({})", pat),
                EnumPatternValue::Tuple(pats) => {
                    write!(f, "(")?;
                    for (i, pat) in pats.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", pat)?;
                    }
                    write!(f, ")")
                }
            }
        } else {
            Ok(())
        }
    }
}

impl<'de> fmt::Display for Literal<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::Boolean(true) => write!(f, "true"),
            Literal::Boolean(false) => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
            Literal::BuiltinCall(builtin) => write!(f, "{}", builtin),
        }
    }
}

impl<'de> fmt::Display for BuiltinCall<'de> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)?;
        if let Some(args) = &self.args {
            write!(f, "(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ")")
        } else {
            Ok(())
        }
    }
}
