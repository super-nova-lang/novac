use crate::parser::ast::*;
use std::borrow::Cow;

/// Annotated program with all types filled in
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedProgram<'de> {
    pub items: Vec<AnnotatedTopLevelItem<'de>>,
}

/// Annotated top-level items
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotatedTopLevelItem<'de> {
    Function(AnnotatedFunction<'de>),
    VariableDecl(AnnotatedVariableDecl<'de>),
    TypeDecl(TypeDecl<'de>), // Type declarations don't need annotation
}

/// Annotated function with inferred types
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedFunction<'de> {
    pub name: Cow<'de, str>,
    pub generics: Vec<Cow<'de, str>>,
    pub params: Vec<AnnotatedFunctionParam<'de>>,
    pub return_type: Type<'de>, // Always present after analysis
    pub body: AnnotatedExprList<'de>,
}

/// Annotated function parameter with inferred type
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedFunctionParam<'de> {
    pub name: Cow<'de, str>,
    pub type_annotation: Type<'de>, // Always present after analysis
}

/// Annotated variable declaration with inferred type
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedVariableDecl<'de> {
    pub name: Cow<'de, str>,
    pub type_annotation: Type<'de>, // Always present after analysis
    pub value: Box<AnnotatedExpr<'de>>,
}

/// Annotated expression list
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotatedExprList<'de> {
    Single(Box<AnnotatedExpr<'de>>),
    Block(Vec<AnnotatedStmt<'de>>),
}

/// Annotated statement
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotatedStmt<'de> {
    VariableDecl(AnnotatedVariableDecl<'de>),
    Expr(Box<AnnotatedExpr<'de>>),
    Return(Option<Box<AnnotatedExpr<'de>>>),
}

/// Annotated expression with type information
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedExpr<'de> {
    pub expr: Expr<'de>,
    pub ty: Type<'de>, // Inferred type
}

impl<'de> AnnotatedExpr<'de> {
    pub fn new(expr: Expr<'de>, ty: Type<'de>) -> Self {
        Self { expr, ty }
    }
}
