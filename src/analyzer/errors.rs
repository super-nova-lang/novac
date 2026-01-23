use crate::parser::ast::Type;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Type mismatch: expected {expected}, found {actual}")]
pub struct TypeMismatchError {
    pub expected: String,
    pub actual: String,
    #[label("expected {expected} here")]
    pub expected_span: SourceSpan,
    #[label("but found {actual}")]
    pub actual_span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Undefined variable: {name}")]
pub struct UndefinedVariableError {
    pub name: String,
    #[label("variable `{name}` is not defined")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Undefined type: {name}")]
pub struct UndefinedTypeError {
    pub name: String,
    #[label("type `{name}` is not defined")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Cannot infer type: {message}")]
pub struct TypeInferenceError {
    pub message: String,
    #[label("{message}")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Invalid operation: {operation} is not valid for type {type_name}")]
pub struct InvalidOperationError {
    pub operation: String,
    pub type_name: String,
    #[label("operation `{operation}` cannot be applied to {type_name}")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Function not found: {name}")]
pub struct FunctionNotFoundError {
    pub name: String,
    #[label("function `{name}` is not defined")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Wrong number of arguments: expected {expected}, found {actual}")]
pub struct WrongArgumentCountError {
    pub expected: usize,
    pub actual: usize,
    #[label("expected {expected} arguments, found {actual}")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Return type mismatch: function returns {expected}, but found {actual}")]
pub struct ReturnTypeMismatchError {
    pub expected: String,
    pub actual: String,
    #[label("function should return {expected}")]
    pub expected_span: SourceSpan,
    #[label("but found {actual}")]
    pub actual_span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Missing return value: function expects return type {expected}")]
pub struct MissingReturnValueError {
    pub expected: String,
    #[label("function should return {expected}")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Unknown builtin: {name}")]
pub struct UnknownBuiltinError {
    pub name: String,
    #[label("unknown builtin `@{name}`")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Wrong number of arguments for builtin @{name}: expected {expected}, found {actual}")]
pub struct WrongBuiltinArgumentCountError {
    pub name: String,
    pub expected: usize,
    pub actual: usize,
    #[label("builtin `@{name}` expects {expected} arguments, found {actual}")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Wrong argument type for builtin @{name}: expected {expected}, found {actual}")]
pub struct WrongBuiltinArgumentTypeError {
    pub name: String,
    pub expected: String,
    pub actual: String,
    #[label("builtin `@{name}` expects {expected}, found {actual}")]
    pub span: SourceSpan,
}

// Helper function to format types for error messages
pub fn format_type(ty: &Type) -> String {
    use crate::parser::ast::PrimitiveType;
    match ty {
        Type::Primitive(p) => {
            let s = match p {
                PrimitiveType::I8 => "i8",
                PrimitiveType::I16 => "i16",
                PrimitiveType::I32 => "i32",
                PrimitiveType::I64 => "i64",
                PrimitiveType::Isize => "isize",
                PrimitiveType::U8 => "u8",
                PrimitiveType::U16 => "u16",
                PrimitiveType::U32 => "u32",
                PrimitiveType::U64 => "u64",
                PrimitiveType::Usize => "usize",
                PrimitiveType::Str => "str",
                PrimitiveType::Char => "char",
                PrimitiveType::Bool => "bool",
                PrimitiveType::Nil => "nil",
                PrimitiveType::List => "list",
            };
            s.to_string()
        }
        Type::Named(n) => n.to_string(),
        Type::Generic { base, args } => {
            let base_str = format_type(base);
            let args_str = args
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", base_str, args_str)
        }
        Type::Function { params } => {
            let params_str = params
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("|{}|", params_str)
        }
        Type::Tuple(types) => {
            let types_str = types
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", types_str)
        }
        Type::AnonymousStruct(fields) => {
            let fields_str = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, format_type(&f.type_)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("struct {{ {} }}", fields_str)
        }
    }
}
