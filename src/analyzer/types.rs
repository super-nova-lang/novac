use crate::analyzer::context::TypeContext;
use crate::analyzer::errors::{format_type, UndefinedTypeError};
use crate::parser::ast::{PrimitiveType, Type, TypeDeclKind};
use miette::{miette, Error, SourceSpan};

/// Check if a type is numeric
pub fn is_numeric(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            PrimitiveType::I8
                | PrimitiveType::I16
                | PrimitiveType::I32
                | PrimitiveType::I64
                | PrimitiveType::Isize
                | PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64
                | PrimitiveType::Usize
        )
    )
}

/// Check if a type is boolean
pub fn is_boolean(ty: &Type) -> bool {
    matches!(ty, Type::Primitive(PrimitiveType::Bool))
}

/// Resolve a type, converting named types to their concrete definitions
pub fn resolve_type<'de>(
    ty: &Type<'de>,
    ctx: &TypeContext<'de>,
    span: SourceSpan,
) -> Result<Type<'de>, Error> {
    match ty {
        Type::Primitive(_) => Ok(ty.clone()),
            Type::Named(name) => {
                // Check if it's a primitive type name first
                let prim_opt = match name.as_ref() {
                    "i8" => Some(PrimitiveType::I8),
                    "i16" => Some(PrimitiveType::I16),
                    "i32" => Some(PrimitiveType::I32),
                    "i64" => Some(PrimitiveType::I64),
                    "isize" => Some(PrimitiveType::Isize),
                    "u8" => Some(PrimitiveType::U8),
                    "u16" => Some(PrimitiveType::U16),
                    "u32" => Some(PrimitiveType::U32),
                    "u64" => Some(PrimitiveType::U64),
                    "usize" => Some(PrimitiveType::Usize),
                    "str" => Some(PrimitiveType::Str),
                    "char" => Some(PrimitiveType::Char),
                    "bool" => Some(PrimitiveType::Bool),
                    "nil" => Some(PrimitiveType::Nil),
                    "list" => Some(PrimitiveType::List),
                    _ => None,
                };
                
                if let Some(prim) = prim_opt {
                    Ok(Type::Primitive(prim))
                } else if let Some(type_decl) = ctx.lookup_type(name.as_ref()) {
                    // Lookup the type declaration
                    match &type_decl.decl {
                        TypeDeclKind::Struct(_struct_decl) => {
                            // Return the struct type - we could create a resolved version
                            // For now, we'll keep it as Named since the struct definition is in ctx
                            Ok(ty.clone())
                        }
                        TypeDeclKind::Enum(_) => {
                            // Return the enum type
                            Ok(ty.clone())
                        }
                    }
                } else {
                    Err(UndefinedTypeError {
                        name: name.to_string(),
                        span,
                    }
                    .into())
                }
            }
        Type::Generic { base, args } => {
            let resolved_base = resolve_type(base, ctx, span)?;
            let resolved_args: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| resolve_type(arg, ctx, span))
                .collect();
            Ok(Type::Generic {
                base: Box::new(resolved_base),
                args: resolved_args?,
            })
        }
        Type::Function { params } => {
            let resolved_params: Result<Vec<_>, _> = params
                .iter()
                .map(|p| resolve_type(p, ctx, span))
                .collect();
            Ok(Type::Function {
                params: resolved_params?,
            })
        }
        Type::Tuple(types) => {
            let resolved_types: Result<Vec<_>, _> = types
                .iter()
                .map(|t| resolve_type(t, ctx, span))
                .collect();
            Ok(Type::Tuple(resolved_types?))
        }
        Type::AnonymousStruct(_fields) => {
            // Anonymous structs are already concrete
            Ok(ty.clone())
        }
    }
}

/// Check if two types are compatible
pub fn types_compatible<'de>(
    expected: &Type<'de>,
    actual: &Type<'de>,
    ctx: &TypeContext<'de>,
) -> bool {
    // Exact match
    if expected == actual {
        return true;
    }

    // Both are primitives - check if they're the same
    if let (Type::Primitive(e), Type::Primitive(a)) = (expected, actual) {
        return e == a;
    }

    // Both are numeric - they're compatible for arithmetic operations
    if is_numeric(expected) && is_numeric(actual) {
        return true;
    }

    // Both are named types - check if they resolve to the same type
    if let (Type::Named(e_name), Type::Named(a_name)) = (expected, actual) {
        if e_name == a_name {
            return true;
        }
        // Could check if they resolve to the same concrete type
    }

    // Generic types - check base and args
    if let (
        Type::Generic {
            base: e_base,
            args: e_args,
        },
        Type::Generic {
            base: a_base,
            args: a_args,
        },
    ) = (expected, actual)
    {
        if e_args.len() == a_args.len() {
            return types_compatible(e_base, a_base, ctx)
                && e_args
                    .iter()
                    .zip(a_args.iter())
                    .all(|(e, a)| types_compatible(e, a, ctx));
        }
    }

    // Tuple types
    if let (Type::Tuple(e_types), Type::Tuple(a_types)) = (expected, actual) {
        if e_types.len() == a_types.len() {
            return e_types
                .iter()
                .zip(a_types.iter())
                .all(|(e, a)| types_compatible(e, a, ctx));
        }
    }

    // Function types
    if let (Type::Function { params: e_params }, Type::Function { params: a_params }) =
        (expected, actual)
    {
        if e_params.len() == a_params.len() {
            return e_params
                .iter()
                .zip(a_params.iter())
                .all(|(e, a)| types_compatible(e, a, ctx));
        }
    }

    false
}

/// Get the default type for a numeric literal
pub fn default_numeric_type() -> Type<'static> {
    Type::Primitive(PrimitiveType::I32)
}

/// Infer the result type of a binary operation
pub fn binary_op_result_type<'de>(
    op: &crate::parser::ast::BinOp,
    left: &Type<'de>,
    right: &Type<'de>,
) -> Result<Type<'de>, Error> {
    use crate::parser::ast::BinOp;

    match op {
        // Arithmetic operations - result is numeric
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem | BinOp::Pow => {
            if is_numeric(left) && is_numeric(right) {
                // For now, return the left type (could do type promotion)
                Ok(left.clone())
            } else {
                Err(miette!(
                    "Arithmetic operations require numeric operands, got {} and {}",
                    format_type(left),
                    format_type(right)
                ))
            }
        }
        // Comparison operations - result is bool
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
            // Allow comparison of compatible types
            Ok(Type::Primitive(PrimitiveType::Bool))
        }
        // String concatenation
        BinOp::Concat => {
            if matches!(left, Type::Primitive(PrimitiveType::Str))
                && matches!(right, Type::Primitive(PrimitiveType::Str))
            {
                Ok(Type::Primitive(PrimitiveType::Str))
            } else {
                Err(miette!(
                    "String concatenation requires str operands, got {} and {}",
                    format_type(left),
                    format_type(right)
                ))
            }
        }
        // Logical operations
        BinOp::And | BinOp::Or => {
            // Both operands must be boolean
            if is_boolean(left) && is_boolean(right) {
                Ok(Type::Primitive(PrimitiveType::Bool))
            } else {
                Err(miette!(
                    "Logical operations require boolean operands, got {} and {}",
                    format_type(left),
                    format_type(right)
                ))
            }
        }
    }
}

/// Infer the result type of a unary operation
pub fn unary_op_result_type<'de>(
    op: &crate::parser::ast::UnaryOp,
    operand: &Type<'de>,
) -> Result<Type<'de>, Error> {
    use crate::parser::ast::UnaryOp;

    match op {
        UnaryOp::Neg => {
            if is_numeric(operand) {
                Ok(operand.clone())
            } else {
                Err(miette!(
                    "Negation requires numeric operand, got {}",
                    format_type(operand)
                ))
            }
        }
        UnaryOp::Not => {
            // Operand must be boolean
            if is_boolean(operand) {
                Ok(Type::Primitive(PrimitiveType::Bool))
            } else {
                Err(miette!(
                    "Logical negation requires boolean operand, got {}",
                    format_type(operand)
                ))
            }
        }
    }
}
