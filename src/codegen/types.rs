use crate::analyzer::annotated::*;
use crate::parser::ast::*;
use inkwell::types::*;
use miette::{miette, Result};
use std::collections::HashMap;

/// Convert a language type to an LLVM type
pub fn type_to_llvm_type<'ctx>(
    ty: &Type,
    context: &'ctx inkwell::context::Context,
    struct_types: &HashMap<String, StructType<'ctx>>,
    enum_types: &HashMap<String, StructType<'ctx>>,
) -> Result<BasicTypeEnum<'ctx>> {
    match ty {
        Type::Primitive(prim) => primitive_to_llvm_type(*prim, context),
        Type::Named(name) => {
            let name_str = name.as_ref();
            // Check if it's a struct type
            if let Some(struct_ty) = struct_types.get(name_str) {
                Ok(struct_ty.as_basic_type_enum())
            } else if let Some(enum_ty) = enum_types.get(name_str) {
                Ok(enum_ty.as_basic_type_enum())
            } else {
                Err(miette!("Unknown type: {}", name_str))
            }
        }
        Type::Tuple(types) => {
            let llvm_types: Result<Vec<_>> = types
                .iter()
                .map(|t| type_to_llvm_type(t, context, struct_types, enum_types))
                .collect();
            let struct_ty = context.opaque_struct_type("tuple");
            struct_ty.set_body(
                &llvm_types?
                    .iter()
                    .map(|t| t.into())
                    .collect::<Vec<_>>(),
                false,
            );
            Ok(struct_ty.as_basic_type_enum())
        }
        Type::Function { params } => {
            // Function pointer type
            let param_types: Result<Vec<_>> = params
                .iter()
                .map(|p| type_to_llvm_type(p, context, struct_types, enum_types))
                .collect();
            let fn_type = context.void_type().fn_type(
                &param_types?
                    .iter()
                    .map(|t| t.into())
                    .collect::<Vec<_>>(),
                false,
            );
            Ok(fn_type.as_basic_type_enum())
        }
        Type::Generic { base, args: _ } => {
            // For now, handle generics by using the base type
            // TODO: Proper generic handling
            type_to_llvm_type(base, context, struct_types, enum_types)
        }
        Type::AnonymousStruct(fields) => {
            let field_types: Result<Vec<_>> = fields
                .iter()
                .map(|f| type_to_llvm_type(&f.type_, context, struct_types, enum_types))
                .collect();
            let struct_ty = context.opaque_struct_type("anon_struct");
            struct_ty.set_body(
                &field_types?
                    .iter()
                    .map(|t| t.into())
                    .collect::<Vec<_>>(),
                false,
            );
            Ok(struct_ty.as_basic_type_enum())
        }
    }
}

/// Convert a primitive type to LLVM type
fn primitive_to_llvm_type<'ctx>(
    prim: PrimitiveType,
    context: &'ctx inkwell::context::Context,
) -> Result<BasicTypeEnum<'ctx>> {
    match prim {
        PrimitiveType::I8 => Ok(context.i8_type().as_basic_type_enum()),
        PrimitiveType::I16 => Ok(context.i16_type().as_basic_type_enum()),
        PrimitiveType::I32 => Ok(context.i32_type().as_basic_type_enum()),
        PrimitiveType::I64 => Ok(context.i64_type().as_basic_type_enum()),
        PrimitiveType::Isize => {
            // Use i64 for isize (64-bit platform)
            Ok(context.i64_type().as_basic_type_enum())
        }
        PrimitiveType::U8 => Ok(context.i8_type().as_basic_type_enum()),
        PrimitiveType::U16 => Ok(context.i16_type().as_basic_type_enum()),
        PrimitiveType::U32 => Ok(context.i32_type().as_basic_type_enum()),
        PrimitiveType::U64 => Ok(context.i64_type().as_basic_type_enum()),
        PrimitiveType::Usize => {
            // Use i64 for usize (64-bit platform)
            Ok(context.i64_type().as_basic_type_enum())
        }
        PrimitiveType::Str => {
            // String is i8* (pointer to i8)
            Ok(context.i8_type().ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum())
        }
        PrimitiveType::Char => Ok(context.i8_type().as_basic_type_enum()),
        PrimitiveType::Bool => Ok(context.bool_type().as_basic_type_enum()),
        PrimitiveType::Nil => {
            // Nil is void
            Err(miette!("Cannot convert nil type to LLVM type (use void function return type)"))
        }
        PrimitiveType::List => {
            // List is a pointer to a struct containing data
            // For now, use a generic pointer
            Err(miette!("List type not yet implemented"))
        }
    }
}

/// Get LLVM return type (void for nil, otherwise the type)
pub fn return_type_to_llvm_type<'ctx>(
    ty: &Type,
    context: &'ctx inkwell::context::Context,
    struct_types: &HashMap<String, StructType<'ctx>>,
    enum_types: &HashMap<String, StructType<'ctx>>,
) -> Result<inkwell::types::BasicMetadataTypeEnum<'ctx>> {
    match ty {
        Type::Primitive(PrimitiveType::Nil) => {
            Ok(context.void_type().as_basic_metadata_enum())
        }
        _ => {
            let basic_ty = type_to_llvm_type(ty, context, struct_types, enum_types)?;
            Ok(basic_ty.as_basic_metadata_enum())
        }
    }
}
