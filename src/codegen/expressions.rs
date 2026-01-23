use crate::analyzer::*;
use crate::codegen::types::*;
use crate::codegen::builder::*;
use crate::parser::ast::*;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::*;
use miette::{miette, Result};

/// Generate code for an expression
pub fn codegen_expr(
    codegen: &mut crate::codegen::Codegen,
    expr: &AnnotatedExpr,
) -> Result<BasicValueEnum<'static>> {
    match &expr.expr {
        Expr::Literal(lit) => codegen_literal(codegen, lit, &expr.ty),
        Expr::Ident(name) => {
            let ptr = codegen.variables_mut().get(name.as_ref())
                .copied()
                .ok_or_else(|| miette!("Variable {} not found", name))?;
            let ty = type_to_llvm_type(
                &expr.ty,
                codegen.context(),
            &codegen.struct_types,
            &codegen.enum_types,
            )?;
            let value = codegen.builder().build_load(ty, ptr, name.as_ref())
                .map_err(|e| miette!("Failed to load variable: {}", e))?;
            Ok(value)
        }
        Expr::Binary { left, op, right } => {
            codegen_binary_op(codegen, left, *op, right, &expr.ty)
        }
        Expr::Unary { op, expr: e } => {
            codegen_unary_op(codegen, *op, e, &expr.ty)
        }
        Expr::Call { callee, args } => {
            codegen_call(codegen, callee, args)
        }
        Expr::MethodCall { receiver, method, args } => {
            codegen_method_call(codegen, receiver, method, args)
        }
        Expr::Member { object, field } => {
            codegen_member_access(codegen, object, field)
        }
        Expr::StructLit { type_, fields } => {
            codegen_struct_literal(codegen, type_.as_ref(), fields, &expr.ty)
        }
        Expr::EnumVariant { name, value } => {
            codegen_enum_variant(codegen, name, value.as_ref(), &expr.ty)
        }
        Expr::If { condition, then_block, elif_blocks, else_block } => {
            codegen_if(codegen, condition, then_block, elif_blocks, else_block.as_deref(), &expr.ty)
        }
        Expr::Match { expr: e, arms } => {
            codegen_match(codegen, e, arms, &expr.ty)
        }
        Expr::List(elements) => {
            codegen_list(codegen, elements, &expr.ty)
        }
        Expr::Tuple(elements) => {
            codegen_tuple(codegen, elements, &expr.ty)
        }
        Expr::Paren(expr) => codegen_expr(codegen, expr),
        Expr::AnonFn { params, body } => {
            Err(miette!("Anonymous functions not yet implemented"))
        }
    }
}

/// Generate code for a literal
fn codegen_literal(
    codegen: &mut crate::codegen::Codegen,
    lit: &Literal,
    ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    match lit {
        Literal::Number(n) => {
            let llvm_ty = type_to_llvm_type(
                ty,
                codegen.context(),
                &codegen.struct_types,
                &codegen.enum_types,
            )?;
            match llvm_ty {
                BasicTypeEnum::IntType(int_ty) => {
                    Ok(int_ty.const_int(*n as u64, false).into())
                }
                _ => Err(miette!("Number literal type mismatch")),
            }
        }
        Literal::String(s) => {
            // Create string constant
            let ptr = create_string_constant(codegen.context(), codegen.module(), s);
            Ok(ptr.as_basic_value_enum())
        }
        Literal::Char(c) => {
            let char_ty = codegen.context().i8_type();
            Ok(char_ty.const_int(*c as u64, false).into())
        }
        Literal::Boolean(b) => {
            let bool_ty = codegen.context().bool_type();
            Ok(bool_ty.const_int(if *b { 1 } else { 0 }, false).into())
        }
        Literal::Nil => {
            Err(miette!("Nil literal cannot be converted to value"))
        }
        Literal::BuiltinCall(builtin) => {
            codegen_builtin_call(codegen, builtin)
        }
    }
}

/// Generate code for a binary operation
fn codegen_binary_op(
    codegen: &mut crate::codegen::Codegen,
    left: &AnnotatedExpr,
    op: BinOp,
    right: &AnnotatedExpr,
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    let left_val = codegen_expr(codegen, left)?;
    let right_val = codegen_expr(codegen, right)?;

    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
            codegen_arithmetic_op(codegen, op, left_val, right_val)
        }
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
            codegen_comparison_op(codegen, op, left_val, right_val, &left.ty)
        }
        BinOp::And | BinOp::Or => {
            codegen_logical_op(codegen, op, left_val, right_val)
        }
        BinOp::Concat => {
            Err(miette!("String concatenation not yet implemented"))
        }
        BinOp::Pow => {
            Err(miette!("Power operation not yet implemented"))
        }
    }
}

/// Generate code for an arithmetic operation
fn codegen_arithmetic_op(
    codegen: &mut crate::codegen::Codegen,
    op: BinOp,
    left: BasicValueEnum<'static>,
    right: BasicValueEnum<'static>,
) -> Result<BasicValueEnum<'static>> {
    let (left_int, right_int) = match (left, right) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => (l, r),
        _ => return Err(miette!("Arithmetic operations require integer operands")),
    };

    let result = match op {
        BinOp::Add => codegen.builder.build_int_add(left_int, right_int, "add")?,
        BinOp::Sub => codegen.builder.build_int_sub(left_int, right_int, "sub")?,
        BinOp::Mul => codegen.builder.build_int_mul(left_int, right_int, "mul")?,
        BinOp::Div => codegen.builder.build_int_signed_div(left_int, right_int, "div")?,
        BinOp::Rem => codegen.builder.build_int_signed_rem(left_int, right_int, "rem")?,
        _ => return Err(miette!("Invalid arithmetic operation")),
    };

    Ok(result.as_basic_value_enum())
}

/// Generate code for a comparison operation
fn codegen_comparison_op<'ctx>(
    codegen: &mut crate::codegen::Codegen,
    op: BinOp,
    left: BasicValueEnum<'ctx>,
    right: BasicValueEnum<'ctx>,
    ty: &Type,
) -> Result<BasicValueEnum<'ctx>> {
    let (left_int, right_int) = match (left, right) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => (l, r),
        _ => return Err(miette!("Comparison operations require integer operands")),
    };

    let int_predicate = match op {
        BinOp::Eq => IntPredicate::EQ,
        BinOp::Ne => IntPredicate::NE,
        BinOp::Lt => IntPredicate::SLT,
        BinOp::Gt => IntPredicate::SGT,
        BinOp::Le => IntPredicate::SLE,
        BinOp::Ge => IntPredicate::SGE,
        _ => return Err(miette!("Invalid comparison operation")),
    };

    let result = codegen.builder().build_int_compare(int_predicate, left_int, right_int, "cmp")
        .map_err(|e| miette!("Failed to build comparison: {}", e))?;
    Ok(result.as_basic_value_enum())
}

/// Generate code for a logical operation
fn codegen_logical_op(
    codegen: &mut crate::codegen::Codegen,
    op: BinOp,
    left: BasicValueEnum<'static>,
    right: BasicValueEnum<'static>,
) -> Result<BasicValueEnum<'static>> {
    let (left_bool, right_bool) = match (left, right) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
            // Convert to boolean if needed
            (l, r)
        }
        _ => return Err(miette!("Logical operations require boolean operands")),
    };

    let result = match op {
        BinOp::And => codegen.builder().build_and(left_bool, right_bool, "and")
            .map_err(|e| miette!("Failed to build and: {}", e))?,
        BinOp::Or => codegen.builder().build_or(left_bool, right_bool, "or")
            .map_err(|e| miette!("Failed to build or: {}", e))?,
        _ => return Err(miette!("Invalid logical operation")),
    };

    Ok(result.as_basic_value_enum())
}

/// Generate code for a unary operation
fn codegen_unary_op(
    codegen: &mut crate::codegen::Codegen,
    op: UnaryOp,
    expr: &AnnotatedExpr,
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    let val = codegen_expr(codegen, expr)?;

    match op {
        UnaryOp::Neg => {
            let int_val = match val {
                BasicValueEnum::IntValue(i) => i,
                _ => return Err(miette!("Negation requires integer operand")),
            };
            let zero = int_val.get_type().const_int(0, false);
            let result = codegen.builder().build_int_sub(zero, int_val, "neg")
                .map_err(|e| miette!("Failed to build neg: {}", e))?;
            Ok(result.as_basic_value_enum())
        }
        UnaryOp::Not => {
            let int_val = match val {
                BasicValueEnum::IntValue(i) => i,
                _ => return Err(miette!("Logical not requires boolean operand")),
            };
            let one = int_val.get_type().const_int(1, false);
            let result = codegen.builder().build_int_xor(int_val, one, "not")
                .map_err(|e| miette!("Failed to build not: {}", e))?;
            Ok(result.as_basic_value_enum())
        }
    }
}

/// Generate code for a function call
fn codegen_call(
    codegen: &mut crate::codegen::Codegen,
    callee: &AnnotatedExpr,
    args: &[Expr],
) -> Result<BasicValueEnum<'static>> {
    // For now, assume callee is an identifier
    if let Expr::Ident(name) = &callee.expr {
        let function = codegen.functions().get(name.as_ref())
            .copied()
            .ok_or_else(|| miette!("Function {} not found", name))?;

        // For function calls, we need the function signature to get argument types
        // This is simplified - in practice we'd need to look up the function definition
        // For now, assume no arguments or handle differently
        let arg_values: Vec<BasicValueEnum> = vec![];

        // Call the function
        let result = codegen.builder().build_call(function, &arg_values, "call")?;
        
        // Check if function returns void
        if function.get_type().get_return_type().is_none() {
            // Return a dummy value for void functions
            Ok(codegen.context().i32_type().const_int(0, false).into())
        } else {
            match result.try_as_basic_value() {
                Ok(val) => Ok(val.left().unwrap().as_basic_value_enum()),
                Err(_) => Err(miette!("Function call did not return a basic value")),
            }
        }
    } else {
        Err(miette!("Complex function call expressions not yet implemented"))
    }
}

/// Generate code for a method call
fn codegen_method_call(
    codegen: &mut crate::codegen::Codegen,
    receiver: &AnnotatedExpr,
    method: &str,
    args: &[Expr],
) -> Result<BasicValueEnum<'static>> {
    // Resolve receiver type
    let receiver_type = &receiver.ty;
    let type_name = match receiver_type {
        Type::Named(name) => name.as_ref(),
        _ => return Err(miette!("Method call requires named type receiver")),
    };

    // Look up method in type declaration
    let type_decl = codegen.type_decls.get(type_name)
        .ok_or_else(|| miette!("Type {} not found", type_name))?;

    if let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
        if let Some(impl_block) = &struct_decl.impl_block {
            for func in impl_block {
                if func.name.as_ref() == method {
                    // Found the method - generate call
                    let mangled_name = mangle_method_name(type_name, method);
        let function = codegen.functions().get(&mangled_name)
            .copied()
            .ok_or_else(|| miette!("Method {} not found", mangled_name))?;

        // Generate receiver value
        let receiver_val = codegen_expr(codegen, receiver)?;

        // Generate argument values (simplified)
        let arg_values = vec![receiver_val];

        let result = codegen.builder().build_call(function, &arg_values, "method_call")
            .map_err(|e| miette!("Failed to build method call: {}", e))?;
        match result.try_as_basic_value() {
            Ok(val) => return Ok(val.left().unwrap().as_basic_value_enum()),
            Err(_) => return Err(miette!("Method call did not return a basic value")),
        }
                }
            }
        }
    }

    Err(miette!("Method {} not found on type {}", method, type_name))
}

/// Generate code for member access
fn codegen_member_access(
    codegen: &mut crate::codegen::Codegen,
    object: &AnnotatedExpr,
    field: &MemberField,
) -> Result<BasicValueEnum<'static>> {
    let object_val = codegen_expr(codegen, object)?;
    let object_ptr = match object_val {
        BasicValueEnum::PointerValue(p) => p,
        _ => {
            // If object is not a pointer, we need to get its address
            // For now, assume it's already a pointer or we need to alloca
            return Err(miette!("Member access requires pointer type"));
        }
    };

    match field {
        MemberField::Name(field_name) => {
            // Get struct type
            let struct_ty = match &object.ty {
                Type::Named(name) => {
                    codegen.struct_types().get(name.as_ref())
                        .ok_or_else(|| miette!("Struct type {} not found", name))?
                }
                _ => return Err(miette!("Member access requires struct type")),
            };

            // Find field index
            let type_decl = codegen.type_decls().get(
                match &object.ty {
                    Type::Named(n) => n.as_ref(),
                    _ => return Err(miette!("Expected named type")),
                }
            ).ok_or_else(|| miette!("Type declaration not found"))?;

            let field_index = if let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
                struct_decl.fields.iter()
                    .position(|f| f.name.as_ref() == field_name.as_ref())
                    .ok_or_else(|| miette!("Field {} not found", field_name))?
            } else {
                return Err(miette!("Expected struct type"));
            };

            // Generate GEP
            let indices = [
                codegen.context().i32_type().const_int(0, false),
                codegen.context().i32_type().const_int(field_index as u64, false),
            ];
            let field_ptr = codegen.builder().build_gep(*struct_ty, object_ptr, &indices, "field")
                .map_err(|e| miette!("Failed to build gep: {}", e))?;
            
            // Load field value
            let field_ty = struct_ty.get_field_type_at_index(field_index as u32)
                .ok_or_else(|| miette!("Field type not found"))?;
            let value = codegen.builder().build_load(field_ty, field_ptr, field_name.as_ref())
                .map_err(|e| miette!("Failed to load field: {}", e))?;
            Ok(value)
        }
        MemberField::Index(idx) => {
            // Array/tuple index access
            let indices = [
                codegen.context().i32_type().const_int(0, false),
                codegen.context().i32_type().const_int(*idx, false),
            ];
            // For array/tuple access, we need to get the element type
            // This is simplified - proper implementation would handle different cases
            let elem_ptr = codegen.builder().build_gep(
                codegen.context().i8_type().into(),
                object_ptr,
                &indices,
                "elem",
            )
            .map_err(|e| miette!("Failed to build gep: {}", e))?;
            let elem_ty = codegen.context().i8_type();
            let value = codegen.builder().build_load(elem_ty, elem_ptr, "elem_val")
                .map_err(|e| miette!("Failed to load element: {}", e))?;
            Ok(value)
        }
    }
}

/// Generate code for a struct literal
fn codegen_struct_literal(
    codegen: &mut crate::codegen::Codegen,
    type_: Option<&Type>,
    fields: &[StructFieldInit],
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    // Get struct type
    let struct_name = match type_.or(Some(result_ty)) {
        Some(Type::Named(name)) => name.as_ref(),
        _ => return Err(miette!("Struct literal requires named type")),
    };

    let struct_ty = *codegen.struct_types().get(struct_name)
        .ok_or_else(|| miette!("Struct type {} not found", struct_name))?;

    // Allocate struct on stack
    let alloca = {
        let builder = codegen.builder();
        builder.build_alloca(struct_ty, "struct")
            .map_err(|e| miette!("Failed to alloca struct: {}", e))?
    };

    // Initialize fields
    let type_decl = codegen.type_decls().get(struct_name)
        .ok_or_else(|| miette!("Type declaration not found"))?;

    if let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
        for field_init in fields {
            // Find field index
            let field_index = struct_decl.fields.iter()
                .position(|f| f.name.as_ref() == field_init.name.as_ref())
                .ok_or_else(|| miette!("Field {} not found", field_init.name))?;

            // Generate field value (simplified - would need AnnotatedExpr)
            // For now, return error
            return Err(miette!("Struct literal field initialization needs type information"));
        }
    }

    // Load struct value
    let value = {
        let builder = codegen.builder();
        builder.build_load(struct_ty, alloca, "struct_val")
            .map_err(|e| miette!("Failed to load struct: {}", e))?
    };
    Ok(value)
}

/// Generate code for an enum variant
fn codegen_enum_variant(
    codegen: &mut crate::codegen::Codegen,
    name: &str,
    value: Option<&EnumVariantValue>,
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    Err(miette!("Enum variant construction not yet implemented"))
}

/// Generate code for an if expression
fn codegen_if(
    codegen: &mut crate::codegen::Codegen,
    condition: &AnnotatedExpr,
    then_block: &[Stmt],
    elif_blocks: &[ElifBlock],
    else_block: Option<&[Stmt]>,
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    let cond_val = codegen_expr(codegen, condition)?;
    let cond_int = match cond_val {
        BasicValueEnum::IntValue(i) => i,
        _ => return Err(miette!("Condition must be boolean")),
    };

    let function = codegen.current_function()
        .ok_or_else(|| miette!("If expression must be inside a function"))?;

    let context = codegen.context();
    let then_bb = context.append_basic_block(function, "then");
    let else_bb = context.append_basic_block(function, "else");
    let merge_bb = context.append_basic_block(function, "merge");

    // Branch on condition
    {
        let builder = codegen.builder();
        builder.build_conditional_branch(cond_int, then_bb, else_bb)
            .map_err(|e| miette!("Failed to build conditional branch: {}", e))?;
    }

    // Generate then block
    {
        let builder = codegen.builder();
        builder.position_at_end(then_bb);
        // TODO: Generate statements in then block
        builder.build_unconditional_branch(merge_bb)
            .map_err(|e| miette!("Failed to build branch: {}", e))?;
    }

    // Generate else block
    {
        let builder = codegen.builder();
        builder.position_at_end(else_bb);
        // TODO: Generate statements in else block
        builder.build_unconditional_branch(merge_bb)
            .map_err(|e| miette!("Failed to build branch: {}", e))?;
    }

    // Merge block
    {
        let builder = codegen.builder();
        builder.position_at_end(merge_bb);
    }

    // For now, return a placeholder value
    let result_ty_llvm = type_to_llvm_type(
        result_ty,
        codegen.context(),
            &codegen.struct_types,
            &codegen.enum_types,
    )?;
    match result_ty_llvm {
        BasicTypeEnum::IntType(int_ty) => {
            Ok(int_ty.const_int(0, false).into())
        }
        _ => Err(miette!("If expression result type not yet fully implemented")),
    }
}

/// Generate code for a match expression
fn codegen_match(
    codegen: &mut crate::codegen::Codegen,
    expr: &AnnotatedExpr,
    arms: &[MatchArm],
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    Err(miette!("Match expressions not yet implemented"))
}

/// Generate code for a list
fn codegen_list(
    codegen: &mut crate::codegen::Codegen,
    elements: &[Expr],
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    Err(miette!("List construction not yet implemented"))
}

/// Generate code for a tuple
fn codegen_tuple(
    codegen: &mut crate::codegen::Codegen,
    elements: &[Expr],
    result_ty: &Type,
) -> Result<BasicValueEnum<'static>> {
    Err(miette!("Tuple construction not yet implemented"))
}

/// Generate code for a builtin call
fn codegen_builtin_call(
    codegen: &mut crate::codegen::Codegen,
    builtin: &BuiltinCall,
) -> Result<BasicValueEnum<'static>> {
    match builtin.name.as_ref() {
        "println" | "print" => {
            // Declare printf function if not already declared
            let printf_type = codegen.context().i8_type().ptr_type(AddressSpace::default())
                .fn_type(&[codegen.context().i8_type().ptr_type(AddressSpace::default()).into()], true);
            
            let printf = codegen.module().get_function("printf")
                .unwrap_or_else(|| codegen.module().add_function("printf", printf_type, None));

            // Generate format string and arguments
            // For now, simplified
            Err(miette!("Builtin print functions not yet fully implemented"))
        }
        "unreachable" => {
            // LLVM unreachable instruction
            codegen.builder().build_unreachable()
                .map_err(|e| miette!("Failed to build unreachable: {}", e))?;
            // Return a dummy value (unreachable code)
            Ok(codegen.context().i32_type().const_int(0, false).into())
        }
        _ => Err(miette!("Unknown builtin: {}", builtin.name)),
    }
}
