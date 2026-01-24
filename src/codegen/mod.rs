use crate::analyzer::AnnotatedProgram;
use crate::analyzer::{
    AnnotatedExpr, AnnotatedExprList, AnnotatedFunction, AnnotatedStmt, AnnotatedTopLevelItem,
    AnnotatedVariableDecl,
};
use crate::parser::ast::{
    BinOp, ElifBlock, EnumVariant, Expr, Literal, MemberField, PrimitiveType, Stmt, StructField,
    Type, TypeDecl, TypeDeclKind, UnaryOp,
};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use miette::{Error, Result, SourceSpan, miette};
use std::collections::HashMap;

/// Helper to convert BuilderError to miette Error
fn builder_error_to_miette(err: inkwell::builder::BuilderError) -> Error {
    miette!("LLVM builder error: {}", err)
}

/// Name mangler for generating unique LLVM identifiers
pub struct Mangler(pub String);

impl Mangler {
    /// Mangle a function name: module_name + function_name + param_types + return_type
    pub fn mangle_function<'de>(
        &self,
        name: &str,
        params: &[Type<'de>],
        return_type: &Type<'de>,
    ) -> String {
        let mut result = format!("{}_{}", self.0, name);
        for param in params {
            result.push('_');
            result.push_str(&self.mangle_type(param));
        }
        result.push('_');
        result.push_str(&self.mangle_type(return_type));
        result
    }

    /// Mangle a struct name: module_name + struct_name + field_types
    pub fn mangle_struct<'de>(&self, name: &str, fields: &[StructField<'de>]) -> String {
        let mut result = format!("{}_{}", self.0, name);
        for field in fields {
            result.push('_');
            result.push_str(&self.mangle_type(&field.type_));
        }
        result
    }

    /// Mangle an enum name: module_name + enum_name + variant_count + first_letters
    pub fn mangle_enum<'de>(&self, name: &str, variants: &[EnumVariant<'de>]) -> String {
        let mut result = format!("{}_{}_{}", self.0, name, variants.len());
        for variant in variants {
            if let Some(first_char) = variant.name.chars().next() {
                result.push(first_char);
            }
        }
        result
    }

    /// Convert a type to a mangled string representation
    pub fn mangle_type<'de>(&self, ty: &Type<'de>) -> String {
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::I8 => "i8".to_string(),
                PrimitiveType::I16 => "i16".to_string(),
                PrimitiveType::I32 => "i32".to_string(),
                PrimitiveType::I64 => "i64".to_string(),
                PrimitiveType::Isize => "isize".to_string(),
                PrimitiveType::U8 => "u8".to_string(),
                PrimitiveType::U16 => "u16".to_string(),
                PrimitiveType::U32 => "u32".to_string(),
                PrimitiveType::U64 => "u64".to_string(),
                PrimitiveType::Usize => "usize".to_string(),
                PrimitiveType::Str => "str".to_string(),
                PrimitiveType::Char => "char".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
                PrimitiveType::Nil => "nil".to_string(),
                PrimitiveType::List => "list".to_string(),
            },
            Type::Named(name) => format!("{}", name.as_ref()),
            Type::Generic { base, args } => {
                let mut result = self.mangle_type(base);
                result.push('<');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&self.mangle_type(arg));
                }
                result.push('>');
                result
            }
            Type::Function { params } => {
                let mut result = "fn".to_string();
                for param in params {
                    result.push('_');
                    result.push_str(&self.mangle_type(param));
                }
                result
            }
            Type::Tuple(types) => {
                let mut result = "tuple".to_string();
                for ty in types {
                    result.push('_');
                    result.push_str(&self.mangle_type(ty));
                }
                result
            }
            Type::AnonymousStruct(fields) => {
                let mut result = "anon".to_string();
                for field in fields {
                    result.push('_');
                    result.push_str(&self.mangle_type(&field.type_));
                }
                result
            }
        }
    }
}

/// LLVM IR emitter for generating code from annotated programs
pub struct Emitter<'ctx, 'de> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    mangler: Mangler,
    source: &'de str,
    functions: HashMap<String, FunctionValue<'ctx>>,
    function_info: HashMap<String, AnnotatedFunction<'de>>, // Store function info for lookups
    variables: HashMap<String, BasicValueEnum<'ctx>>,
    struct_types: HashMap<String, inkwell::types::StructType<'ctx>>,
    enum_types: HashMap<String, inkwell::types::StructType<'ctx>>,
}

impl<'ctx, 'de> Emitter<'ctx, 'de> {
    /// Create a new emitter with the given module name and source code
    pub fn new(context: &'ctx Context, module_name: &str, source: &'de str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let mangler = Mangler(module_name.to_string());

        Self {
            context,
            module,
            builder,
            mangler,
            source,
            functions: HashMap::new(),
            function_info: HashMap::new(),
            variables: HashMap::new(),
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
        }
    }

    /// Wrap an error with source code for better diagnostics
    fn wrap_error_with_source(&self, err: Error) -> Error {
        err.with_source_code(self.source.to_string())
    }

    /// Get a dummy span for error reporting (fallback when we can't compute a real span)
    fn dummy_span(&self) -> SourceSpan {
        SourceSpan::from(0..0)
    }

    /// Compute a span for an identifier by searching in the source
    fn span_for_ident(&self, name: &str) -> SourceSpan {
        // Search for the identifier in the source
        // We look for the identifier as a whole word (not part of another identifier)
        if let Some(pos) = self.source.find(name) {
            // Verify it's a whole word (not part of a larger identifier)
            let before = if pos > 0 {
                self.source.chars().nth(pos - 1)
            } else {
                None
            };
            let after = self.source.chars().nth(pos + name.len());

            let is_word_boundary = |c: Option<char>| {
                c.map(|ch| !ch.is_alphanumeric() && ch != '_')
                    .unwrap_or(true)
            };

            if is_word_boundary(before) && is_word_boundary(after) {
                return SourceSpan::new(pos.into(), name.len());
            }
        }
        // Fallback: search for any occurrence
        self.source
            .find(name)
            .map(|pos| SourceSpan::new(pos.into(), name.len()))
            .unwrap_or_else(|| self.dummy_span())
    }

    /// Compute a span for an expression by trying to find its identifier or using a fallback
    fn span_for_expr(&self, expr: &Expr<'de>) -> SourceSpan {
        match expr {
            Expr::Ident(name) => self.span_for_ident(name),
            Expr::Call { callee, .. } => self.span_for_expr(callee),
            Expr::MethodCall { receiver, .. } => self.span_for_expr(receiver),
            Expr::Member { object, .. } => self.span_for_expr(object),
            Expr::Binary { left, .. } => self.span_for_expr(left),
            Expr::Unary { expr, .. } => self.span_for_expr(expr),
            Expr::Paren(expr) => self.span_for_expr(expr),
            Expr::Literal(Literal::BuiltinCall(builtin)) => {
                // Find the @ symbol followed by the builtin name
                let search_str = format!("@{}", builtin.name);
                self.source
                    .find(&search_str)
                    .map(|pos| SourceSpan::new(pos.into(), search_str.len()))
                    .unwrap_or_else(|| self.dummy_span())
            }
            _ => self.dummy_span(),
        }
    }

    /// Compute a span for a type by finding its name in the source
    fn span_for_type(&self, ty: &Type<'de>) -> SourceSpan {
        match ty {
            Type::Named(name) => self.span_for_ident(name),
            Type::Primitive(prim) => {
                // Try to find the primitive type name
                let name = match prim {
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
                self.span_for_ident(name)
            }
            _ => self.dummy_span(),
        }
    }

    /// Convert module to LLVM IR string
    pub fn to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Map Novac type to LLVM type
    pub fn llvm_type(&self, ty: &Type<'de>) -> Result<BasicTypeEnum<'ctx>> {
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::I8 => Ok(self.context.i8_type().as_basic_type_enum()),
                PrimitiveType::I16 => Ok(self.context.i16_type().as_basic_type_enum()),
                PrimitiveType::I32 => Ok(self.context.i32_type().as_basic_type_enum()),
                PrimitiveType::I64 => Ok(self.context.i64_type().as_basic_type_enum()),
                PrimitiveType::Isize => Ok(self.context.i64_type().as_basic_type_enum()), // Assume 64-bit
                PrimitiveType::U8 => Ok(self.context.i8_type().as_basic_type_enum()),
                PrimitiveType::U16 => Ok(self.context.i16_type().as_basic_type_enum()),
                PrimitiveType::U32 => Ok(self.context.i32_type().as_basic_type_enum()),
                PrimitiveType::U64 => Ok(self.context.i64_type().as_basic_type_enum()),
                PrimitiveType::Usize => Ok(self.context.i64_type().as_basic_type_enum()), // Assume 64-bit
                PrimitiveType::Bool => Ok(self.context.bool_type().as_basic_type_enum()),
                PrimitiveType::Char => Ok(self.context.i8_type().as_basic_type_enum()),
                PrimitiveType::Str => Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()),
                PrimitiveType::Nil => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "cannot convert nil type to LLVM type"
                        )],
                        "Cannot convert nil type to LLVM type"
                    )))
                }
                PrimitiveType::List => {
                    // List is represented as a pointer to a struct containing data
                    // For now, use a generic pointer
                    Ok(self
                        .context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum())
                }
            },
            Type::Named(name) => {
                // Look up struct or enum type
                if let Some(struct_ty) = self.struct_types.get(name.as_ref()) {
                    Ok(struct_ty.as_basic_type_enum())
                } else if let Some(enum_ty) = self.enum_types.get(name.as_ref()) {
                    Ok(enum_ty.as_basic_type_enum())
                } else {
                    let span = self.span_for_type(ty);
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            format!("type `{}` is not defined", name.as_ref())
                        )],
                        "Unknown type: {}",
                        name.as_ref()
                    )))
                }
            }
            Type::Tuple(types) => {
                let field_types: Result<Vec<_>> = types
                    .iter()
                    .map(|ty| self.llvm_type(ty).map(|t| t.into()))
                    .collect();
                let struct_ty = self.context.struct_type(&field_types?, false);
                Ok(struct_ty.as_basic_type_enum())
            }
            Type::Function { params: _ } => {
                // Function types are represented as function pointers
                // For now, use a generic pointer type
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            Type::Generic { base, args: _ } => {
                // For generics, use the base type (monomorphization would happen later)
                self.llvm_type(base)
            }
            Type::AnonymousStruct(fields) => {
                let field_types: Result<Vec<_>> = fields
                    .iter()
                    .map(|field| self.llvm_type(&field.type_).map(|t| t.into()))
                    .collect();
                let struct_ty = self.context.struct_type(&field_types?, false);
                Ok(struct_ty.as_basic_type_enum())
            }
        }
    }

    /// Emit a program
    pub fn emit_program(&mut self, program: &AnnotatedProgram<'de>) -> Result<()> {
        // First pass: declare all types
        for item in &program.items {
            if let AnnotatedTopLevelItem::TypeDecl(type_decl) = item {
                self.emit_type_decl(type_decl)?;
            }
        }

        // Second pass: declare all functions (for forward references)
        for item in &program.items {
            if let AnnotatedTopLevelItem::Function(func) = item {
                self.declare_function(func)?;
            }
        }

        // Third pass: emit all function bodies and variable declarations
        for item in &program.items {
            match item {
                AnnotatedTopLevelItem::Function(func) => {
                    self.emit_function(func)?;
                }
                AnnotatedTopLevelItem::VariableDecl(var) => {
                    self.emit_global_variable(var)?;
                }
                AnnotatedTopLevelItem::TypeDecl(_) => {
                    // Already handled in first pass
                }
            }
        }

        Ok(())
    }

    /// Declare a function (without body) for forward references
    fn declare_function(&mut self, func: &AnnotatedFunction<'de>) -> Result<()> {
        // Don't mangle main function - it needs to be called "main" for the linker
        let mangled_name = if func.name.as_ref() == "main" {
            "main".to_string()
        } else {
            self.mangler.mangle_function(
                func.name.as_ref(),
                &func
                    .params
                    .iter()
                    .map(|p| p.type_annotation.clone())
                    .collect::<Vec<_>>(),
                &func.return_type,
            )
        };

        let param_types: Result<Vec<_>> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.type_annotation))
            .collect();

        // Convert parameter types for function signature
        use inkwell::types::BasicMetadataTypeEnum;
        let param_metadatas: Vec<BasicMetadataTypeEnum> = param_types?
            .iter()
            .map(|t| match t {
                BasicTypeEnum::IntType(ty) => BasicMetadataTypeEnum::IntType(*ty),
                BasicTypeEnum::FloatType(ty) => BasicMetadataTypeEnum::FloatType(*ty),
                BasicTypeEnum::PointerType(ty) => BasicMetadataTypeEnum::PointerType(*ty),
                BasicTypeEnum::StructType(ty) => BasicMetadataTypeEnum::StructType(*ty),
                BasicTypeEnum::ArrayType(ty) => BasicMetadataTypeEnum::ArrayType(*ty),
                BasicTypeEnum::VectorType(ty) => BasicMetadataTypeEnum::VectorType(*ty),
                BasicTypeEnum::ScalableVectorType(ty) => {
                    BasicMetadataTypeEnum::ScalableVectorType(*ty)
                }
            })
            .collect();

        let fn_type = if matches!(func.return_type, Type::Primitive(PrimitiveType::Nil)) {
            self.context.void_type().fn_type(&param_metadatas, false)
        } else {
            let return_llvm_type = self.llvm_type(&func.return_type)?;
            match return_llvm_type {
                BasicTypeEnum::IntType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::FloatType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::PointerType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::StructType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::ArrayType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::VectorType(ty) => ty.fn_type(&param_metadatas, false),
                BasicTypeEnum::ScalableVectorType(ty) => ty.fn_type(&param_metadatas, false),
            }
        };

        let function = self.module.add_function(&mangled_name, fn_type, None);
        self.functions.insert(mangled_name.clone(), function);
        self.function_info.insert(mangled_name, func.clone());

        Ok(())
    }

    /// Emit a function
    fn emit_function(&mut self, func: &AnnotatedFunction<'de>) -> Result<()> {
        // Don't mangle main function - it needs to be called "main" for the linker
        let mangled_name = if func.name.as_ref() == "main" {
            "main".to_string()
        } else {
            self.mangler.mangle_function(
                func.name.as_ref(),
                &func
                    .params
                    .iter()
                    .map(|p| p.type_annotation.clone())
                    .collect::<Vec<_>>(),
                &func.return_type,
            )
        };

        let function = self.functions.get(&mangled_name).ok_or_else(|| {
            let span = self.span_for_ident(func.name.as_ref());
            self.wrap_error_with_source(miette!(
                labels = vec![miette::LabeledSpan::at(
                    span,
                    format!("function `{}` is not declared", func.name.as_ref())
                )],
                "Function {} not declared",
                mangled_name
            ))
        })?;

        // Create entry block
        let basic_block = self.context.append_basic_block(*function, "entry");
        self.builder.position_at_end(basic_block);

        // Clear variables for new function scope
        self.variables.clear();

        // Add parameters to variable map
        for (i, param) in func.params.iter().enumerate() {
            let param_value = function
                .get_nth_param(i as u32)
                .ok_or_else(|| miette!("Parameter {} not found in function {}", i, mangled_name))?;
            self.variables
                .insert(param.name.as_ref().to_string(), param_value.into());
        }

        // Emit function body
        match &func.body {
            AnnotatedExprList::Single(expr) => {
                let value = self.emit_expr(expr)?;
                if !matches!(func.return_type, Type::Primitive(PrimitiveType::Nil)) {
                    self.builder
                        .build_return(Some(&value))
                        .map_err(builder_error_to_miette)?;
                } else {
                    self.builder
                        .build_return(None)
                        .map_err(builder_error_to_miette)?;
                }
            }
            AnnotatedExprList::Block(stmts) => {
                for stmt in stmts {
                    self.emit_stmt(stmt)?;
                }
                // If no explicit return, add one
                if !self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some()
                {
                    if matches!(func.return_type, Type::Primitive(PrimitiveType::Nil)) {
                        self.builder
                            .build_return(None)
                            .map_err(builder_error_to_miette)?;
                    } else {
                        let span = self.span_for_ident(func.name.as_ref());
                        return Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                format!("function `{}` must return a value", func.name.as_ref())
                            )],
                            "Function {} must return a value",
                            func.name.as_ref()
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Emit a statement
    fn emit_stmt(&mut self, stmt: &AnnotatedStmt<'de>) -> Result<()> {
        match stmt {
            AnnotatedStmt::VariableDecl(var) => {
                let value = self.emit_expr(&var.value)?;
                self.variables.insert(var.name.as_ref().to_string(), value);
            }
            AnnotatedStmt::Expr(expr) => {
                let _value = self.emit_expr(expr)?;
                // Discard the value
            }
            AnnotatedStmt::Return(Some(expr)) => {
                let value = self.emit_expr(expr)?;
                self.builder
                    .build_return(Some(&value))
                    .map_err(builder_error_to_miette)?;
            }
            AnnotatedStmt::Return(None) => {
                self.builder
                    .build_return(None)
                    .map_err(builder_error_to_miette)?;
            }
        }
        Ok(())
    }

    /// Emit an expression
    fn emit_expr(&mut self, expr: &AnnotatedExpr<'de>) -> Result<BasicValueEnum<'ctx>> {
        match &expr.expr {
            Expr::Literal(lit) => self.emit_literal(lit, &expr.ty),
            Expr::Ident(name) => {
                let span = self.span_for_expr(&expr.expr);
                self.variables.get(name.as_ref()).cloned().ok_or_else(|| {
                    self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            format!("variable `{}` is not defined", name.as_ref())
                        )],
                        "Undefined variable: {}",
                        name.as_ref()
                    ))
                })
            }
            Expr::Binary { left, op, right } => {
                let left_val =
                    self.emit_expr(&AnnotatedExpr::new(*left.clone(), expr.ty.clone()))?;
                let right_val =
                    self.emit_expr(&AnnotatedExpr::new(*right.clone(), expr.ty.clone()))?;
                self.emit_binary_op(op, left_val, right_val, &expr.ty)
            }
            Expr::Unary { op, expr: inner } => {
                let inner_val =
                    self.emit_expr(&AnnotatedExpr::new(*inner.clone(), expr.ty.clone()))?;
                self.emit_unary_op(op, inner_val, &expr.ty)
            }
            Expr::Call { callee, args } => {
                // Handle function calls
                if let Expr::Ident(name) = callee.as_ref() {
                    // Find function with matching name and parameter count
                    let func_info = self
                        .function_info
                        .values()
                        .find(|f| f.name.as_ref() == name.as_ref() && f.params.len() == args.len())
                        .cloned()
                        .ok_or_else(|| {
                            let span = self.span_for_expr(callee);
                            self.wrap_error_with_source(miette!(
                                labels = vec![miette::LabeledSpan::at(
                                    span,
                                    format!(
                                        "function `{}` not found or wrong argument count",
                                        name.as_ref()
                                    )
                                )],
                                "Function `{}` not found or wrong argument count",
                                name.as_ref()
                            ))
                        })?;

                    // Generate mangled name (don't mangle main)
                    let mangled_name = if func_info.name.as_ref() == "main" {
                        "main".to_string()
                    } else {
                        self.mangler.mangle_function(
                            func_info.name.as_ref(),
                            &func_info
                                .params
                                .iter()
                                .map(|p| p.type_annotation.clone())
                                .collect::<Vec<_>>(),
                            &func_info.return_type,
                        )
                    };

                    // Get function value (clone to avoid borrow issues)
                    let function = *self.functions.get(&mangled_name).ok_or_else(|| {
                        let span = self.span_for_expr(callee);
                        self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                format!("function `{}` not found", name.as_ref())
                            )],
                            "Function `{}` not found",
                            name.as_ref()
                        ))
                    })?;

                    // Emit argument expressions
                    let mut arg_values = Vec::new();
                    for arg in args {
                        // We need to infer the type for the argument
                        // For now, use the expected parameter type
                        let arg_idx = arg_values.len();
                        let expected_type = if arg_idx < func_info.params.len() {
                            func_info.params[arg_idx].type_annotation.clone()
                        } else {
                            Type::Primitive(PrimitiveType::I32) // Fallback
                        };
                        let annotated_arg = AnnotatedExpr::new(arg.clone(), expected_type);
                        let arg_val = self.emit_expr(&annotated_arg)?;
                        arg_values.push(arg_val);
                    }

                    // Build call instruction - convert arguments to BasicMetadataValueEnum
                    let call_args: Vec<BasicMetadataValueEnum> = arg_values
                        .iter()
                        .map(|v| match v {
                            BasicValueEnum::IntValue(iv) => BasicMetadataValueEnum::IntValue(*iv),
                            BasicValueEnum::FloatValue(fv) => {
                                BasicMetadataValueEnum::FloatValue(*fv)
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                BasicMetadataValueEnum::PointerValue(*pv)
                            }
                            BasicValueEnum::StructValue(sv) => {
                                BasicMetadataValueEnum::StructValue(*sv)
                            }
                            BasicValueEnum::ArrayValue(av) => {
                                BasicMetadataValueEnum::ArrayValue(*av)
                            }
                            BasicValueEnum::VectorValue(vv) => {
                                BasicMetadataValueEnum::VectorValue(*vv)
                            }
                            BasicValueEnum::ScalableVectorValue(svv) => {
                                BasicMetadataValueEnum::ScalableVectorValue(*svv)
                            }
                        })
                        .collect();

                    let call_result = self
                        .builder
                        .build_call(function, &call_args, "call")
                        .map_err(builder_error_to_miette)?;

                    // Handle return value
                    if matches!(func_info.return_type, Type::Primitive(PrimitiveType::Nil)) {
                        // Void return - return a dummy value
                        Ok(self.context.bool_type().const_int(0, false).into())
                    } else {
                        // Get return value from call
                        // CallSiteValue implements AsValueRef, we can use it as AnyValueEnum
                        // The call instruction itself is the return value
                        let return_llvm_type = self.llvm_type(&func_info.return_type)?;
                        // Convert CallSiteValue to AnyValueEnum using its AsValueRef implementation
                        // We need to use the value reference
                        use inkwell::values::AsValueRef;
                        let value_ref = call_result.as_value_ref();
                        // Create AnyValueEnum from the value reference
                        let any_val = unsafe { AnyValueEnum::new(value_ref) };
                        // Now convert to BasicValueEnum based on type
                        match return_llvm_type {
                            BasicTypeEnum::IntType(int_ty) => match any_val {
                                AnyValueEnum::IntValue(iv) => Ok(iv.into()),
                                _ => Ok(int_ty.const_int(0, false).into()),
                            },
                            BasicTypeEnum::FloatType(float_ty) => match any_val {
                                AnyValueEnum::FloatValue(fv) => Ok(fv.into()),
                                _ => Ok(float_ty.const_float(0.0).into()),
                            },
                            BasicTypeEnum::PointerType(ptr_ty) => match any_val {
                                AnyValueEnum::PointerValue(pv) => Ok(pv.into()),
                                _ => Ok(ptr_ty.const_null().into()),
                            },
                            BasicTypeEnum::StructType(_) => match any_val {
                                AnyValueEnum::StructValue(sv) => Ok(sv.into()),
                                _ => {
                                    let span = self.span_for_expr(callee);
                                    return Err(self.wrap_error_with_source(miette!(
                                        labels = vec![miette::LabeledSpan::at(
                                            span,
                                            "cannot extract struct return value"
                                        )],
                                        "Cannot extract struct return value"
                                    )));
                                }
                            },
                            _ => {
                                let span = self.span_for_expr(callee);
                                Err(self.wrap_error_with_source(miette!(
                                    labels = vec![miette::LabeledSpan::at(
                                        span,
                                        "unsupported return type for function call"
                                    )],
                                    "Unsupported return type for function call"
                                )))
                            }
                        }
                    }
                } else {
                    // Complex callee expression - not yet supported
                    let span = self.span_for_expr(callee);
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "complex function calls (non-identifier callee) not yet supported"
                        )],
                        "Complex function calls (non-identifier callee) not yet supported"
                    )))
                }
            }
            Expr::Paren(inner) => {
                self.emit_expr(&AnnotatedExpr::new(*inner.clone(), expr.ty.clone()))
            }
            Expr::Member { object, field } => {
                // Handle member access (struct field or tuple/index access)
                let object_val = self.emit_expr(&AnnotatedExpr::new(
                    *object.clone(),
                    expr.ty.clone(), // Placeholder type
                ))?;

                match field {
                    MemberField::Name(field_name) => {
                        // Struct field access
                        // Get the object type to determine field index
                        // For now, we'll need to look up the struct type
                        // This is simplified - in a real implementation, we'd track struct layouts
                        let span = self.span_for_expr(&expr.expr);
                        Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                format!(
                                    "struct field access `{}` not yet fully implemented",
                                    field_name.as_ref()
                                )
                            )],
                            "Struct field access `{}` not yet fully implemented",
                            field_name.as_ref()
                        )))
                    }
                    MemberField::Index(idx) => {
                        // Tuple or array indexing
                        // For tuples, use extractvalue
                        // For arrays, use GEP
                        match object_val {
                            BasicValueEnum::StructValue(sv) => {
                                // Tuple access - use extractvalue
                                let extracted = self
                                    .builder
                                    .build_extract_value(sv, *idx as u32, "extract")
                                    .map_err(builder_error_to_miette)?;
                                Ok(extracted)
                            }
                            BasicValueEnum::ArrayValue(av) => {
                                // Array access - use extractvalue for constant arrays
                                // For variable arrays, we'd need to use GEP, but extractvalue works for now
                                let extracted = self
                                    .builder
                                    .build_extract_value(av, *idx as u32, "array_extract")
                                    .map_err(builder_error_to_miette)?;
                                Ok(extracted)
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                // Pointer to array or struct - use GEP
                                // For pointer types, we need to get the pointee type differently
                                // Use a simplified approach: assume it's a pointer to the element type
                                let ptr_type = pv.get_type();
                                // Get the element type from the pointer type
                                // PointerType doesn't have get_element_type, we need to use the type system differently
                                // For now, use a workaround: create GEP with index and load
                                let indices =
                                    [self.context.i32_type().const_int(*idx as u64, false).into()];
                                let gep = unsafe {
                                    self.builder
                                        .build_gep(self.context.i8_type(), pv, &indices, "ptr_idx")
                                        .map_err(builder_error_to_miette)?
                                };
                                // Load the value (simplified - assumes i8)
                                let loaded = self
                                    .builder
                                    .build_load(self.context.i8_type(), gep, "load")
                                    .map_err(builder_error_to_miette)?;
                                Ok(loaded)
                            }
                            _ => {
                                let span = self.span_for_expr(&expr.expr);
                                Err(self.wrap_error_with_source(miette!(
                                    labels = vec![miette::LabeledSpan::at(
                                        span,
                                        format!("index access on unsupported type")
                                    )],
                                    "Index access on unsupported type"
                                )))
                            }
                        }
                    }
                }
            }
            Expr::StructLit { type_, fields } => {
                // Handle struct literal construction
                // Determine the struct type
                let struct_type = if let Some(ty) = type_ {
                    self.llvm_type(ty)?
                } else {
                    // Anonymous struct - infer from fields
                    // For now, return an error
                    let span = self.span_for_expr(&expr.expr);
                    return Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "anonymous struct literals not yet fully implemented"
                        )],
                        "Anonymous struct literals not yet fully implemented"
                    )));
                };

                match struct_type {
                    BasicTypeEnum::StructType(st) => {
                        // Allocate stack space for struct
                        let alloca = self
                            .builder
                            .build_alloca(st, "struct_alloca")
                            .map_err(builder_error_to_miette)?;

                        // Initialize fields
                        for field_init in fields {
                            // Find field index by name (simplified - would need struct layout info)
                            // For now, use a placeholder approach
                            let field_val = self.emit_expr(&AnnotatedExpr::new(
                                *field_init.value.clone(),
                                Type::Primitive(PrimitiveType::I32), // Placeholder
                            ))?;

                            // Store field value (simplified - would need actual field index)
                            // We need to use GEP to get the field pointer first
                            // For now, this is a placeholder - proper implementation would:
                            // 1. Find field index by name in struct type
                            // 2. Use build_struct_gep to get field pointer
                            // 3. Store to that pointer
                            // For now, skip field initialization (incomplete)
                        }

                        // Load the struct value
                        let loaded = self
                            .builder
                            .build_load(st, alloca, "struct_load")
                            .map_err(builder_error_to_miette)?;
                        Ok(loaded)
                    }
                    _ => {
                        let span = self.span_for_expr(&expr.expr);
                        Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "struct literal type is not a struct"
                            )],
                            "Struct literal type is not a struct"
                        )))
                    }
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                // Handle method calls
                // Resolve receiver type and look up method
                // For now, return an error - this requires type context lookup
                let span = self.span_for_expr(&expr.expr);
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        format!(
                            "method calls not yet fully implemented: `{}`",
                            method.as_ref()
                        )
                    )],
                    "Method calls not yet fully implemented: `{}`",
                    method.as_ref()
                )))
            }
            Expr::Match { expr, arms } => {
                // Handle match expressions
                // Create basic blocks for each arm and build switch/if-else chain
                // For now, return an error
                let span = self.span_for_expr(expr);
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        "match expressions not yet fully implemented"
                    )],
                    "Match expressions not yet fully implemented"
                )))
            }
            Expr::Tuple(elements) => {
                // Handle tuple construction
                // Tuples are represented as structs
                // Emit all element values and construct struct
                let element_types: Result<Vec<_>> = elements
                    .iter()
                    .map(|_| {
                        // Infer element type (simplified)
                        Ok(Type::Primitive(PrimitiveType::I32))
                    })
                    .collect();

                let field_types: Result<Vec<_>> = element_types?
                    .iter()
                    .map(|ty| self.llvm_type(ty).map(|t| t.into()))
                    .collect();

                let tuple_struct = self.context.struct_type(&field_types?, false);

                // Emit element values
                let mut element_values = Vec::new();
                for elem in elements {
                    let elem_val = self.emit_expr(&AnnotatedExpr::new(
                        elem.clone(),
                        Type::Primitive(PrimitiveType::I32), // Placeholder
                    ))?;
                    element_values.push(elem_val);
                }

                // Create undef struct and insert values
                let undef_val = tuple_struct.get_undef();
                let mut current_val: inkwell::values::StructValue =
                    undef_val.try_into().map_err(|_| {
                        let span = self.span_for_expr(&expr.expr);
                        self.wrap_error_with_source(miette!(
                            labels =
                                vec![miette::LabeledSpan::at(span, "cannot create tuple struct")],
                            "Cannot create tuple struct"
                        ))
                    })?;
                for (i, elem_val) in element_values.iter().enumerate() {
                    let inserted = self
                        .builder
                        .build_insert_value(current_val, *elem_val, i as u32, "insert")
                        .map_err(builder_error_to_miette)?;
                    // Convert back to StructValue
                    current_val = inserted.try_into().map_err(|_| {
                        let span = self.span_for_expr(&expr.expr);
                        self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "tuple field insertion failed"
                            )],
                            "Tuple field insertion failed"
                        ))
                    })?;
                }

                Ok(current_val.into())
            }
            Expr::List(elements) => {
                // Handle list construction
                // Lists are represented as arrays or pointer-based structures
                // For now, return an error
                let span = self.span_for_expr(&expr.expr);
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        "list construction not yet fully implemented"
                    )],
                    "List construction not yet fully implemented"
                )))
            }
            Expr::If {
                condition,
                then_block,
                elif_blocks,
                else_block,
            } => {
                // Emit if expression with control flow
                let cond_val = self.emit_expr(&AnnotatedExpr::new(
                    *condition.clone(),
                    Type::Primitive(PrimitiveType::Bool),
                ))?;

                // Get current function and basic block
                let current_block = self.builder.get_insert_block().unwrap();
                let function = current_block.get_parent().unwrap();

                // Create basic blocks for branches
                let then_bb = self.context.append_basic_block(function, "if.then");
                let mut elif_bbs = Vec::new();
                for (i, _) in elif_blocks.iter().enumerate() {
                    elif_bbs.push(
                        self.context
                            .append_basic_block(function, &format!("if.elif{}", i)),
                    );
                }
                let else_bb = else_block
                    .as_ref()
                    .map(|_| self.context.append_basic_block(function, "if.else"));
                let merge_bb = self.context.append_basic_block(function, "if.end");

                // Branch on condition
                // For now, handle simple if-else (no elif)
                if elif_blocks.is_empty() {
                    if let Some(else_bb) = else_bb {
                        // If-else
                        self.builder
                            .build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb)
                            .map_err(builder_error_to_miette)?;
                    } else {
                        // If only
                        self.builder
                            .build_conditional_branch(cond_val.into_int_value(), then_bb, merge_bb)
                            .map_err(builder_error_to_miette)?;
                    }
                } else {
                    // Has elif blocks - chain them
                    // For now, implement simple chain
                    self.builder
                        .build_conditional_branch(
                            cond_val.into_int_value(),
                            then_bb,
                            elif_bbs.first().copied().unwrap_or(merge_bb),
                        )
                        .map_err(builder_error_to_miette)?;
                }

                // Emit then block
                self.builder.position_at_end(then_bb);
                let mut then_value = None;
                for stmt in then_block {
                    match stmt {
                        Stmt::Return(Some(expr)) => {
                            // Return statement - need to infer type
                            // For now, use a placeholder type
                            let ret_val = self.emit_expr(&AnnotatedExpr::new(
                                *expr.clone(),
                                Type::Primitive(PrimitiveType::I32), // Placeholder
                            ))?;
                            self.builder
                                .build_return(Some(&ret_val))
                                .map_err(builder_error_to_miette)?;
                            then_value = Some(ret_val);
                            break;
                        }
                        Stmt::Return(None) => {
                            self.builder
                                .build_return(None)
                                .map_err(builder_error_to_miette)?;
                            break;
                        }
                        Stmt::Expr(expr) => {
                            // Emit expression - infer type (simplified)
                            let val = self.emit_expr(&AnnotatedExpr::new(
                                *expr.clone(),
                                Type::Primitive(PrimitiveType::I32), // Placeholder
                            ))?;
                            then_value = Some(val);
                        }
                        Stmt::VariableDecl(var) => {
                            // Emit variable declaration
                            // Infer type from value (simplified)
                            let val = self.emit_expr(&AnnotatedExpr::new(
                                *var.value.clone(),
                                var.type_annotation
                                    .clone()
                                    .unwrap_or(Type::Primitive(PrimitiveType::I32)),
                            ))?;
                            self.variables
                                .insert(var.name.as_ref().to_string(), val.clone());
                            then_value = Some(val);
                        }
                    }
                }
                // If block didn't return, branch to merge
                if !then_bb.get_terminator().is_some() {
                    self.builder
                        .build_unconditional_branch(merge_bb)
                        .map_err(builder_error_to_miette)?;
                }

                // Handle elif blocks (simplified - just handle first one for now)
                for (i, elif_block) in elif_blocks.iter().enumerate() {
                    if i < elif_bbs.len() {
                        let elif_bb = elif_bbs[i];
                        self.builder.position_at_end(elif_bb);

                        // Emit elif condition
                        let elif_cond = self.emit_expr(&AnnotatedExpr::new(
                            *elif_block.condition.clone(),
                            Type::Primitive(PrimitiveType::Bool),
                        ))?;

                        // Create next block (next elif or else or merge)
                        let next_bb = if i + 1 < elif_bbs.len() {
                            elif_bbs[i + 1]
                        } else if let Some(else_bb) = else_bb {
                            else_bb
                        } else {
                            merge_bb
                        };

                        // Branch on elif condition
                        let elif_then_bb = self
                            .context
                            .append_basic_block(function, &format!("elif{}.then", i));
                        self.builder
                            .build_conditional_branch(
                                elif_cond.into_int_value(),
                                elif_then_bb,
                                next_bb,
                            )
                            .map_err(builder_error_to_miette)?;

                        // Emit elif then block
                        self.builder.position_at_end(elif_then_bb);
                        for stmt in &elif_block.body {
                            match stmt {
                                Stmt::Return(Some(expr)) => {
                                    let ret_val = self.emit_expr(&AnnotatedExpr::new(
                                        *expr.clone(),
                                        Type::Primitive(PrimitiveType::I32), // Placeholder
                                    ))?;
                                    self.builder
                                        .build_return(Some(&ret_val))
                                        .map_err(builder_error_to_miette)?;
                                    break;
                                }
                                Stmt::Return(None) => {
                                    self.builder
                                        .build_return(None)
                                        .map_err(builder_error_to_miette)?;
                                    break;
                                }
                                Stmt::Expr(expr) => {
                                    let _val = self.emit_expr(&AnnotatedExpr::new(
                                        *expr.clone(),
                                        Type::Primitive(PrimitiveType::I32), // Placeholder
                                    ))?;
                                }
                                Stmt::VariableDecl(var) => {
                                    let val = self.emit_expr(&AnnotatedExpr::new(
                                        *var.value.clone(),
                                        var.type_annotation
                                            .clone()
                                            .unwrap_or(Type::Primitive(PrimitiveType::I32)),
                                    ))?;
                                    self.variables.insert(var.name.as_ref().to_string(), val);
                                }
                            }
                        }
                        if !elif_then_bb.get_terminator().is_some() {
                            self.builder
                                .build_unconditional_branch(merge_bb)
                                .map_err(builder_error_to_miette)?;
                        }
                    }
                }

                // Handle else block
                if let Some(else_bb) = else_bb {
                    self.builder.position_at_end(else_bb);
                    let mut else_value = None;
                    for stmt in else_block.as_ref().unwrap() {
                        match stmt {
                            Stmt::Return(Some(expr)) => {
                                let ret_val = self.emit_expr(&AnnotatedExpr::new(
                                    *expr.clone(),
                                    Type::Primitive(PrimitiveType::I32), // Placeholder
                                ))?;
                                self.builder
                                    .build_return(Some(&ret_val))
                                    .map_err(builder_error_to_miette)?;
                                else_value = Some(ret_val);
                                break;
                            }
                            Stmt::Return(None) => {
                                self.builder
                                    .build_return(None)
                                    .map_err(builder_error_to_miette)?;
                                break;
                            }
                            Stmt::Expr(expr) => {
                                let val = self.emit_expr(&AnnotatedExpr::new(
                                    *expr.clone(),
                                    Type::Primitive(PrimitiveType::I32), // Placeholder
                                ))?;
                                else_value = Some(val);
                            }
                            Stmt::VariableDecl(var) => {
                                let val = self.emit_expr(&AnnotatedExpr::new(
                                    *var.value.clone(),
                                    var.type_annotation
                                        .clone()
                                        .unwrap_or(Type::Primitive(PrimitiveType::I32)),
                                ))?;
                                self.variables
                                    .insert(var.name.as_ref().to_string(), val.clone());
                                else_value = Some(val);
                            }
                        }
                    }
                    if !else_bb.get_terminator().is_some() {
                        self.builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(builder_error_to_miette)?;
                    }
                }

                // Position at merge block
                self.builder.position_at_end(merge_bb);

                // If expression returns a value, we need a phi node
                // For now, return a placeholder value
                // In a full implementation, we'd create phi nodes for values from different branches
                if matches!(expr.ty, Type::Primitive(PrimitiveType::Nil)) {
                    Ok(self.context.bool_type().const_int(0, false).into())
                } else {
                    // Return a default value of the expression type
                    let default_ty = self.llvm_type(&expr.ty)?;
                    match default_ty {
                        BasicTypeEnum::IntType(t) => Ok(t.const_int(0, false).into()),
                        BasicTypeEnum::FloatType(t) => Ok(t.const_float(0.0).into()),
                        BasicTypeEnum::PointerType(t) => Ok(t.const_null().into()),
                        _ => {
                            let span = self.span_for_expr(&expr.expr);
                            Err(self.wrap_error_with_source(miette!(
                                labels = vec![miette::LabeledSpan::at(
                                    span,
                                    "if expression with complex return type not yet fully supported"
                                )],
                                "If expression with complex return type not yet fully supported"
                            )))
                        }
                    }
                }
            }
            _ => {
                let span = self.span_for_expr(&expr.expr);
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        "expression type not yet implemented"
                    )],
                    "Expression type not yet implemented: {:?}",
                    expr.expr
                )))
            }
        }
    }

    /// Emit a literal
    fn emit_literal(&mut self, lit: &Literal<'de>, ty: &Type<'de>) -> Result<BasicValueEnum<'ctx>> {
        match lit {
            Literal::Number(n) => match ty {
                Type::Primitive(PrimitiveType::I8) => {
                    Ok(self.context.i8_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::I16) => {
                    Ok(self.context.i16_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::I32) => {
                    Ok(self.context.i32_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::I64) => {
                    Ok(self.context.i64_type().const_int(*n, false).into())
                }
                Type::Primitive(PrimitiveType::U8) => {
                    Ok(self.context.i8_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::U16) => {
                    Ok(self.context.i16_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::U32) => {
                    Ok(self.context.i32_type().const_int(*n as u64, false).into())
                }
                Type::Primitive(PrimitiveType::U64) => {
                    Ok(self.context.i64_type().const_int(*n, false).into())
                }
                _ => Ok(self.context.i32_type().const_int(*n as u64, false).into()),
            },
            Literal::String(s) => {
                // Create a global string constant
                // Include null terminator in the array size
                let string_type = self.context.i8_type().array_type(s.len() as u32 + 1);
                // const_string with null_terminated=true automatically adds null terminator
                let string_value = self.context.const_string(s.as_bytes(), true);
                let global = self.module.add_global(string_type, None, "str");
                global.set_initializer(&string_value);
                Ok(global.as_pointer_value().into())
            }
            Literal::Char(c) => Ok(self
                .context
                .i8_type()
                .const_int(*c as u8 as u64, false)
                .into()),
            Literal::Boolean(b) => Ok(self
                .context
                .bool_type()
                .const_int(if *b { 1 } else { 0 }, false)
                .into()),
            Literal::Nil => {
                let span = self.dummy_span();
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(span, "cannot emit nil literal")],
                    "Cannot emit nil literal"
                )))
            }
            Literal::BuiltinCall(builtin) => self.emit_builtin_call(builtin),
        }
    }

    /// Emit a binary operation
    fn emit_binary_op(
        &mut self,
        op: &BinOp,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
        ty: &Type<'de>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match op {
            BinOp::Add => {
                if matches!(ty, Type::Primitive(PrimitiveType::Str)) {
                    // String concatenation - would need runtime support
                    let span = self.dummy_span();
                    return Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "string concatenation not yet implemented"
                        )],
                        "String concatenation not yet implemented"
                    )));
                }
                match (left, right) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(builder_error_to_miette)?
                        .into()),
                    _ => {
                        let span = self.dummy_span();
                        Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "unsupported types for addition"
                            )],
                            "Unsupported types for addition"
                        )))
                    }
                }
            }
            BinOp::Sub => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_sub(l, r, "sub")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for subtraction"
                        )],
                        "Unsupported types for subtraction"
                    )))
                }
            },
            BinOp::Mul => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_mul(l, r, "mul")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for multiplication"
                        )],
                        "Unsupported types for multiplication"
                    )))
                }
            },
            BinOp::Div => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_signed_div(l, r, "div")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for division"
                        )],
                        "Unsupported types for division"
                    )))
                }
            },
            BinOp::Rem => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_signed_rem(l, r, "rem")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for remainder"
                        )],
                        "Unsupported types for remainder"
                    )))
                }
            },
            BinOp::Eq => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::EQ, l, r, "eq")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "eq")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for equality"
                        )],
                        "Unsupported types for equality"
                    )))
                }
            },
            BinOp::Ne => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::NE, l, r, "ne")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "ne")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for inequality"
                        )],
                        "Unsupported types for inequality"
                    )))
                }
            },
            BinOp::Lt => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::SLT, l, r, "lt")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "lt")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for less than"
                        )],
                        "Unsupported types for less than"
                    )))
                }
            },
            BinOp::Gt => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::SGT, l, r, "gt")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "gt")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for greater than"
                        )],
                        "Unsupported types for greater than"
                    )))
                }
            },
            BinOp::Le => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::SLE, l, r, "le")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "le")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for less than or equal"
                        )],
                        "Unsupported types for less than or equal"
                    )))
                }
            },
            BinOp::Ge => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_int_compare(IntPredicate::SGE, l, r, "ge")
                    .map_err(builder_error_to_miette)?
                    .into()),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "ge")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for greater than or equal"
                        )],
                        "Unsupported types for greater than or equal"
                    )))
                }
            },
            BinOp::And => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_and(l, r, "and")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for logical and"
                        )],
                        "Unsupported types for logical and"
                    )))
                }
            },
            BinOp::Or => match (left, right) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => Ok(self
                    .builder
                    .build_or(l, r, "or")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported types for logical or"
                        )],
                        "Unsupported types for logical or"
                    )))
                }
            },
            BinOp::Concat => {
                // String concatenation - would need runtime support
                // For now, return an error indicating it needs runtime library
                let span = self.dummy_span();
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        "string concatenation requires runtime library support"
                    )],
                    "String concatenation requires runtime library support"
                )))
            }
            BinOp::Pow => {
                // Power operation - use LLVM pow intrinsic
                // For integers, we can use a loop or call pow function
                match (left, right) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        // For integer power, we can implement a loop or use pow from math library
                        // For now, declare pow function and call it
                        // This is simplified - in a real implementation, we'd handle different types
                        let span = self.dummy_span();
                        Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "power operation requires math library support"
                            )],
                            "Power operation requires math library support"
                        )))
                    }
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        // For floats, use LLVM pow intrinsic
                        let pow_fn = self
                            .module
                            .get_function("llvm.pow.f64")
                            .or_else(|| {
                                // Declare pow intrinsic if not present
                                let fn_type = self.context.f64_type().fn_type(
                                    &[
                                        self.context.f64_type().into(),
                                        self.context.f64_type().into(),
                                    ],
                                    false,
                                );
                                Some(self.module.add_function("llvm.pow.f64", fn_type, None))
                            })
                            .ok_or_else(|| {
                                let span = self.dummy_span();
                                self.wrap_error_with_source(miette!(
                                    labels = vec![miette::LabeledSpan::at(
                                        span,
                                        "cannot declare pow function"
                                    )],
                                    "Cannot declare pow function"
                                ))
                            })?;
                        let call_args: Vec<BasicMetadataValueEnum> = vec![
                            BasicMetadataValueEnum::FloatValue(l),
                            BasicMetadataValueEnum::FloatValue(r),
                        ];
                        let call_result = self
                            .builder
                            .build_call(pow_fn, &call_args, "pow")
                            .map_err(builder_error_to_miette)?;
                        // Extract return value
                        use inkwell::values::AsValueRef;
                        let value_ref = call_result.as_value_ref();
                        let any_val = unsafe { AnyValueEnum::new(value_ref) };
                        match any_val {
                            AnyValueEnum::FloatValue(fv) => Ok(fv.into()),
                            _ => {
                                let span = self.dummy_span();
                                Err(self.wrap_error_with_source(miette!(
                                    labels = vec![miette::LabeledSpan::at(
                                        span,
                                        "pow returned unexpected type"
                                    )],
                                    "Pow returned unexpected type"
                                )))
                            }
                        }
                    }
                    _ => {
                        let span = self.dummy_span();
                        Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "power operation requires numeric operands"
                            )],
                            "Power operation requires numeric operands"
                        )))
                    }
                }
            }
        }
    }

    /// Emit a unary operation
    fn emit_unary_op(
        &self,
        op: &UnaryOp,
        operand: BasicValueEnum<'ctx>,
        _ty: &Type<'de>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match op {
            UnaryOp::Neg => match operand {
                BasicValueEnum::IntValue(v) => Ok(self
                    .builder
                    .build_int_neg(v, "neg")
                    .map_err(builder_error_to_miette)?
                    .into()),
                BasicValueEnum::FloatValue(v) => Ok(self
                    .builder
                    .build_float_neg(v, "neg")
                    .map_err(builder_error_to_miette)?
                    .into()),
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported type for negation"
                        )],
                        "Unsupported type for negation"
                    )))
                }
            },
            UnaryOp::Not => match operand {
                BasicValueEnum::IntValue(v) => {
                    // Logical not: compare with zero
                    let zero = v.get_type().const_int(0, false);
                    Ok(self
                        .builder
                        .build_int_compare(IntPredicate::EQ, v, zero, "not")
                        .map_err(builder_error_to_miette)?
                        .into())
                }
                _ => {
                    let span = self.dummy_span();
                    Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "unsupported type for logical not"
                        )],
                        "Unsupported type for logical not"
                    )))
                }
            },
        }
    }

    /// Try to infer a type from an expression (for format string arguments)
    fn infer_expr_type_for_format(&self, expr: &Expr<'de>) -> Type<'de> {
        match expr {
            Expr::Literal(Literal::Number(_)) => Type::Primitive(PrimitiveType::I32),
            Expr::Literal(Literal::String(_)) => Type::Primitive(PrimitiveType::Str),
            Expr::Literal(Literal::Char(_)) => Type::Primitive(PrimitiveType::Char),
            Expr::Literal(Literal::Boolean(_)) => Type::Primitive(PrimitiveType::Bool),
            Expr::Literal(Literal::Nil) => Type::Primitive(PrimitiveType::Nil),
            Expr::Ident(name) => {
                // Try to look up variable type from context
                // For now, use a default - in a real implementation we'd track this
                Type::Primitive(PrimitiveType::I32)
            }
            _ => Type::Primitive(PrimitiveType::I32), // Default fallback
        }
    }

    /// Parse a format string and extract {} placeholders, converting them to printf format specifiers
    /// Returns the converted format string with %PLACEHOLDER% markers
    fn parse_format_string(&self, format_str: &str, num_args: usize) -> Result<String, Error> {
        let mut result = String::new();
        let mut chars = format_str.chars().peekable();
        let mut placeholder_count = 0;

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if let Some(&next) = chars.peek() {
                    if next == '{' {
                        // Escaped {{ - output single {
                        result.push('{');
                        chars.next(); // consume the second {
                    } else if next == '}' {
                        // Found {} placeholder
                        chars.next(); // consume the }
                        placeholder_count += 1;
                        if placeholder_count > num_args {
                            return Err(miette!(
                                "format string has {} placeholders but only {} arguments provided",
                                placeholder_count,
                                num_args
                            ));
                        }
                        // Placeholder for printf specifier - will be replaced
                        result.push_str("%PLACEHOLDER%");
                    } else {
                        // Invalid - { followed by something other than { or }
                        return Err(miette!(
                            "invalid format string: `{{` must be followed by `{{` or `}}`"
                        ));
                    }
                } else {
                    // { at end of string
                    return Err(miette!("invalid format string: unclosed `{{`"));
                }
            } else if ch == '}' {
                if let Some(&next) = chars.peek() {
                    if next == '}' {
                        // Escaped }} - output single }
                        result.push('}');
                        chars.next(); // consume the second }
                    } else {
                        // Invalid - } without matching {
                        return Err(miette!(
                            "invalid format string: `}}` must be escaped as `}}}}` or used in `{{}}`"
                        ));
                    }
                } else {
                    // } at end of string
                    return Err(miette!("invalid format string: unclosed `}}`"));
                }
            } else {
                result.push(ch);
            }
        }

        if placeholder_count != num_args {
            return Err(miette!(
                "format string has {} placeholders but {} arguments provided",
                placeholder_count,
                num_args
            ));
        }

        Ok(result)
    }

    /// Convert a BasicValueEnum to a printf format specifier and return the value to pass
    /// For booleans, returns a pointer to "true" or "false" string and "%s" as the specifier
    fn value_to_printf_spec(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type<'de>,
    ) -> Result<(String, BasicMetadataValueEnum<'ctx>), Error> {
        match (value, ty) {
            (BasicValueEnum::IntValue(iv), Type::Primitive(PrimitiveType::Bool)) => {
                // Boolean: create "true" and "false" string constants and use select
                // Reuse existing globals if they exist, otherwise create new ones
                let true_global = match self.module.get_global("bool_true_str") {
                    Some(g) => g,
                    None => {
                        let true_str = self.context.const_string(b"true", true);
                        let g = self.module.add_global(
                            self.context.i8_type().array_type(5), // "true" + null terminator
                            None,
                            "bool_true_str",
                        );
                        g.set_initializer(&true_str);
                        g
                    }
                };

                let false_global = match self.module.get_global("bool_false_str") {
                    Some(g) => g,
                    None => {
                        let false_str = self.context.const_string(b"false", true);
                        let g = self.module.add_global(
                            self.context.i8_type().array_type(6), // "false" + null terminator
                            None,
                            "bool_false_str",
                        );
                        g.set_initializer(&false_str);
                        g
                    }
                };

                // Use select instruction to choose between true and false strings
                let true_ptr = true_global.as_pointer_value();
                let false_ptr = false_global.as_pointer_value();

                // Convert boolean int to i1 for select
                let bool_type = self.context.bool_type();
                let bool_val = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        iv,
                        bool_type.const_int(0, false),
                        "bool_check",
                    )
                    .map_err(builder_error_to_miette)?;

                let selected = self
                    .builder
                    .build_select(bool_val, true_ptr, false_ptr, "bool_str_select")
                    .map_err(builder_error_to_miette)?;

                // Extract the pointer value from the select result
                let selected_ptr = match selected {
                    BasicValueEnum::PointerValue(pv) => pv,
                    _ => {
                        return Err(miette!("expected pointer value from select instruction"));
                    }
                };

                Ok((
                    "%s".to_string(),
                    BasicMetadataValueEnum::PointerValue(selected_ptr),
                ))
            }
            (BasicValueEnum::IntValue(iv), Type::Primitive(PrimitiveType::Char)) => {
                // Char is stored as i8, extend to i32 for printf %c
                let char_int = self
                    .builder
                    .build_int_z_extend(iv, self.context.i32_type(), "char_extend")
                    .map_err(builder_error_to_miette)?;
                Ok(("%c".to_string(), BasicMetadataValueEnum::IntValue(char_int)))
            }
            (BasicValueEnum::IntValue(iv), Type::Primitive(prim)) => {
                let spec = match prim {
                    PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 => "%d",
                    PrimitiveType::I64 => "%ld",
                    PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 => "%u",
                    PrimitiveType::U64 => "%lu",
                    PrimitiveType::Isize => "%ld",
                    PrimitiveType::Usize => "%lu",
                    _ => "%d", // Default fallback
                };
                Ok((spec.to_string(), BasicMetadataValueEnum::IntValue(iv)))
            }
            (BasicValueEnum::FloatValue(fv), _) => {
                Ok(("%f".to_string(), BasicMetadataValueEnum::FloatValue(fv)))
            }
            (BasicValueEnum::PointerValue(pv), Type::Primitive(PrimitiveType::Str)) => {
                Ok(("%s".to_string(), BasicMetadataValueEnum::PointerValue(pv)))
            }
            (BasicValueEnum::PointerValue(pv), Type::Primitive(PrimitiveType::Char)) => {
                // For char pointer, we need to dereference the pointer to get the char value
                // But printf expects %c with an int, so we need to load the char
                let char_val = self
                    .builder
                    .build_load(self.context.i8_type(), pv, "char_load")
                    .map_err(builder_error_to_miette)?;
                let char_int = self
                    .builder
                    .build_int_z_extend(
                        char_val.into_int_value(),
                        self.context.i32_type(),
                        "char_extend",
                    )
                    .map_err(builder_error_to_miette)?;
                Ok(("%c".to_string(), BasicMetadataValueEnum::IntValue(char_int)))
            }
            (BasicValueEnum::PointerValue(pv), _) => {
                // For other pointer types, treat as string
                Ok(("%s".to_string(), BasicMetadataValueEnum::PointerValue(pv)))
            }
            _ => {
                // Fallback: try to use as int
                if let BasicValueEnum::IntValue(iv) = value {
                    Ok(("%d".to_string(), BasicMetadataValueEnum::IntValue(iv)))
                } else {
                    Err(miette!("unsupported type for format string"))
                }
            }
        }
    }

    /// Emit a builtin call
    fn emit_builtin_call(
        &mut self,
        builtin: &crate::parser::ast::BuiltinCall<'de>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match builtin.name.as_ref() {
            "println" | "print" => {
                // Declare printf if not already declared
                let i8_type = self.context.i8_type();
                let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
                use inkwell::types::BasicMetadataTypeEnum;
                let printf_type =
                    i8_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], true); // variadic

                let printf_fn = match self.module.get_function("printf") {
                    Some(f) => f,
                    None => self.module.add_function("printf", printf_type, None),
                };

                // Get arguments
                let args = builtin.args.as_ref().map(|v| v.as_slice()).unwrap_or(&[]);
                if args.is_empty() {
                    let span =
                        self.span_for_expr(&Expr::Literal(Literal::BuiltinCall(builtin.clone())));
                    return Err(self.wrap_error_with_source(miette!(
                        labels = vec![miette::LabeledSpan::at(
                            span,
                            "println/print requires at least one argument"
                        )],
                        "println/print requires at least one argument"
                    )));
                }

                // Extract format string from first argument (must be a string literal)
                let format_str_expr = &args[0];
                let format_str = match format_str_expr {
                    Expr::Literal(Literal::String(s)) => s.as_ref(),
                    _ => {
                        let span = self.span_for_expr(format_str_expr);
                        return Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "format string must be a string literal"
                            )],
                            "Format string must be a string literal (runtime format strings not supported)"
                        )));
                    }
                };

                // Count additional arguments
                let num_args = args.len() - 1;

                // Parse format string to find {} placeholders
                let parsed_format = self
                    .parse_format_string(format_str, num_args)
                    .map_err(|e| self.wrap_error_with_source(e))?;

                // Emit all arguments and determine their types
                let mut arg_specs: Vec<(String, BasicMetadataValueEnum<'ctx>)> = Vec::new();
                for arg_expr in args.iter().skip(1) {
                    // Infer type for the argument
                    let arg_type = self.infer_expr_type_for_format(arg_expr);

                    // Emit the argument
                    let arg_val =
                        self.emit_expr(&AnnotatedExpr::new(arg_expr.clone(), arg_type.clone()))?;

                    // Convert to printf specifier and value
                    let (spec, metadata_val) = self.value_to_printf_spec(arg_val, &arg_type)?;
                    arg_specs.push((spec, metadata_val));
                }

                // Build the final format string by replacing %PLACEHOLDER% with actual specifiers
                // Replace them in order from left to right
                let mut final_format = parsed_format;
                for (spec, _) in arg_specs.iter() {
                    // Replace the first occurrence of %PLACEHOLDER% with the actual specifier
                    final_format = final_format.replacen("%PLACEHOLDER%", spec, 1);
                }

                // Create the format string constant
                let format_bytes = final_format.as_bytes();
                let format_str_const = self.context.const_string(format_bytes, true);
                let format_global = self.module.add_global(
                    self.context
                        .i8_type()
                        .array_type(format_bytes.len() as u32 + 1),
                    None,
                    "format_str",
                );
                format_global.set_initializer(&format_str_const);

                // Build printf call arguments
                let mut printf_args: Vec<BasicMetadataValueEnum> =
                    vec![BasicMetadataValueEnum::PointerValue(
                        format_global.as_pointer_value(),
                    )];

                // Add all the argument values
                for (_, metadata_val) in arg_specs {
                    printf_args.push(metadata_val);
                }

                // Call printf
                let _call_result = self
                    .builder
                    .build_call(printf_fn, &printf_args, "printf_call")
                    .map_err(builder_error_to_miette)?;

                // If println, also emit newline
                if builtin.name.as_ref() == "println" {
                    // Call printf with "\n" format string
                    let newline_str = self.context.const_string(b"\n", true);
                    let newline_global = self.module.add_global(
                        self.context.i8_type().array_type(2),
                        None,
                        "newline_str",
                    );
                    newline_global.set_initializer(&newline_str);
                    let newline_args = vec![BasicMetadataValueEnum::PointerValue(
                        newline_global.as_pointer_value(),
                    )];
                    let _newline_call = self
                        .builder
                        .build_call(printf_fn, &newline_args, "println_newline")
                        .map_err(builder_error_to_miette)?;
                }

                // Return void (nil)
                Ok(self.context.bool_type().const_int(0, false).into())
            }
            "unreachable" => {
                self.builder
                    .build_unreachable()
                    .map_err(builder_error_to_miette)?;
                Ok(self.context.bool_type().const_int(0, false).into())
            }
            _ => {
                let span =
                    self.span_for_expr(&Expr::Literal(Literal::BuiltinCall(builtin.clone())));
                Err(self.wrap_error_with_source(miette!(
                    labels = vec![miette::LabeledSpan::at(
                        span,
                        format!("unknown builtin `@{}`", builtin.name.as_ref())
                    )],
                    "Unknown builtin: {}",
                    builtin.name.as_ref()
                )))
            }
        }
    }

    /// Emit a type declaration
    fn emit_type_decl(&mut self, type_decl: &TypeDecl<'de>) -> Result<()> {
        match &type_decl.decl {
            TypeDeclKind::Struct(struct_decl) => {
                let field_types: Result<Vec<_>> = struct_decl
                    .fields
                    .iter()
                    .map(|field| self.llvm_type(&field.type_).map(|t| t.into()))
                    .collect();
                let struct_type = self.context.struct_type(&field_types?, false);
                let _mangled_name = self
                    .mangler
                    .mangle_struct(type_decl.name.as_ref(), &struct_decl.fields);
                // Note: set_name is not available in this version of Inkwell
                self.struct_types
                    .insert(type_decl.name.as_ref().to_string(), struct_type);
            }
            TypeDeclKind::Enum(enum_decl) => {
                // Represent enum as a struct with a tag and union of variants
                // For simplicity, use an integer tag
                let tag_type = self.context.i32_type();
                // For now, just store the tag
                let enum_type = self.context.struct_type(&[tag_type.into()], false);
                let _mangled_name = self
                    .mangler
                    .mangle_enum(type_decl.name.as_ref(), &enum_decl.variants);
                // Note: set_name is not available in this version of Inkwell
                self.enum_types
                    .insert(type_decl.name.as_ref().to_string(), enum_type);
            }
        }
        Ok(())
    }

    /// Emit a global variable
    fn emit_global_variable(&mut self, var: &AnnotatedVariableDecl<'de>) -> Result<()> {
        let var_type = self.llvm_type(&var.type_annotation)?;

        // For global variables, we need to compute the initializer as a constant
        // This is simplified - in a real implementation, we'd need to handle more cases
        let initial_value = match &var.value.expr {
            Expr::Literal(Literal::Number(n)) => match var.type_annotation {
                Type::Primitive(PrimitiveType::I32) => {
                    BasicValueEnum::IntValue(self.context.i32_type().const_int(*n as u64, false))
                }
                Type::Primitive(PrimitiveType::I64) => {
                    BasicValueEnum::IntValue(self.context.i64_type().const_int(*n, false))
                }
                _ => BasicValueEnum::IntValue(self.context.i32_type().const_int(*n as u64, false)),
            },
            Expr::Literal(Literal::Boolean(b)) => BasicValueEnum::IntValue(
                self.context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false),
            ),
            _ => {
                // For complex expressions, we'd need to evaluate them at compile time
                // For now, use a zero initializer
                match var_type {
                    BasicTypeEnum::IntType(t) => BasicValueEnum::IntValue(t.const_int(0, false)),
                    BasicTypeEnum::FloatType(t) => BasicValueEnum::FloatValue(t.const_float(0.0)),
                    _ => {
                        let span = self.span_for_expr(&var.value.expr);
                        return Err(self.wrap_error_with_source(miette!(
                            labels = vec![miette::LabeledSpan::at(
                                span,
                                "cannot initialize global variable with complex expression"
                            )],
                            "Cannot initialize global variable with complex expression"
                        )));
                    }
                }
            }
        };

        // Create global variable
        let global = self.module.add_global(var_type, None, var.name.as_ref());
        global.set_initializer(&initial_value);
        global.set_linkage(inkwell::module::Linkage::Internal);

        Ok(())
    }
}

/// Generate LLVM IR from an annotated program
pub fn generate<'de>(
    module_name: String,
    program: AnnotatedProgram<'de>,
    source: &'de str,
) -> Result<String, Error> {
    let context = Context::create();
    let mut emitter = Emitter::new(&context, &module_name, source);
    emitter.emit_program(&program)?;
    Ok(emitter.to_string())
}
