mod builder;
mod expressions;
mod types;

use crate::analyzer::*;
use crate::parser::ast::*;
use builder::*;
use expressions::codegen_expr;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::*;
use miette::{Result, miette};
use std::collections::HashMap;
use types::*;

/// Main code generator that converts annotated AST to LLVM IR
pub struct Codegen {
    context: Context,
    module: Module<'static>,
    builder: Builder<'static>,
    /// Current function being generated (if any)
    current_function: Option<FunctionValue<'static>>,
    /// Variable symbol table (name -> alloca'd pointer)
    variables: HashMap<String, PointerValue<'static>>,
    /// Function symbol table (name -> function value)
    functions: HashMap<String, FunctionValue<'static>>,
    /// Struct type definitions (name -> struct type)
    struct_types: HashMap<String, inkwell::types::StructType<'static>>,
    /// Enum type definitions (name -> struct type representing the enum)
    enum_types: HashMap<String, inkwell::types::StructType<'static>>,
    /// Type declarations for lookup
    type_decls: HashMap<String, TypeDecl<'static>>,
    /// Counter for unique variable names
    var_counter: u32,
}

impl Codegen {
    /// Create a new codegen instance from an annotated program
    pub fn from_annotated_program(module_name: String, program: AnnotatedProgram) -> Result<Self> {
        let context = Context::create();
        let module = context.create_module(&module_name);
        let builder = context.create_builder();

        let mut codegen = Self {
            context,
            module,
            builder,
            current_function: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            type_decls: HashMap::new(),
            var_counter: 0,
        };

        // First pass: collect type declarations and create LLVM types
        for item in &program.items {
            if let AnnotatedTopLevelItem::TypeDecl(type_decl) = item {
                codegen
                    .type_decls
                    .insert(type_decl.name.to_string(), type_decl.clone());
                codegen.create_type_declaration(type_decl)?;
            }
        }

        // Second pass: collect function signatures (for forward references)
        for item in &program.items {
            if let AnnotatedTopLevelItem::Function(func) = item {
                codegen.declare_function(func)?;
            }
        }

        // Third pass: generate function bodies
        for item in program.items {
            match item {
                AnnotatedTopLevelItem::Function(func) => {
                    codegen.codegen_function(&func)?;
                }
                AnnotatedTopLevelItem::VariableDecl(var) => {
                    // Top-level variables become global variables
                    codegen.codegen_global_variable(&var)?;
                }
                AnnotatedTopLevelItem::TypeDecl(_) => {
                    // Already handled in first pass
                }
            }
        }

        Ok(codegen)
    }

    /// Get reference to context
    pub fn context(&self) -> &Context {
        &self.context
    }

    /// Get mutable reference to builder
    pub fn builder(&mut self) -> &mut Builder<'static> {
        &mut self.builder
    }

    /// Get reference to module
    pub fn module(&self) -> &Module<'static> {
        &self.module
    }

    /// Get mutable reference to variables
    pub fn variables_mut(&mut self) -> &mut HashMap<String, PointerValue<'static>> {
        &mut self.variables
    }

    /// Get reference to struct types
    pub fn struct_types(&self) -> &HashMap<String, inkwell::types::StructType<'static>> {
        &self.struct_types
    }

    /// Get reference to enum types
    pub fn enum_types(&self) -> &HashMap<String, inkwell::types::StructType<'static>> {
        &self.enum_types
    }

    /// Get reference to functions
    pub fn functions(&self) -> &HashMap<String, FunctionValue<'static>> {
        &self.functions
    }

    /// Get mutable reference to functions
    pub fn functions_mut(&mut self) -> &mut HashMap<String, FunctionValue<'static>> {
        &mut self.functions
    }

    /// Get reference to type declarations
    pub fn type_decls(&self) -> &HashMap<String, TypeDecl<'static>> {
        &self.type_decls
    }

    /// Get current function
    pub fn current_function(&self) -> Option<FunctionValue<'static>> {
        self.current_function
    }

    /// Set current function
    pub fn set_current_function(&mut self, func: Option<FunctionValue<'static>>) {
        self.current_function = func;
    }

    /// Create LLVM type declaration for a struct or enum
    fn create_type_declaration(&mut self, type_decl: &TypeDecl) -> Result<()> {
        match &type_decl.decl {
            TypeDeclKind::Struct(struct_decl) => {
                self.create_struct_type(&type_decl.name.to_string(), struct_decl)?;
            }
            TypeDeclKind::Enum(enum_decl) => {
                self.create_enum_type(&type_decl.name.to_string(), enum_decl)?;
            }
        }
        Ok(())
    }

    /// Create a struct type in LLVM
    fn create_struct_type(&mut self, name: &str, struct_decl: &StructDecl) -> Result<()> {
        let field_types: Result<Vec<_>> = struct_decl
            .fields
            .iter()
            .map(|field| {
                type_to_llvm_type(
                    &field.type_,
                    &self.context,
                    &self.struct_types,
                    &self.enum_types,
                )
            })
            .collect();

        let struct_ty = self.context.opaque_struct_type(name);
        struct_ty.set_body(
            &field_types?.iter().map(|t| t.into()).collect::<Vec<_>>(),
            false,
        );

        self.struct_types.insert(name.to_string(), struct_ty);
        Ok(())
    }

    /// Create an enum type in LLVM (tagged union)
    fn create_enum_type(&mut self, name: &str, enum_decl: &EnumDecl) -> Result<()> {
        // Enums are represented as a struct with:
        // - discriminant (i32) to identify the variant
        // - union of all variant payloads
        // For simplicity, we'll use a struct with discriminant + largest payload
        // TODO: Optimize enum representation

        let mut payload_types = Vec::new();

        for variant in &enum_decl.variants {
            if let Some(variant_ty) = &variant.type_ {
                let llvm_ty = type_to_llvm_type(
                    variant_ty,
                    &self.context,
                    &self.struct_types,
                    &self.enum_types,
                )?;
                payload_types.push(llvm_ty);
            }
        }

        // Create enum struct: { i32 discriminant, ...payload }
        let mut struct_fields = vec![self.context.i32_type().into()];
        if !payload_types.is_empty() {
            // Use the first payload type for now (simplified)
            // In a full implementation, we'd create a proper union
            struct_fields.push(payload_types[0].into());
        }

        let enum_ty = self.context.opaque_struct_type(name);
        enum_ty.set_body(&struct_fields, false);

        self.enum_types.insert(name.to_string(), enum_ty);
        Ok(())
    }

    /// Declare a function (forward declaration)
    fn declare_function(&mut self, func: &AnnotatedFunction) -> Result<()> {
        let name = mangle_function_name(&func.name.to_string());

        let param_types: Result<Vec<_>> = func
            .params
            .iter()
            .map(|param| {
                type_to_llvm_type(
                    &param.type_annotation,
                    &self.context,
                    &self.struct_types,
                    &self.enum_types,
                )
            })
            .collect();

        let return_type = return_type_to_llvm_type(
            &func.return_type,
            &self.context,
            &self.struct_types,
            &self.enum_types,
        )?;

        let fn_type = return_type.fn_type(
            &param_types?.iter().map(|t| t.into()).collect::<Vec<_>>(),
            false,
        );

        let function = self.module.add_function(&name, fn_type, None);
        self.functions_mut().insert(func.name.to_string(), function);
        Ok(())
    }

    /// Generate code for a function
    fn codegen_function(&mut self, func: &AnnotatedFunction) -> Result<()> {
        let name = mangle_function_name(&func.name.to_string());
        let function = self
            .functions()
            .get(&func.name.to_string())
            .ok_or_else(|| miette!("Function {} not found", func.name))?;

        // Create entry block
        let basic_block = self.context.append_basic_block(*function, "entry");
        self.builder().position_at_end(basic_block);

        self.current_function = Some(*function);

        // Allocate space for parameters
        for (i, param) in func.params.iter().enumerate() {
            let param_value = function
                .get_nth_param(i as u32)
                .ok_or_else(|| miette!("Parameter {} not found", i))?;

            let param_type = type_to_llvm_type(
                &param.type_annotation,
                &self.context,
                &self.struct_types,
                &self.enum_types,
            )?;

            // Allocate stack space for parameter
            let alloca = self
                .builder()
                .build_alloca(param_type, &param.name)
                .map_err(|e| miette!("Failed to alloca parameter: {}", e))?;
            self.builder()
                .build_store(alloca, param_value)
                .map_err(|e| miette!("Failed to store parameter: {}", e))?;
            self.variables_mut().insert(param.name.to_string(), alloca);
        }

        // Generate function body
        let return_value = self.codegen_expr_list(&func.body)?;

        // Handle return
        match return_value {
            Some(value) => {
                self.builder()
                    .build_return(Some(&value))
                    .map_err(|e| miette!("Failed to build return: {}", e))?;
            }
            None => {
                // Check if return type is void
                if matches!(func.return_type, Type::Primitive(PrimitiveType::Nil)) {
                    self.builder()
                        .build_return(None)
                        .map_err(|e| miette!("Failed to build return: {}", e))?;
                } else {
                    return Err(miette!("Function must return a value"));
                }
            }
        }

        self.set_current_function(None);
        self.variables.clear(); // Clear local variables
        Ok(())
    }

    /// Generate code for a global variable
    fn codegen_global_variable(&mut self, var: &AnnotatedVariableDecl) -> Result<()> {
        let var_type = type_to_llvm_type(
            &var.type_annotation,
            &self.context,
            &self.struct_types,
            &self.enum_types,
        )?;

        // Create global variable
        let global = self.module.add_global(var_type, None, &var.name);

        // Generate initializer - for globals, we need to create a constant
        // This is simplified - in practice, we'd need to handle this differently
        // For now, skip initialization of globals with complex expressions
        if let Expr::Literal(Literal::Number(n)) = &var.value.expr {
            if let BasicTypeEnum::IntType(int_ty) = var_type {
                let const_val = int_ty.const_int(*n as u64, false);
                global.set_initializer(&const_val);
            }
        }
        global.set_constant(false);

        Ok(())
    }

    /// Generate code for an expression list
    fn codegen_expr_list(
        &mut self,
        expr_list: &AnnotatedExprList,
    ) -> Result<Option<BasicValueEnum<'static>>> {
        match expr_list {
            AnnotatedExprList::Single(expr) => Ok(Some(codegen_expr(self, expr)?)),
            AnnotatedExprList::Block(stmts) => {
                let mut last_value = None;
                for stmt in stmts {
                    last_value = self.codegen_stmt(stmt)?;
                }
                Ok(last_value)
            }
        }
    }

    /// Generate code for a statement
    fn codegen_stmt(&mut self, stmt: &AnnotatedStmt) -> Result<Option<BasicValueEnum<'static>>> {
        match stmt {
            AnnotatedStmt::VariableDecl(var) => {
                self.codegen_variable_decl(var)?;
                Ok(None)
            }
            AnnotatedStmt::Expr(expr) => Ok(Some(codegen_expr(self, expr)?)),
            AnnotatedStmt::Return(expr) => {
                if let Some(e) = expr {
                    let value = codegen_expr(self, e)?;
                    self.builder()
                        .build_return(Some(&value))
                        .map_err(|e| miette!("Failed to build return: {}", e))?;
                } else {
                    self.builder()
                        .build_return(None)
                        .map_err(|e| miette!("Failed to build return: {}", e))?;
                }
                Ok(None)
            }
        }
    }

    /// Generate code for a variable declaration
    fn codegen_variable_decl(&mut self, var: &AnnotatedVariableDecl) -> Result<()> {
        let var_type = type_to_llvm_type(
            &var.type_annotation,
            &self.context,
            &self.struct_types,
            &self.enum_types,
        )?;

        // Allocate stack space
        let alloca = self
            .builder()
            .build_alloca(var_type, &var.name)
            .map_err(|e| miette!("Failed to alloca variable: {}", e))?;

        // Generate initializer
        let init_value = codegen_expr(self, &var.value)?;
        self.builder()
            .build_store(alloca, init_value)
            .map_err(|e| miette!("Failed to store variable: {}", e))?;

        // Add to symbol table
        self.variables_mut().insert(var.name.to_string(), alloca);
        Ok(())
    }

    /// Emit LLVM IR as a string
    pub fn emit(&self) -> String {
        self.module.to_string()
    }
}
