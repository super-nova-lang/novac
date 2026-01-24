pub mod annotated;
mod context;
mod errors;
mod types;

use crate::analyzer::errors::*;
use crate::analyzer::types::*;
use crate::parser::ast::*;
use miette::{Error, SourceSpan};
use std::borrow::Cow;

/// Main analyzer that performs type checking and inference
pub struct Analyzer<'de> {
    source: &'de str,
    functions: std::collections::HashMap<Cow<'de, str>, AnnotatedFunction<'de>>,
}

impl<'de> Analyzer<'de> {
    /// Create a new analyzer
    pub fn new(source: &'de str) -> Self {
        Self {
            source,
            functions: std::collections::HashMap::new(),
        }
    }

    /// Wrap an error with source code for better diagnostics
    fn wrap_error_with_source(&self, err: Error) -> Error {
        err.with_source_code(self.source.to_string())
    }

    /// Analyze a program and return an annotated AST
    pub fn analyze(&mut self, program: Program<'de>) -> Result<AnnotatedProgram<'de>, Error> {
        let mut ctx = TypeContext::new();

        // First pass: collect type declarations
        for item in &program.items {
            if let TopLevelItem::TypeDecl(type_decl) = item {
                ctx.add_type(type_decl.name.clone(), type_decl.clone());
            }
        }

        // Second pass: analyze functions and collect their signatures
        // We need to do this in two sub-passes:
        // 1. First, collect function signatures (for forward references)
        // 2. Then fully analyze functions
        
        // For now, we'll analyze functions as we encounter them
        // In a more sophisticated implementation, we'd do a full forward reference pass
        
        // Third pass: analyze all items
        let mut annotated_items = Vec::new();
        for item in program.items {
            match item {
                TopLevelItem::TypeDecl(type_decl) => {
                    annotated_items.push(AnnotatedTopLevelItem::TypeDecl(type_decl));
                }
                TopLevelItem::Function(func) => {
                    let annotated_func = self.analyze_function(&func, &mut ctx)?;
                    self.functions
                        .insert(annotated_func.name.clone(), annotated_func.clone());
                    annotated_items.push(AnnotatedTopLevelItem::Function(annotated_func));
                }
                TopLevelItem::VariableDecl(var) => {
                    let annotated_var = self.analyze_variable_decl(&var, &mut ctx)?;
                    annotated_items.push(AnnotatedTopLevelItem::VariableDecl(annotated_var));
                }
            }
        }

        Ok(AnnotatedProgram {
            items: annotated_items,
        })
    }

    /// Analyze a function (public for use in documentation generation)
    pub fn analyze_function(
        &mut self,
        func: &Function<'de>,
        ctx: &mut TypeContext<'de>,
    ) -> Result<AnnotatedFunction<'de>, Error> {
        // Create a new scope for the function
        let mut func_ctx = ctx.new_scope();

        // Analyze parameters
        let mut annotated_params = Vec::new();
        for param in &func.params {
            let param_type = if let Some(ty) = &param.type_annotation {
                // Resolve the type
                resolve_type(ty, &func_ctx, self.span_for_type(ty))?
            } else {
                // No type annotation - try to infer from usage in function body
                // For now, we'll use a placeholder type and let the body inference handle it
                // In a more sophisticated implementation, we'd do bidirectional type checking
                // For gradual typing, we allow unannotated parameters but can't fully type-check
                default_numeric_type() // Default to i32 for unannotated numeric parameters
            };

            func_ctx.add_variable(param.name.clone(), param_type.clone());
            annotated_params.push(AnnotatedFunctionParam {
                name: param.name.clone(),
                type_annotation: param_type,
            });
        }

        // Analyze function body
        let annotated_body = self.analyze_expr_list(&func.body, &mut func_ctx)?;

        // Determine return type
        let return_type = if let Some(declared_return) = &func.return_type {
            // Function has explicit return type
            resolve_type(declared_return, &func_ctx, self.span_for_type(declared_return))?
        } else {
            // Infer return type from body
            self.infer_expr_list_type(&func.body, &func_ctx)?
        };

        // Check return statements match the return type
        self.check_return_statements(&func.body, &return_type, &func_ctx)?;

        Ok(AnnotatedFunction {
            name: func.name.clone(),
            generics: func.generics.clone(),
            params: annotated_params,
            return_type,
            body: annotated_body,
        })
    }

    /// Analyze a variable declaration
    fn analyze_variable_decl(
        &mut self,
        var: &VariableDecl<'de>,
        ctx: &mut TypeContext<'de>,
    ) -> Result<AnnotatedVariableDecl<'de>, Error> {
        // Analyze the value expression
        let annotated_value = self.analyze_expr(&var.value, ctx)?;
        let value_type = &annotated_value.ty;

        let var_type = if let Some(annotation) = &var.type_annotation {
            // Variable has explicit type annotation
            let expected_type = resolve_type(annotation, ctx, self.span_for_type(annotation))?;
            // Check compatibility
            if !types_compatible(&expected_type, value_type, ctx) {
                return Err(self.wrap_error_with_source(
                    TypeMismatchError {
                        expected: format_type(&expected_type),
                        actual: format_type(value_type),
                        expected_span: self.span_for_type(annotation),
                        actual_span: self.span_for_expr(&var.value),
                    }
                    .into(),
                ));
            }
            expected_type
        } else {
            // Infer type from value
            value_type.clone()
        };

        // Add variable to context
        ctx.add_variable(var.name.clone(), var_type.clone());

        Ok(AnnotatedVariableDecl {
            name: var.name.clone(),
            type_annotation: var_type,
            value: annotated_value,
        })
    }

    /// Analyze an expression and return an annotated expression
    fn analyze_expr(
        &mut self,
        expr: &Expr<'de>,
        ctx: &TypeContext<'de>,
    ) -> Result<Box<AnnotatedExpr<'de>>, Error> {
        let ty = self.infer_expr_type(expr, ctx)?;
        Ok(Box::new(AnnotatedExpr::new(expr.clone(), ty)))
    }

    /// Infer the type of an expression
    fn infer_expr_type(&mut self, expr: &Expr<'de>, ctx: &TypeContext<'de>) -> Result<Type<'de>, Error> {
        match expr {
            Expr::Literal(Literal::BuiltinCall(builtin)) => {
                self.infer_builtin_call_type(builtin, ctx)
            }
            Expr::Literal(lit) => self.infer_literal_type(lit),
            Expr::Ident(name) => {
                // First check if it's a type (for method calls like Person:new())
                if ctx.lookup_type(name.as_ref()).is_some() {
                    // It's a type - return the type
                    Ok(Type::Named(name.clone()))
                } else {
                    // Check if it's a variable
                    ctx.lookup_variable(name.as_ref())
                        .ok_or_else(|| {
                            self.wrap_error_with_source(
                                UndefinedVariableError {
                                    name: name.to_string(),
                                    span: self.span_for_ident(name),
                                }
                                .into(),
                            )
                        }).cloned()
                }
            }
            Expr::Binary { left, op, right } => {
                let left_type = self.infer_expr_type(left, ctx)?;
                let right_type = self.infer_expr_type(right, ctx)?;
                binary_op_result_type(op, &left_type, &right_type)
            }
            Expr::Unary { op, expr } => {
                let operand_type = self.infer_expr_type(expr, ctx)?;
                unary_op_result_type(op, &operand_type)
            }
            Expr::Call { callee, args } => {
                // Check if callee is an identifier (function name)
                if let Expr::Ident(name) = callee.as_ref() {
                    // Lookup function - need to clone to avoid borrow issues
                    let func_opt = self.functions.get(name).cloned();
                    if let Some(func) = func_opt {
                        // Check argument count
                        if args.len() != func.params.len() {
                            return Err(self.wrap_error_with_source(
                                WrongArgumentCountError {
                                    expected: func.params.len(),
                                    actual: args.len(),
                                    span: self.span_for_expr(callee),
                                }
                                .into(),
                            ));
                        }
                        // Check argument types
                        for (arg, param) in args.iter().zip(func.params.iter()) {
                            let arg_type = self.infer_expr_type(arg, ctx)?;
                            if !types_compatible(&param.type_annotation, &arg_type, ctx) {
                                return Err(self.wrap_error_with_source(
                                    TypeMismatchError {
                                        expected: format_type(&param.type_annotation),
                                        actual: format_type(&arg_type),
                                        expected_span: self.span_for_type(&param.type_annotation),
                                        actual_span: self.span_for_expr(arg),
                                    }
                                    .into(),
                                ));
                            }
                        }
                        // Return function's return type
                        Ok(func.return_type)
                    } else {
                        // Try to infer callee type - should be a function type
                        let callee_type = self.infer_expr_type(callee, ctx)?;
                        match callee_type {
                            Type::Function { params } => {
                                // Check argument count
                                if args.len() != params.len() {
                                    return Err(self.wrap_error_with_source(
                                        WrongArgumentCountError {
                                            expected: params.len(),
                                            actual: args.len(),
                                            span: self.span_for_expr(callee),
                                        }
                                        .into(),
                                    ));
                                }
                                // Check argument types
                                for (arg, param_type) in args.iter().zip(params.iter()) {
                                    let arg_type = self.infer_expr_type(arg, ctx)?;
                                    if !types_compatible(param_type, &arg_type, ctx) {
                                        return Err(self.wrap_error_with_source(
                                            TypeMismatchError {
                                                expected: format_type(param_type),
                                                actual: format_type(&arg_type),
                                                expected_span: self.span_for_type(param_type),
                                                actual_span: self.span_for_expr(arg),
                                            }
                                            .into(),
                                        ));
                                    }
                                }
                                // Function types don't have return types in the current AST
                                // For now, return a placeholder
                                Ok(Type::Primitive(PrimitiveType::Nil))
                            }
                            _ => Err(InvalidOperationError {
                                operation: "function call".to_string(),
                                type_name: format_type(&callee_type),
                                span: self.span_for_expr(callee),
                            }
                            .into()),
                        }
                    }
                } else {
                    // Callee is not an identifier - try to infer its type
                    let callee_type = self.infer_expr_type(callee, ctx)?;
                    match callee_type {
                        Type::Function { params } => {
                            // Check argument count
                            if args.len() != params.len() {
                                return Err(self.wrap_error_with_source(
                                    WrongArgumentCountError {
                                        expected: params.len(),
                                        actual: args.len(),
                                        span: self.span_for_expr(callee),
                                    }
                                    .into(),
                                ));
                            }
                            // Check argument types
                            for (arg, param_type) in args.iter().zip(params.iter()) {
                                let arg_type = self.infer_expr_type(arg, ctx)?;
                                if !types_compatible(param_type, &arg_type, ctx) {
                                    return Err(self.wrap_error_with_source(
                                        TypeMismatchError {
                                            expected: format_type(param_type),
                                            actual: format_type(&arg_type),
                                            expected_span: self.span_for_type(param_type),
                                            actual_span: self.span_for_expr(arg),
                                        }
                                        .into(),
                                    ));
                                }
                            }
                            // Function types don't have return types in the current AST
                            // For now, return a placeholder
                            Ok(Type::Primitive(PrimitiveType::Nil))
                        }
                        _ => Err(self.wrap_error_with_source(
                            InvalidOperationError {
                                operation: "function call".to_string(),
                                type_name: format_type(&callee_type),
                                span: self.span_for_expr(callee),
                            }
                            .into(),
                        )),
                    }
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                // Infer receiver type
                let receiver_type = self.infer_expr_type(receiver, ctx)?;
                
                // Check if receiver is a type (for static methods like Person:new())
                if let Type::Named(type_name) = &receiver_type {
                    // Look up the type declaration
                    if let Some(type_decl) = ctx.lookup_type(type_name.as_ref())
                        && let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
                            // Look for the method in the impl block
                            if let Some(impl_block) = &struct_decl.impl_block {
                                for func in impl_block {
                                    if func.name.as_ref() == method.as_ref() {
                                        // Found the method! Check arguments and return type
                                        // Check argument count
                                        // For static methods, check all params
                                        // For instance methods, skip 'self' if present
                                        let expected_arg_count = if func.params.first()
                                            .map(|p| p.name.as_ref() == "self")
                                            .unwrap_or(false)
                                        {
                                            func.params.len() - 1  // Skip 'self'
                                        } else {
                                            func.params.len()
                                        };
                                        
                                        if args.len() != expected_arg_count {
                                            return Err(self.wrap_error_with_source(
                                                WrongArgumentCountError {
                                                    expected: expected_arg_count,
                                                    actual: args.len(),
                                                    span: self.span_for_expr(receiver),
                                                }
                                                .into(),
                                            ));
                                        }
                                        
                                        // Check argument types
                                        // For static methods (Person:new), check all params
                                        // For instance methods, skip 'self' parameter
                                        let params_to_check = if func.params.first()
                                            .map(|p| p.name.as_ref() == "self")
                                            .unwrap_or(false)
                                        {
                                            // Instance method - skip 'self' parameter
                                            &func.params[1..]
                                        } else {
                                            // Static method - check all params
                                            &func.params[..]
                                        };
                                        
                                        for (arg, param) in args.iter().zip(params_to_check.iter()) {
                                            let arg_type = self.infer_expr_type(arg, ctx)?;
                                            
                                            // If parameter has type annotation, check it
                                            if let Some(param_ty) = &param.type_annotation {
                                                let param_type = resolve_type(param_ty, ctx, self.span_for_type(param_ty))?;
                                                if !types_compatible(&param_type, &arg_type, ctx) {
                                                    return Err(self.wrap_error_with_source(
                                                        TypeMismatchError {
                                                            expected: format_type(&param_type),
                                                            actual: format_type(&arg_type),
                                                            expected_span: self.span_for_type(param_ty),
                                                            actual_span: self.span_for_expr(arg),
                                                        }
                                                        .into(),
                                                    ));
                                                }
                                            }
                                            // If no type annotation, we can't strictly check, but that's okay for gradual typing
                                        }
                                        
                                        // Return the method's return type, or the struct type if it's Self
                                        if let Some(return_type) = &func.return_type {
                                            // Check if return type is Self before resolving
                                            if let Type::Named(name) = return_type
                                                && name.as_ref() == "Self" {
                                                    // Return the struct type
                                                    return Ok(receiver_type.clone());
                                                }
                                            // Resolve the return type
                                            let resolved = resolve_type(return_type, ctx, self.span_for_type(return_type))?;
                                            return Ok(resolved);
                                        } else {
                                            // No return type specified - infer from body or return struct type
                                            // For now, if it's a constructor-like method (new), return the struct type
                                            if method.as_ref() == "new" {
                                                return Ok(receiver_type.clone());
                                            }
                                            // Otherwise, try to infer from body
                                            return Ok(Type::Primitive(PrimitiveType::Nil));
                                        }
                                    }
                                }
                            }
                        }
                }
                
                // For instance method calls (receiver is a value, not a type)
                // Infer the receiver type
                let receiver_type = self.infer_expr_type(receiver, ctx)?;
                
                // Check if receiver is a struct type
                if let Type::Named(type_name) = &receiver_type {
                    // Look up the type declaration
                    if let Some(type_decl) = ctx.lookup_type(type_name.as_ref())
                        && let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
                            // Look for the method in the impl block
                            if let Some(impl_block) = &struct_decl.impl_block {
                                for func in impl_block {
                                    if func.name.as_ref() == method.as_ref() {
                                        // Found the method! Check arguments and return type
                                        // For instance methods, first param should be 'self'
                                        let expected_arg_count = if func.params.first()
                                            .map(|p| p.name.as_ref() == "self")
                                            .unwrap_or(false)
                                        {
                                            func.params.len() - 1  // Skip 'self'
                                        } else {
                                            func.params.len()
                                        };
                                        
                                        if args.len() != expected_arg_count {
                                            return Err(self.wrap_error_with_source(
                                                WrongArgumentCountError {
                                                    expected: expected_arg_count,
                                                    actual: args.len(),
                                                    span: self.span_for_expr(receiver),
                                                }
                                                .into(),
                                            ));
                                        }
                                        
                                        // Check argument types (skip 'self' if present)
                                        let params_to_check = if func.params.first()
                                            .map(|p| p.name.as_ref() == "self")
                                            .unwrap_or(false)
                                        {
                                            &func.params[1..]
                                        } else {
                                            &func.params[..]
                                        };
                                        
                                        for (arg, param) in args.iter().zip(params_to_check.iter()) {
                                            let arg_type = self.infer_expr_type(arg, ctx)?;
                                            
                                            // If parameter has type annotation, check it
                                            if let Some(param_ty) = &param.type_annotation {
                                                let param_type = resolve_type(param_ty, ctx, self.span_for_type(param_ty))?;
                                                if !types_compatible(&param_type, &arg_type, ctx) {
                                                    return Err(self.wrap_error_with_source(
                                                        TypeMismatchError {
                                                            expected: format_type(&param_type),
                                                            actual: format_type(&arg_type),
                                                            expected_span: self.span_for_type(param_ty),
                                                            actual_span: self.span_for_expr(arg),
                                                        }
                                                        .into(),
                                                    ));
                                                }
                                            }
                                        }
                                        
                                        // Return the method's return type, or nil if none
                                        if let Some(return_type) = &func.return_type {
                                            // Check if return type is Self
                                            if let Type::Named(name) = return_type
                                                && name.as_ref() == "Self" {
                                                    // Return the struct type
                                                    return Ok(receiver_type.clone());
                                                }
                                            // Resolve the return type
                                            let resolved = resolve_type(return_type, ctx, self.span_for_type(return_type))?;
                                            return Ok(resolved);
                                        } else {
                                            // No return type - return nil
                                            return Ok(Type::Primitive(PrimitiveType::Nil));
                                        }
                                    }
                                }
                            }
                        }
                }
                
                // Method not found - return nil (will cause error if called)
                Ok(Type::Primitive(PrimitiveType::Nil))
            }
            Expr::Member { object, field } => {
                let object_type = self.infer_expr_type(object, ctx)?;
                match field {
                    MemberField::Name(field_name) => {
                        // Check if object is a struct type
                        if let Type::Named(type_name) = &object_type {
                            // Look up the type declaration
                            if let Some(type_decl) = ctx.lookup_type(type_name.as_ref())
                                && let TypeDeclKind::Struct(struct_decl) = &type_decl.decl {
                                    // Look for the field in the struct
                                    for struct_field in &struct_decl.fields {
                                        if struct_field.name.as_ref() == field_name.as_ref() {
                                            return Ok(struct_field.type_.clone());
                                        }
                                    }
                                    // Field not found - might be a method call
                                    // Check if it's a method in the impl block
                                    if let Some(impl_block) = &struct_decl.impl_block {
                                        for func in impl_block {
                                            if func.name.as_ref() == field_name.as_ref() {
                                                // Found a method! Return a function type
                                                // For now, return a placeholder function type
                                                // The actual call will be handled by MethodCall
                                                return Ok(Type::Function { params: vec![] });
                                            }
                                        }
                                    }
                                }
                        }
                        // Fallback: return nil for unknown member access
                        Ok(Type::Primitive(PrimitiveType::Nil))
                    }
                    MemberField::Index(_) => {
                        // Index access - could be list or tuple
                        Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
                    }
                }
            }
            Expr::StructLit { type_, fields: _ } => {
                if let Some(ty) = type_ {
                    resolve_type(ty, ctx, self.span_for_type(ty))
                } else {
                    // Anonymous struct - infer from fields
                    // TODO: Implement anonymous struct inference
                    Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
                }
            }
            Expr::EnumVariant { name: _, value: _ } => {
                // TODO: Implement enum variant type inference
                Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
            }
            Expr::AnonFn { params: _, body: _ } => {
                // TODO: Implement anonymous function type inference
                Ok(Type::Function { params: vec![] })
            }
            Expr::Match { expr: _, arms: _ } => {
                // TODO: Implement match expression type inference
                // For now, infer from first arm
                Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
            }
            Expr::If {
                condition: _,
                then_block: _,
                elif_blocks: _,
                else_block: _,
            } => {
                // TODO: Implement if expression type inference
                // All branches must have compatible types
                Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
            }
            Expr::List(elements) => {
                if elements.is_empty() {
                    // Empty list - cannot infer element type
                    // Use a span for "[]" by searching for it
                    let list_span = self.source
                        .find("[]")
                        .map(|pos| SourceSpan::new(pos.into(), 2))
                        .unwrap_or_else(|| self.dummy_span());
                    Err(self.wrap_error_with_source(
                        TypeInferenceError {
                            message: "Cannot infer type of empty list, type annotation required".to_string(),
                            span: list_span,
                        }
                        .into(),
                    ))
                } else {
                    // Infer from first element
                    let first_type = self.infer_expr_type(&elements[0], ctx)?;
                    // Check all elements have compatible types
                    for element in elements.iter().skip(1) {
                        let elem_type = self.infer_expr_type(element, ctx)?;
                        if !types_compatible(&first_type, &elem_type, ctx) {
                            return Err(self.wrap_error_with_source(
                                TypeMismatchError {
                                    expected: format_type(&first_type),
                                    actual: format_type(&elem_type),
                                    expected_span: self.span_for_expr(&elements[0]),
                                    actual_span: self.span_for_expr(element),
                                }
                                .into(),
                            ));
                        }
                    }
                    // List type - for now return the element type
                    // In a full implementation, we'd have Type::Generic { base: Box::new(Type::Primitive(PrimitiveType::List)), args: vec![first_type] }
                    Ok(Type::Primitive(PrimitiveType::List))
                }
            }
            Expr::Tuple(elements) => {
                let mut types = Vec::new();
                for element in elements {
                    types.push(self.infer_expr_type(element, ctx)?);
                }
                Ok(Type::Tuple(types))
            }
            Expr::Paren(expr) => self.infer_expr_type(expr, ctx),
        }
    }

    /// Infer the type of a literal
    fn infer_literal_type(&self, lit: &Literal<'de>) -> Result<Type<'de>, Error> {
        match lit {
            Literal::Number(_) => Ok(default_numeric_type()),
            Literal::String(_) => Ok(Type::Primitive(PrimitiveType::Str)),
            Literal::Char(_) => Ok(Type::Primitive(PrimitiveType::Char)),
            Literal::Boolean(_) => {
                Ok(Type::Primitive(PrimitiveType::Bool))
            }
            Literal::Nil => Ok(Type::Primitive(PrimitiveType::Nil)),
            Literal::BuiltinCall(_) => {
                // Builtin calls are handled in infer_expr_type
                unreachable!("BuiltinCall should be handled in infer_expr_type")
            }
        }
    }

    /// Infer the type of a builtin call
    fn infer_builtin_call_type(
        &mut self,
        builtin: &BuiltinCall<'de>,
        ctx: &TypeContext<'de>,
    ) -> Result<Type<'de>, Error> {
        use crate::analyzer::errors::*;
        
        let args = builtin.args.as_deref().unwrap_or(&[]);
        let name = builtin.name.as_ref();
        
        match name {
            "println" | "print" => {
                // @println and @print require at least 1 argument (format string)
                let builtin_span = self.span_for_expr(&Expr::Literal(Literal::BuiltinCall(builtin.clone())));
                if args.is_empty() {
                    return Err(self.wrap_error_with_source(
                        WrongBuiltinArgumentCountError {
                            name: name.to_string(),
                            expected: 1,
                            actual: 0,
                            span: builtin_span,
                        }
                        .into(),
                    ));
                }
                
                // First argument must be a string
                let first_arg_type = self.infer_expr_type(&args[0], ctx)?;
                if !matches!(first_arg_type, Type::Primitive(PrimitiveType::Str)) {
                    return Err(self.wrap_error_with_source(
                        WrongBuiltinArgumentTypeError {
                            name: name.to_string(),
                            expected: "str".to_string(),
                            actual: format_type(&first_arg_type),
                            span: self.span_for_expr(&args[0]),
                        }
                        .into(),
                    ));
                }
                
                // Additional arguments can be any type (variadic)
                // We don't need to check them, just verify they exist if provided
                // Return Nil type
                Ok(Type::Primitive(PrimitiveType::Nil))
            }
            "unreachable" => {
                // @unreachable requires 0 arguments
                let builtin_span = self.span_for_expr(&Expr::Literal(Literal::BuiltinCall(builtin.clone())));
                if !args.is_empty() {
                    return Err(self.wrap_error_with_source(
                        WrongBuiltinArgumentCountError {
                            name: name.to_string(),
                            expected: 0,
                            actual: args.len(),
                            span: builtin_span,
                        }
                        .into(),
                    ));
                }
                
                // Return Nil type
                Ok(Type::Primitive(PrimitiveType::Nil))
            }
            _ => {
                // Unknown builtin
                let builtin_span = self.span_for_expr(&Expr::Literal(Literal::BuiltinCall(builtin.clone())));
                Err(self.wrap_error_with_source(
                    UnknownBuiltinError {
                        name: name.to_string(),
                        span: builtin_span,
                    }
                    .into(),
                ))
            }
        }
    }

    /// Analyze an expression list
    fn analyze_expr_list(
        &mut self,
        expr_list: &ExprList<'de>,
        ctx: &mut TypeContext<'de>,
    ) -> Result<AnnotatedExprList<'de>, Error> {
        match expr_list {
            ExprList::Single(expr) => {
                let annotated = self.analyze_expr(expr, ctx)?;
                Ok(AnnotatedExprList::Single(annotated))
            }
            ExprList::Block(stmts) => {
                let mut annotated_stmts = Vec::new();
                for stmt in stmts {
                    annotated_stmts.push(self.analyze_stmt(stmt, ctx)?);
                }
                Ok(AnnotatedExprList::Block(annotated_stmts))
            }
        }
    }

    /// Infer the type of an expression list
    fn infer_expr_list_type(
        &mut self,
        expr_list: &ExprList<'de>,
        ctx: &TypeContext<'de>,
    ) -> Result<Type<'de>, Error> {
        match expr_list {
            ExprList::Single(expr) => self.infer_expr_type(expr, ctx),
            ExprList::Block(stmts) => {
                // Block type is the type of the last statement
                if let Some(last_stmt) = stmts.last() {
                    match last_stmt {
                        Stmt::Return(Some(expr)) => self.infer_expr_type(expr, ctx),
                        Stmt::Return(None) => Ok(Type::Primitive(PrimitiveType::Nil)),
                        Stmt::Expr(expr) => self.infer_expr_type(expr, ctx),
                        Stmt::VariableDecl(_) => Ok(Type::Primitive(PrimitiveType::Nil)),
                    }
                } else {
                    Ok(Type::Primitive(PrimitiveType::Nil))
                }
            }
        }
    }

    /// Analyze a statement
    fn analyze_stmt(
        &mut self,
        stmt: &Stmt<'de>,
        ctx: &mut TypeContext<'de>,
    ) -> Result<AnnotatedStmt<'de>, Error> {
        match stmt {
            Stmt::VariableDecl(var) => {
                let annotated = self.analyze_variable_decl(var, ctx)?;
                Ok(AnnotatedStmt::VariableDecl(annotated))
            }
            Stmt::Expr(expr) => {
                let annotated = self.analyze_expr(expr, ctx)?;
                Ok(AnnotatedStmt::Expr(annotated))
            }
            Stmt::Return(expr) => {
                let annotated_expr = if let Some(e) = expr {
                    Some(self.analyze_expr(e, ctx)?)
                } else {
                    None
                };
                Ok(AnnotatedStmt::Return(annotated_expr))
            }
        }
    }

    /// Check that return statements match the expected return type
    fn check_return_statements(
        &mut self,
        expr_list: &ExprList<'de>,
        expected_return: &Type<'de>,
        ctx: &TypeContext<'de>,
    ) -> Result<(), Error> {
        match expr_list {
            ExprList::Single(expr) => {
                // Single expression - check its type matches return type
                let actual_type = self.infer_expr_type(expr, ctx)?;
                if !types_compatible(expected_return, &actual_type, ctx) {
                    return Err(self.wrap_error_with_source(
                        ReturnTypeMismatchError {
                            expected: format_type(expected_return),
                            actual: format_type(&actual_type),
                            expected_span: self.span_for_type(expected_return),
                            actual_span: self.span_for_expr(expr),
                        }
                        .into(),
                    ));
                }
                Ok(())
            }
            ExprList::Block(stmts) => {
                // Check all return statements
                for stmt in stmts {
                    if let Stmt::Return(Some(expr)) = stmt {
                        let actual_type = self.infer_expr_type(expr, ctx)?;
                        if !types_compatible(expected_return, &actual_type, ctx) {
                            return Err(self.wrap_error_with_source(
                                ReturnTypeMismatchError {
                                    expected: format_type(expected_return),
                                    actual: format_type(&actual_type),
                                    expected_span: self.span_for_type(expected_return),
                                    actual_span: self.span_for_expr(expr),
                                }
                                .into(),
                            ));
                        }
                    } else if let Stmt::Return(None) = stmt {
                        // Return without value - check if return type is nil
                        // For return statements, we'll use a span for "return" keyword
                        let return_span = self.span_for_ident("return");
                        if !types_compatible(expected_return, &Type::Primitive(PrimitiveType::Nil), ctx) {
                            return Err(self.wrap_error_with_source(
                                MissingReturnValueError {
                                    expected: format_type(expected_return),
                                    span: return_span,
                                }
                                .into(),
                            ));
                        }
                    }
                }
                Ok(())
            }
        }
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
                c.map(|ch| !ch.is_alphanumeric() && ch != '_').unwrap_or(true)
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
}

/// Public API: Analyze a program
pub fn analyze<'de>(
    program: Program<'de>,
    source: &'de str,
) -> Result<AnnotatedProgram<'de>, Error> {
    let mut analyzer = Analyzer::new(source);
    analyzer.analyze(program)
}

// Re-export annotated types for convenience
pub use annotated::{
    AnnotatedExpr, AnnotatedExprList, AnnotatedFunction, AnnotatedFunctionParam,
    AnnotatedProgram, AnnotatedStmt, AnnotatedTopLevelItem, AnnotatedVariableDecl,
};

// Re-export context for documentation generation
pub type TypeContext<'de> = context::TypeContext<'de>;

// Re-export types utilities for documentation generation
pub use types::default_numeric_type;
