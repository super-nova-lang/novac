mod annotated;
mod context;
mod errors;
mod types;

use crate::analyzer::annotated::*;
use crate::analyzer::context::TypeContext;
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

    /// Analyze a function
    fn analyze_function(
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
                resolve_type(ty, &func_ctx, self.dummy_span())?
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
            resolve_type(declared_return, &func_ctx, self.dummy_span())?
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
            let expected_type = resolve_type(annotation, ctx, self.dummy_span())?;
            // Check compatibility
            if !types_compatible(&expected_type, value_type, ctx) {
                return Err(TypeMismatchError {
                    expected: format_type(&expected_type),
                    actual: format_type(value_type),
                    expected_span: self.dummy_span(),
                    actual_span: self.dummy_span(),
                }
                .into());
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
            Expr::Literal(lit) => self.infer_literal_type(lit),
            Expr::Ident(name) => {
                ctx.lookup_variable(name.as_ref())
                    .ok_or_else(|| {
                        UndefinedVariableError {
                            name: name.to_string(),
                            span: self.dummy_span(),
                        }
                        .into()
                    })
                    .map(|ty| ty.clone())
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
                            return Err(WrongArgumentCountError {
                                expected: func.params.len(),
                                actual: args.len(),
                                span: self.dummy_span(),
                            }
                            .into());
                        }
                        // Check argument types
                        for (arg, param) in args.iter().zip(func.params.iter()) {
                            let arg_type = self.infer_expr_type(arg, ctx)?;
                            if !types_compatible(&param.type_annotation, &arg_type, ctx) {
                                return Err(TypeMismatchError {
                                    expected: format_type(&param.type_annotation),
                                    actual: format_type(&arg_type),
                                    expected_span: self.dummy_span(),
                                    actual_span: self.dummy_span(),
                                }
                                .into());
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
                                    return Err(WrongArgumentCountError {
                                        expected: params.len(),
                                        actual: args.len(),
                                        span: self.dummy_span(),
                                    }
                                    .into());
                                }
                                // Check argument types
                                for (arg, param_type) in args.iter().zip(params.iter()) {
                                    let arg_type = self.infer_expr_type(arg, ctx)?;
                                    if !types_compatible(param_type, &arg_type, ctx) {
                                        return Err(TypeMismatchError {
                                            expected: format_type(param_type),
                                            actual: format_type(&arg_type),
                                            expected_span: self.dummy_span(),
                                            actual_span: self.dummy_span(),
                                        }
                                        .into());
                                    }
                                }
                                // Function types don't have return types in the current AST
                                // For now, return a placeholder
                                Ok(Type::Primitive(PrimitiveType::Nil))
                            }
                            _ => Err(InvalidOperationError {
                                operation: "function call".to_string(),
                                type_name: format_type(&callee_type),
                                span: self.dummy_span(),
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
                                return Err(WrongArgumentCountError {
                                    expected: params.len(),
                                    actual: args.len(),
                                    span: self.dummy_span(),
                                }
                                .into());
                            }
                            // Check argument types
                            for (arg, param_type) in args.iter().zip(params.iter()) {
                                let arg_type = self.infer_expr_type(arg, ctx)?;
                                if !types_compatible(param_type, &arg_type, ctx) {
                                    return Err(TypeMismatchError {
                                        expected: format_type(param_type),
                                        actual: format_type(&arg_type),
                                        expected_span: self.dummy_span(),
                                        actual_span: self.dummy_span(),
                                    }
                                    .into());
                                }
                            }
                            // Function types don't have return types in the current AST
                            // For now, return a placeholder
                            Ok(Type::Primitive(PrimitiveType::Nil))
                        }
                        _ => Err(InvalidOperationError {
                            operation: "function call".to_string(),
                            type_name: format_type(&callee_type),
                            span: self.dummy_span(),
                        }
                        .into()),
                    }
                }
            }
            Expr::MethodCall {
                receiver,
                method: _,
                args: _,
            } => {
                // TODO: Implement method call type inference
                // For now, return a placeholder
                let _receiver_type = self.infer_expr_type(receiver, ctx)?;
                Ok(Type::Primitive(PrimitiveType::Nil))
            }
            Expr::Member { object, field } => {
                let _object_type = self.infer_expr_type(object, ctx)?;
                // TODO: Implement member access type inference
                match field {
                    MemberField::Name(_) => {
                        // Lookup field in struct type
                        Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
                    }
                    MemberField::Index(_) => {
                        // Index access - could be list or tuple
                        Ok(Type::Primitive(PrimitiveType::Nil)) // Placeholder
                    }
                }
            }
            Expr::StructLit { type_, fields: _ } => {
                if let Some(ty) = type_ {
                    resolve_type(ty, ctx, self.dummy_span())
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
                    Err(TypeInferenceError {
                        message: "Cannot infer type of empty list, type annotation required".to_string(),
                        span: self.dummy_span(),
                    }
                    .into())
                } else {
                    // Infer from first element
                    let first_type = self.infer_expr_type(&elements[0], ctx)?;
                    // Check all elements have compatible types
                    for element in elements.iter().skip(1) {
                        let elem_type = self.infer_expr_type(element, ctx)?;
                        if !types_compatible(&first_type, &elem_type, ctx) {
                            return Err(TypeMismatchError {
                                expected: format_type(&first_type),
                                actual: format_type(&elem_type),
                                expected_span: self.dummy_span(),
                                actual_span: self.dummy_span(),
                            }
                            .into());
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
                // TODO: Handle builtin calls
                Ok(Type::Primitive(PrimitiveType::Nil))
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
                    return Err(ReturnTypeMismatchError {
                        expected: format_type(expected_return),
                        actual: format_type(&actual_type),
                        expected_span: self.dummy_span(),
                        actual_span: self.dummy_span(),
                    }
                    .into());
                }
                Ok(())
            }
            ExprList::Block(stmts) => {
                // Check all return statements
                for stmt in stmts {
                    if let Stmt::Return(Some(expr)) = stmt {
                        let actual_type = self.infer_expr_type(expr, ctx)?;
                        if !types_compatible(expected_return, &actual_type, ctx) {
                            return Err(ReturnTypeMismatchError {
                                expected: format_type(expected_return),
                                actual: format_type(&actual_type),
                                expected_span: self.dummy_span(),
                                actual_span: self.dummy_span(),
                            }
                            .into());
                        }
                    } else if let Stmt::Return(None) = stmt {
                        // Return without value - check if return type is nil
                        if !types_compatible(expected_return, &Type::Primitive(PrimitiveType::Nil), ctx) {
                            return Err(MissingReturnValueError {
                                expected: format_type(expected_return),
                                span: self.dummy_span(),
                            }
                            .into());
                        }
                    }
                }
                Ok(())
            }
        }
    }

    /// Get a dummy span for error reporting (in a real implementation, we'd track spans)
    fn dummy_span(&self) -> SourceSpan {
        SourceSpan::from(0..0)
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
pub use annotated::*;
