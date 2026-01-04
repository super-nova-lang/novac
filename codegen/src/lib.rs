#![allow(dead_code)]

use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue};
use parser::nodes::*;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("unsupported top-level node: {0}")]
    UnsupportedNode(String),
    #[error("unsupported statement: {0}")]
    UnsupportedStatement(String),
    #[error("unsupported expression: {0}")]
    UnsupportedExpression(String),
    #[error("unsupported parameter kind: {0}")]
    UnsupportedParam(String),
    #[error("unsupported type: {0:?}")]
    UnsupportedType(Type),
    #[error("unknown identifier: {0}")]
    UnknownIdent(String),
    #[error("call needs a function name")]
    MissingCallee,
    #[error("function missing a return: {0}")]
    MissingReturn(String),
    #[error("call returned void where a value was expected")]
    VoidValue,
    #[error("builder error: {0}")]
    Builder(#[from] BuilderError),
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
        }
    }

    pub fn compile(mut self, nodes: &[Node]) -> Result<Module<'ctx>, CodegenError> {
        let mut decls: Vec<DeclStmt> = Vec::new();
        for node in nodes {
            if let Node::Statement(stmt) = node {
                match stmt {
                    Statement::Decl(decl) => match decl {
                        DeclStmt::ImportDecl {
                            name, link_name, ..
                        } => {
                            self.declare_import(name, link_name);
                        }
                        DeclStmt::Decl { body, params: parent_params, .. } if Self::is_type_decl(body) => {
                            // For type-like declarations (struct/enum/derive), declare functions defined in their `with`/derive blocks
                            if let Some(expr) = &body.1 {
                                match expr.as_ref() {
                                    Expression::StructExpr(_, Some(with_block)) | Expression::EnumExpr(_, Some(with_block)) => {
                                        // collect parent params to prepend to inner decls
                                        let parent_params_clone = parent_params.clone();
                                        for node in with_block {
                                            if let Node::Statement(Statement::Decl(inner)) = node {
                                                if let DeclStmt::Decl { tags, name, generics, params: inner_params, explicit_ret, body } = inner {
                                                    // Only hoist inner decls that take parameters (methods like `self`); skip simple field accessors
                                                    if inner_params.is_empty() {
                                                        continue;
                                                    }
                                                    // create a new decl with parent params prepended
                                                    let mut combined_params = parent_params_clone.clone();
                                                    combined_params.extend(inner_params.clone());
                                                    let new_decl = DeclStmt::Decl {
                                                        tags: tags.clone(),
                                                        name: name.clone(),
                                                        generics: generics.clone(),
                                                        params: combined_params,
                                                        explicit_ret: explicit_ret.clone(),
                                                        body: body.clone(),
                                                    };
                                                    self.declare_function(&new_decl)?;
                                                    decls.push(new_decl);
                                                }
                                            }
                                        }
                                    }
                                    Expression::DeriveExpr(nodes) => {
                                        let parent_params_clone = parent_params.clone();
                                        for node in nodes {
                                            if let Node::Statement(Statement::Decl(inner)) = node {
                                                if let DeclStmt::Decl { tags, name, generics, params: inner_params, explicit_ret, body } = inner {
                                                    let mut combined_params = parent_params_clone.clone();
                                                    combined_params.extend(inner_params.clone());
                                                    let new_decl = DeclStmt::Decl {
                                                        tags: tags.clone(),
                                                        name: name.clone(),
                                                        generics: generics.clone(),
                                                        params: combined_params,
                                                        explicit_ret: explicit_ret.clone(),
                                                        body: body.clone(),
                                                    };
                                                    self.declare_function(&new_decl)?;
                                                    decls.push(new_decl);
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        DeclStmt::Decl { .. } => {
                            self.declare_function(decl)?;
                            decls.push(decl.clone());
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        for decl in &decls {
            if let DeclStmt::Decl { name, .. } = decl {
                if let Some(func) = self.functions.get(name).copied() {
                    self.define_function(decl, func)?;
                }
            }
        }
        Ok(self.module)
    }

    fn declare_import(&mut self, name: &str, link_name: &str) {
        if self.functions.contains_key(name) {
            return;
        }
        if let Some(existing) = self.module.get_function(name) {
            self.functions.insert(name.to_string(), existing);
            return;
        }

        let i8_ptr = self.context.ptr_type(AddressSpace::default());
        let fn_type = self.context.i32_type().fn_type(&[i8_ptr.into()], true);
        let function = self.module.add_function(link_name, fn_type, None);
        self.functions.insert(name.to_string(), function);
    }

    fn is_type_decl(body: &DeclBody) -> bool {
        if let Some(expr) = &body.1 {
            matches!(
                expr.as_ref(),
                Expression::StructExpr(_, _)
                    | Expression::EnumExpr(_, _)
                    | Expression::DeriveExpr(_)
            )
        } else {
            false
        }
    }

    fn declare_function(&mut self, decl: &DeclStmt) -> Result<(), CodegenError> {
        if let DeclStmt::Decl {
            tags: _,
            generics: _,
            name,
            params,
            explicit_ret,
            body: _,
        } = decl
        {
            if self.functions.contains_key(name) {
                return Ok(());
            }

            let (arg_types, _) = self.lower_params(params)?;
            let ret_type = self.lower_ret_type(explicit_ret)?;
            let fn_type = match ret_type {
                Some(basic) => basic.fn_type(&arg_types, false),
                None => self.context.void_type().fn_type(&arg_types, false),
            };

            let function = self.module.add_function(name, fn_type, None);
            self.functions.insert(name.clone(), function);
            Ok(())
        } else {
            Err(CodegenError::UnsupportedNode(format!("{:?}", decl)))
        }
    }

    fn define_function(
        &mut self,
        decl: &DeclStmt,
        function: FunctionValue<'ctx>,
    ) -> Result<(), CodegenError> {
        if let DeclStmt::Decl {
            tags: _,
            generics: _,
            name,
            params,
            explicit_ret,
            body,
        } = decl
        {
            let (_, param_idents) = self.lower_params(params)?;
            let ret_type = self.lower_ret_type(explicit_ret)?;

            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            let mut env: HashMap<String, BasicValueEnum<'ctx>> = HashMap::new();
            for (idx, ident) in param_idents.iter().enumerate() {
                if let Some(param) = function.get_nth_param(idx as u32) {
                    env.insert(ident.clone(), param);
                }
            }

            let mut did_return = false;
            for stmt in &body.0 {
                did_return |= self.emit_statement(stmt, &mut env, function)?;
            }

            if did_return {
                return Ok(());
            }

            if let Some(expr) = &body.1 {
                if ret_type.is_none() {
                    let _ = self.emit_expression(expr, &mut env)?;
                    let _ = self.builder.build_return(None);
                } else {
                    let val = self.emit_expression(expr, &mut env)?;
                    let _ = self.builder.build_return(Some(&val));
                }
                return Ok(());
            }

            if ret_type.is_none() {
                let _ = self.builder.build_return(None);
                Ok(())
            } else {
                Err(CodegenError::MissingReturn(name.clone()))
            }
        } else {
            Err(CodegenError::UnsupportedNode(format!("{:?}", decl)))
        }
    }
    fn lower_params(
        &self,
        params: &[DeclParam],
    ) -> Result<(Vec<BasicMetadataTypeEnum<'ctx>>, Vec<String>), CodegenError> {
        let mut types = Vec::with_capacity(params.len());
        let mut idents = Vec::with_capacity(params.len());
        for param in params {
            match param {
                DeclParam::Typed(name, typ) => {
                    types.push(self.lower_type(typ)?.into());
                    idents.push(name.clone());
                }
                DeclParam::Untyped(name) => {
                    types.push(self.context.i32_type().into());
                    idents.push(name.clone());
                }
                DeclParam::Variadic(_, _)
                | DeclParam::OptionalTyped(_, _, _)
                | DeclParam::OptionalUntyped(_, _) => {
                    return Err(CodegenError::UnsupportedParam(format!("{:?}", param)));
                }
            }
        }
        Ok((types, idents))
    }

    fn lower_ret_type(
        &self,
        typ: &Option<Type>,
    ) -> Result<Option<BasicTypeEnum<'ctx>>, CodegenError> {
        match typ {
            Some(Type::UnitTyp) => Ok(None),
            Some(t) => Ok(Some(self.lower_type(t)?)),
            None => Ok(None),
        }
    }

    fn lower_type(&self, typ: &Type) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        match typ {
            Type::User(name) | Type::Builtin(name) => match name.as_str() {
                "i1" | "bool" => Ok(self.context.bool_type().into()),
                "i8" | "char" => Ok(self.context.i8_type().into()),
                "i32" | "int" => Ok(self.context.i32_type().into()),
                "i64" => Ok(self.context.i64_type().into()),
                "string" => Ok(self.context.ptr_type(AddressSpace::default()).into()),
                _ => Err(CodegenError::UnsupportedType(typ.clone())),
            },
            Type::UnitTyp => Err(CodegenError::UnsupportedType(typ.clone())),
            Type::Generic(_, _) | Type::TypeVar(_) | Type::ListTyp(_) => {
                Err(CodegenError::UnsupportedType(typ.clone()))
            }
        }
    }

    fn emit_statement(
        &mut self,
        stmt: &Statement,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
        func: FunctionValue<'ctx>,
    ) -> Result<bool, CodegenError> {
        match stmt {
            Statement::Return(ret) => {
                match ret {
                    ReturnStmt::WithExpr(expr) => {
                        if func.get_type().get_return_type().is_none() {
                            let _ = self.emit_expression(expr, env)?;
                            let _ = self.builder.build_return(None);
                        } else {
                            let val = self.emit_expression(expr, env)?;
                            let _ = self.builder.build_return(Some(&val));
                        }
                    }
                    ReturnStmt::Naked => {
                        if func.get_type().get_return_type().is_none() {
                            let _ = self.builder.build_return(None);
                        } else {
                            return Err(CodegenError::MissingReturn(
                                func.get_name().to_string_lossy().into_owned(),
                            ));
                        }
                    }
                }
                Ok(true)
            }
            Statement::Expression(expr) => {
                let _ = self.emit_expression(expr, env)?;
                Ok(false)
            }
            // Gracefully ignore statements we don't lower yet.
            _ => Ok(false),
        }
    }

    fn emit_expression(
        &mut self,
        expr: &Expression,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expression::RelationalExpr(rel) => self.emit_relational(rel, env),
            Expression::AssignmentExpr(_) => Ok(self.zero()),
            Expression::ListExpr(_) => Ok(self.zero()),
            Expression::MatchExpr(_) => Ok(self.zero()),
            Expression::StructExpr(_, _)
            | Expression::EnumExpr(_, _)
            | Expression::DeriveExpr(_) => Ok(self.zero()),
            Expression::CallExpr(call) => self.emit_call(call, env),
        }
    }

    fn emit_relational(
        &mut self,
        rel: &RelationalExpr,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match rel {
            RelationalExpr::RelationalVal(add) => self.emit_additive(add, env),
            RelationalExpr::Eql(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::EQ),
            RelationalExpr::Neq(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::NE),
            RelationalExpr::Lt(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::SLT),
            RelationalExpr::Gt(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::SGT),
            RelationalExpr::Leq(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::SLE),
            RelationalExpr::Geq(l, r) => self.emit_int_cmp(l, r, env, inkwell::IntPredicate::SGE),
        }
    }

    fn emit_int_cmp(
        &mut self,
        l: &Box<AdditiveExpr>,
        r: &Box<AdditiveExpr>,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
        pred: inkwell::IntPredicate,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let lhs = self.emit_additive(l, env)?.into_int_value();
        let rhs = self.emit_additive(r, env)?.into_int_value();
        let cmp = self.builder.build_int_compare(pred, lhs, rhs, "icmp")?;
        Ok(cmp.into())
    }

    fn emit_additive(
        &mut self,
        add: &AdditiveExpr,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match add {
            AdditiveExpr::Add(lhs, rhs) => {
                let l = self.emit_additive(lhs, env)?.into_int_value();
                let r = self.emit_multiplicative(rhs, env)?.into_int_value();
                let v = self.builder.build_int_add(l, r, "add")?;
                Ok(v.into())
            }
            AdditiveExpr::Sub(lhs, rhs) => {
                let l = self.emit_additive(lhs, env)?.into_int_value();
                let r = self.emit_multiplicative(rhs, env)?.into_int_value();
                let v = self.builder.build_int_sub(l, r, "sub")?;
                Ok(v.into())
            }
            AdditiveExpr::AdditiveVal(expr) => self.emit_multiplicative(expr, env),
        }
    }

    fn emit_multiplicative(
        &mut self,
        mul: &MultiplicativeExpr,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match mul {
            MultiplicativeExpr::Mul(lhs, rhs) => {
                let l = self.emit_multiplicative(lhs, env)?.into_int_value();
                let r = self.emit_unary(rhs, env)?.into_int_value();
                let v = self.builder.build_int_mul(l, r, "mul")?;
                Ok(v.into())
            }
            MultiplicativeExpr::Div(lhs, rhs) => {
                let l = self.emit_multiplicative(lhs, env)?.into_int_value();
                let r = self.emit_unary(rhs, env)?.into_int_value();
                let v = self.builder.build_int_signed_div(l, r, "div")?;
                Ok(v.into())
            }
            MultiplicativeExpr::Mod(lhs, rhs) => {
                let l = self.emit_multiplicative(lhs, env)?.into_int_value();
                let r = self.emit_unary(rhs, env)?.into_int_value();
                let v = self.builder.build_int_signed_rem(l, r, "mod")?;
                Ok(v.into())
            }
            MultiplicativeExpr::Pow(_, _) => Ok(self.zero()),
            MultiplicativeExpr::MultiplicativeVal(expr) => self.emit_unary(expr, env),
        }
    }

    fn emit_unary(
        &mut self,
        unary: &UnaryExpr,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match unary {
            UnaryExpr::Neg(expr) => {
                let v = self.emit_unary(expr, env)?.into_int_value();
                let val = self.builder.build_int_neg(v, "neg")?;
                Ok(val.into())
            }
            UnaryExpr::Not(expr) => {
                let v = self.emit_unary(expr, env)?.into_int_value();
                let val = self.builder.build_not(v, "not")?;
                Ok(val.into())
            }
            UnaryExpr::UnaryMember(_, _) => Ok(self.zero()),
            UnaryExpr::UnaryCall(call) => self.emit_call(call, env),
            UnaryExpr::UnaryVal(atom) => self.emit_atom(atom, env),
        }
    }

    fn emit_atom(
        &mut self,
        atom: &Atom,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match atom {
            Atom::Int(n) => Ok(self.context.i32_type().const_int(*n as u64, true).into()),
            Atom::Bool(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),
            Atom::Char(c) => Ok(self.context.i8_type().const_int(*c as u64, false).into()),
            Atom::String(s) => {
                let global = self.builder.build_global_string_ptr(s, "str")?;
                Ok(global.as_pointer_value().into())
            }
            Atom::Ident(name) => env
                .get(name)
                .copied()
                .ok_or_else(|| CodegenError::UnknownIdent(name.clone())),
            Atom::ImplicitMember(name) => env
                .get(name)
                .copied()
                .ok_or_else(|| CodegenError::UnknownIdent(name.clone())),
            Atom::Grouping(expr) => self.emit_expression(expr, env),
            Atom::UnitVal => Ok(self.zero()),
        }
    }

    fn emit_call(
        &mut self,
        call: &CallExpr,
        env: &mut HashMap<String, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let CallExpr::DeclCall(expr, params) = call;
        let callee_name = match self.extract_ident(expr) {
            Some(name) => name,
            None => return Ok(self.zero()),
        };
        let fn_val = self
            .functions
            .get(callee_name)
            .copied()
            .or_else(|| self.module.get_function(callee_name))
            .unwrap_or_else(|| {
                let i8_ptr = self.context.ptr_type(AddressSpace::default());
                let fn_type = self.context.i32_type().fn_type(&[i8_ptr.into()], true);
                let f = self.module.add_function(callee_name, fn_type, None);
                self.functions.insert(callee_name.to_string(), f);
                f
            });

        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(params.len());
        for p in params {
            match p {
                CallParam::Named(_, expr) | CallParam::Positional(expr) => {
                    let arg = self.emit_expression(expr, env)?;
                    args.push(arg.into());
                }
            }
        }

        let callsite = self.builder.build_call(fn_val, &args, "call")?;

        match callsite.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(ret) => Ok(ret),
            _ => Ok(self.zero()),
        }
    }

    fn extract_ident<'a>(&self, expr: &'a Expression) -> Option<&'a str> {
        if let Expression::RelationalExpr(RelationalExpr::RelationalVal(add)) = expr {
            if let AdditiveExpr::AdditiveVal(mul) = add.as_ref() {
                if let MultiplicativeExpr::MultiplicativeVal(unary) = mul.as_ref() {
                    return Self::extract_ident_from_unary(unary);
                }
            }
        }
        None
    }

    fn extract_ident_from_unary(unary: &UnaryExpr) -> Option<&str> {
        match unary {
            UnaryExpr::UnaryVal(Atom::Ident(id)) => Some(id.as_str()),
            UnaryExpr::UnaryMember(inner, ident) => match inner.as_ref() {
                UnaryExpr::UnaryVal(Atom::Ident(_)) => Some(ident.as_str()),
                _ => None,
            },
            _ => None,
        }
    }

    fn zero(&self) -> BasicValueEnum<'ctx> {
        self.context.i32_type().const_int(0, false).into()
    }
}
