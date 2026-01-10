use crate::parser::nodes::{
    AdditiveExpr, AssignmentExpr, Atom, CallExpr, CallParam, DeclBody, DeclParam, DeclStmt,
    ElseStmt, Expression, IfStmt, MatchExpr, MultiplicativeExpr, Node, RelationalExpr,
    ReturnStmt, Statement, Type, UnaryExpr,
};
use anyhow::{Result, anyhow};
use std::collections::HashMap;

use super::common::Context;

/// Emits AMD64 code from the AST.
pub struct Emitter<'ctx> {
    ctx: &'ctx Context,
    header: Vec<String>,
    text: Vec<String>,
    rodata: Vec<String>,
    data: Vec<String>,
    bss: Vec<String>,
    label_counter: usize,
    string_counter: usize,
    locals: HashMap<String, i32>, // var_name -> stack_offset
    stack_offset: i32,
    in_function: bool, // Track if we're currently inside a function
}

impl<'ctx> Emitter<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Self {
        Self {
            ctx,
            header: Vec::new(),
            text: Vec::new(),
            rodata: Vec::new(),
            data: Vec::new(),
            bss: Vec::new(),
            label_counter: 0,
            string_counter: 0,
            locals: HashMap::new(),
            stack_offset: 0,
            in_function: false,
        }
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn fresh_string_label(&mut self) -> String {
        let label = format!(".STR_{}", self.string_counter);
        self.string_counter += 1;
        label
    }

    fn emit_text(&mut self, line: String) {
        self.text.push(line);
    }

    fn emit_rodata(&mut self, line: String) {
        self.rodata.push(line);
    }

    pub fn emit(&self, ast: &[Node]) -> Result<String> {
        let mut emitter = Emitter::new(self.ctx);

        emitter.preamble(ast);

        // Process all top-level nodes
        for node in ast {
            emitter.emit_node(node)?;
        }

        emitter.entry_stub();

        Ok(emitter.render())
    }

    fn preamble(&mut self, ast: &[Node]) {
        self.header
            .push(format!("# module: {}", self.ctx.module_name));
        self.header.push(format!("# nodes: {}", ast.len()));
        self.header
            .push(format!("# mangler: {}", self.ctx.mangler().module_tag()));
        self.header.push(String::from("# amd64 backend"));
        self.header.push(String::new());
        self.header.push(String::from(".global _start"));
    }

    fn entry_stub(&mut self) {
        self.text.push(String::from("_start:"));
        self.text.push(String::from("    # Call main function"));
        self.text.push(String::from("    call    main"));
        self.text
            .push(String::from("    # Exit with return value from main"));
        self.text.push(String::from(
            "    mov     %rax, %rdi    # exit code from main",
        ));
        self.text
            .push(String::from("    mov     $60, %rax     # sys_exit"));
        self.text.push(String::from("    syscall"));
    }

    // ===== AST Node Emission =====

    fn emit_node(&mut self, node: &Node) -> Result<()> {
        match node {
            Node::Statement(stmt) => self.emit_statement(stmt),
            Node::Expression(expr) => {
                self.emit_expression(expr)?;
                Ok(())
            }
            Node::Error(msg, _) => Err(anyhow!("AST error node: {}", msg)),
        }
    }

    fn emit_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Decl(decl) => self.emit_decl(decl),
            Statement::Return(ret) => self.emit_return(ret),
            Statement::If(if_stmt) => self.emit_if(if_stmt),
            Statement::Expression(expr) => {
                self.emit_expression(expr)?;
                Ok(())
            }
            Statement::Open(_) => Ok(()), // Ignore imports for now
        }
    }

    // ===== Function Declarations =====

    fn emit_decl(&mut self, decl: &DeclStmt) -> Result<()> {
        match decl {
            DeclStmt::Decl {
                name,
                params,
                explicit_ret,
                body,
                ..
            } => {
                // If we're already in a function and this has no params, it's a local variable
                if self.in_function && params.is_empty() {
                    self.emit_local_var(name, body)
                } else {
                    // It's a function declaration
                    self.emit_function(name, params, explicit_ret, body)
                }
            }
            DeclStmt::ImportDecl { .. } => Ok(()), // Ignore for now
            _ => Ok(()),                           // Other decl types not yet implemented
        }
    }

    fn emit_local_var(&mut self, name: &str, body: &DeclBody) -> Result<()> {
        // Allocate space for the variable
        self.stack_offset -= 8;
        self.locals.insert(name.to_string(), self.stack_offset);

        // Emit initialization expression if present
        if let Some(init_expr) = &body.1 {
            self.emit_expression(init_expr)?;
            self.emit_text(format!("    mov     %rax, {}(%rbp)", self.stack_offset));
        }

        Ok(())
    }

    fn emit_function(
        &mut self,
        name: &str,
        params: &[DeclParam],
        ret_type: &Option<Type>,
        body: &DeclBody,
    ) -> Result<()> {
        // Generate mangled symbol
        let symbol = self
            .ctx
            .mangler()
            .mangle_function_symbol(name, params, ret_type);

        // Make main globally visible
        if name == "main" {
            self.emit_text(String::from(".global main"));
        }

        // Emit both mangled and unmangled labels
        self.emit_text(format!("{}:", symbol));
        self.emit_text(format!("{}:", name));

        // Prologue
        self.emit_text(String::from("    push    %rbp"));
        self.emit_text(String::from("    mov     %rsp, %rbp"));

        // Reset local variable tracking and mark that we're in a function
        self.locals.clear();
        self.stack_offset = 0;
        self.in_function = true;

        // Map parameters to registers (SysV ABI: rdi, rsi, rdx, rcx, r8, r9)
        let param_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        for (i, param) in params.iter().enumerate() {
            if i < 6 {
                let param_name = match param {
                    DeclParam::Typed(name, _) | DeclParam::Untyped(name) => name,
                    DeclParam::OptionalTyped(name, _, _) | DeclParam::OptionalUntyped(name, _) => {
                        name
                    }
                    DeclParam::Variadic(name, _) => name,
                };
                // Allocate stack space for parameter
                self.stack_offset -= 8;
                self.locals.insert(param_name.clone(), self.stack_offset);
                // Store parameter from register to stack
                self.emit_text(format!(
                    "    mov     {}, {}(%rbp)",
                    param_regs[i], self.stack_offset
                ));
            }
        }

        // Emit function body
        for stmt in &body.0 {
            self.emit_statement(stmt)?;
        }

        // Emit return expression if present
        if let Some(expr) = &body.1 {
            self.emit_expression(expr)?;
            // Result is in %rax
        }

        // Epilogue
        self.emit_text(String::from("    mov     %rbp, %rsp"));
        self.emit_text(String::from("    pop     %rbp"));
        self.emit_text(String::from("    ret"));
        self.emit_text(String::new());

        // Clear function context
        self.in_function = false;

        Ok(())
    }

    // ===== Control Flow =====

    fn emit_if(&mut self, if_stmt: &IfStmt) -> Result<()> {
        let else_label = self.fresh_label("else");
        let end_label = self.fresh_label("endif");

        // Emit condition
        self.emit_expression(&if_stmt.cond)?;

        // Test result (in %rax)
        self.emit_text(String::from("    test    %rax, %rax"));
        self.emit_text(format!("    jz      {}", else_label));

        // Emit then body
        for node in &if_stmt.body {
            self.emit_node(node)?;
        }
        self.emit_text(format!("    jmp     {}", end_label));

        // Emit else/elif
        self.emit_text(format!("{}:", else_label));
        self.emit_else(&if_stmt.elif)?;

        self.emit_text(format!("{}:", end_label));
        Ok(())
    }

    fn emit_else(&mut self, else_stmt: &ElseStmt) -> Result<()> {
        match else_stmt {
            ElseStmt::ElseIf(cond, body, next_else) => {
                let else_label = self.fresh_label("else");
                let end_label = self.fresh_label("endif");

                self.emit_expression(cond)?;
                self.emit_text(String::from("    test    %rax, %rax"));
                self.emit_text(format!("    jz      {}", else_label));

                for node in body {
                    self.emit_node(node)?;
                }
                self.emit_text(format!("    jmp     {}", end_label));

                self.emit_text(format!("{}:", else_label));
                self.emit_else(next_else)?;
                self.emit_text(format!("{}:", end_label));
            }
            ElseStmt::Else(body) => {
                for node in body {
                    self.emit_node(node)?;
                }
            }
            ElseStmt::Nope => {}
        }
        Ok(())
    }



    fn emit_return(&mut self, ret_stmt: &ReturnStmt) -> Result<()> {
        match ret_stmt {
            ReturnStmt::WithExpr(expr) => {
                self.emit_expression(expr)?;
                // Value is in %rax
            }
            ReturnStmt::Naked => {
                // No return value
                self.emit_text(String::from("    xor     %rax, %rax"));
            }
        }

        // Early return - emit epilogue
        self.emit_text(String::from("    mov     %rbp, %rsp"));
        self.emit_text(String::from("    pop     %rbp"));
        self.emit_text(String::from("    ret"));

        Ok(())
    }

    // ===== Expression Emission =====

    fn emit_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::CallExpr(call) => self.emit_call(call),
            Expression::RelationalExpr(rel) => self.emit_relational(rel),
            Expression::AssignmentExpr(assign) => self.emit_assignment(assign),
            Expression::ListExpr(_) => Ok(()), // Not yet implemented
            Expression::MatchExpr(match_expr) => self.emit_match(match_expr),
            Expression::StructExpr(_, _) => Ok(()), // Not yet implemented
            Expression::EnumExpr(_, _) => Ok(()),   // Not yet implemented
            Expression::DeriveExpr(_) => Ok(()),    // Not yet implemented
        }
    }

    fn emit_relational(&mut self, rel: &RelationalExpr) -> Result<()> {
        match rel {
            RelationalExpr::RelationalVal(val) => self.emit_additive(val),
            RelationalExpr::Eql(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    sete    %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            RelationalExpr::Neq(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    setne   %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            RelationalExpr::Lt(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    setl    %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            RelationalExpr::Gt(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    setg    %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            RelationalExpr::Leq(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    setle   %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            RelationalExpr::Geq(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_additive(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    cmp     %rax, %rcx"));
                self.emit_text(String::from("    setge   %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
        }
    }

    fn emit_additive(&mut self, add: &AdditiveExpr) -> Result<()> {
        match add {
            AdditiveExpr::AdditiveVal(val) => self.emit_multiplicative(val),
            AdditiveExpr::Add(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_multiplicative(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    add     %rcx, %rax"));
                Ok(())
            }
            AdditiveExpr::Sub(left, right) => {
                self.emit_additive(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_multiplicative(right)?;
                self.emit_text(String::from("    mov     %rax, %rcx"));
                self.emit_text(String::from("    pop     %rax"));
                self.emit_text(String::from("    sub     %rcx, %rax"));
                Ok(())
            }
        }
    }

    fn emit_multiplicative(&mut self, mul: &MultiplicativeExpr) -> Result<()> {
        match mul {
            MultiplicativeExpr::MultiplicativeVal(val) => self.emit_unary(val),
            MultiplicativeExpr::Mul(left, right) => {
                self.emit_multiplicative(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_unary(right)?;
                self.emit_text(String::from("    pop     %rcx"));
                self.emit_text(String::from("    imul    %rcx, %rax"));
                Ok(())
            }
            MultiplicativeExpr::Div(left, right) => {
                self.emit_multiplicative(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_unary(right)?;
                self.emit_text(String::from("    mov     %rax, %rcx"));
                self.emit_text(String::from("    pop     %rax"));
                self.emit_text(String::from("    cqo"));
                self.emit_text(String::from("    idiv    %rcx"));
                Ok(())
            }
            MultiplicativeExpr::Mod(left, right) => {
                self.emit_multiplicative(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_unary(right)?;
                self.emit_text(String::from("    mov     %rax, %rcx"));
                self.emit_text(String::from("    pop     %rax"));
                self.emit_text(String::from("    cqo"));
                self.emit_text(String::from("    idiv    %rcx"));
                self.emit_text(String::from("    mov     %rdx, %rax"));
                Ok(())
            }
            MultiplicativeExpr::Pow(left, right) => {
                // Simplified power - assume small integer exponents
                self.emit_multiplicative(left)?;
                self.emit_text(String::from("    push    %rax"));
                self.emit_unary(right)?;
                self.emit_text(String::from("    mov     %rax, %rcx")); // exponent in rcx
                self.emit_text(String::from("    pop     %rax")); // base in rax
                self.emit_text(String::from("    mov     $1, %rdx")); // result in rdx

                let loop_label = self.fresh_label("pow");
                let end_label = self.fresh_label("endpow");

                self.emit_text(format!("{}:", loop_label));
                self.emit_text(String::from("    test    %rcx, %rcx"));
                self.emit_text(format!("    jz      {}", end_label));
                self.emit_text(String::from("    imul    %rax, %rdx"));
                self.emit_text(String::from("    dec     %rcx"));
                self.emit_text(format!("    jmp     {}", loop_label));
                self.emit_text(format!("{}:", end_label));
                self.emit_text(String::from("    mov     %rdx, %rax"));
                Ok(())
            }
        }
    }

    fn emit_unary(&mut self, unary: &UnaryExpr) -> Result<()> {
        match unary {
            UnaryExpr::UnaryVal(atom) => self.emit_atom(atom),
            UnaryExpr::Neg(inner) => {
                self.emit_unary(inner)?;
                self.emit_text(String::from("    neg     %rax"));
                Ok(())
            }
            UnaryExpr::Not(inner) => {
                self.emit_unary(inner)?;
                self.emit_text(String::from("    test    %rax, %rax"));
                self.emit_text(String::from("    setz    %al"));
                self.emit_text(String::from("    movzb   %al, %rax"));
                Ok(())
            }
            UnaryExpr::UnaryCall(call) => self.emit_call(call),
            _ => Ok(()), // Member access not yet implemented
        }
    }

    fn emit_atom(&mut self, atom: &Atom) -> Result<()> {
        match atom {
            Atom::Int(n) => {
                self.emit_text(format!("    mov     ${}, %rax", n));
                Ok(())
            }
            Atom::Bool(b) => {
                let val = if *b { 1 } else { 0 };
                self.emit_text(format!("    mov     ${}, %rax", val));
                Ok(())
            }
            Atom::String(s) => {
                let label = self.fresh_string_label();
                // Emit string to rodata
                self.emit_rodata(format!("{}:", label));
                self.emit_rodata(format!("    .asciz  \"{}\"", s.escape_default()));
                // Load address into rax
                self.emit_text(format!("    lea     {}(%rip), %rax", label));
                Ok(())
            }
            Atom::Ident(name) => {
                // Load local variable
                if let Some(&offset) = self.locals.get(name) {
                    self.emit_text(format!("    mov     {}(%rbp), %rax", offset));
                    Ok(())
                } else {
                    Err(anyhow!("Undefined variable: {}", name))
                }
            }
            Atom::Grouping(expr) => self.emit_expression(expr),
            Atom::UnitVal => {
                self.emit_text(String::from("    xor     %rax, %rax"));
                Ok(())
            }
            _ => Ok(()), // Other atom types not yet implemented
        }
    }

    fn emit_assignment(&mut self, assign: &AssignmentExpr) -> Result<()> {
        match assign {
            AssignmentExpr::AddAssign(name, expr) => {
                self.emit_additive(expr)?;
                if let Some(&offset) = self.locals.get(name) {
                    self.emit_text(format!("    add     %rax, {}(%rbp)", offset));
                    self.emit_text(format!("    mov     {}(%rbp), %rax", offset));
                } else {
                    return Err(anyhow!("Undefined variable: {}", name));
                }
                Ok(())
            }
            AssignmentExpr::SubAssign(name, expr) => {
                self.emit_additive(expr)?;
                if let Some(&offset) = self.locals.get(name) {
                    self.emit_text(format!("    neg     %rax"));
                    self.emit_text(format!("    add     %rax, {}(%rbp)", offset));
                    self.emit_text(format!("    mov     {}(%rbp), %rax", offset));
                } else {
                    return Err(anyhow!("Undefined variable: {}", name));
                }
                Ok(())
            }
            AssignmentExpr::MulAssign(name, expr) => {
                self.emit_additive(expr)?;
                if let Some(&offset) = self.locals.get(name) {
                    self.emit_text(format!("    imul    {}(%rbp), %rax", offset));
                    self.emit_text(format!("    mov     %rax, {}(%rbp)", offset));
                } else {
                    return Err(anyhow!("Undefined variable: {}", name));
                }
                Ok(())
            }
            AssignmentExpr::DivAssign(name, expr) => {
                if let Some(&offset) = self.locals.get(name) {
                    self.emit_text(format!("    mov     {}(%rbp), %rax", offset));
                    self.emit_text(String::from("    push    %rax"));
                    self.emit_additive(expr)?;
                    self.emit_text(String::from("    mov     %rax, %rcx"));
                    self.emit_text(String::from("    pop     %rax"));
                    self.emit_text(String::from("    cqo"));
                    self.emit_text(String::from("    idiv    %rcx"));
                    self.emit_text(format!("    mov     %rax, {}(%rbp)", offset));
                } else {
                    return Err(anyhow!("Undefined variable: {}", name));
                }
                Ok(())
            }
        }
    }

    fn emit_call(&mut self, call: &CallExpr) -> Result<()> {
        match call {
            CallExpr::DeclCall(func_expr, args) => {
                // Get function name (simplified - assume it's an identifier)
                let func_name =
                    if let Expression::RelationalExpr(RelationalExpr::RelationalVal(add)) =
                        func_expr.as_ref()
                    {
                        if let AdditiveExpr::AdditiveVal(mul) = add.as_ref() {
                            if let MultiplicativeExpr::MultiplicativeVal(unary) = mul.as_ref() {
                                if let UnaryExpr::UnaryVal(Atom::Ident(name)) = unary.as_ref() {
                                    name.clone()
                                } else {
                                    return Err(anyhow!(
                                        "Complex function expressions not yet supported"
                                    ));
                                }
                            } else {
                                return Err(anyhow!(
                                    "Complex function expressions not yet supported"
                                ));
                            }
                        } else {
                            return Err(anyhow!("Complex function expressions not yet supported"));
                        }
                    } else {
                        return Err(anyhow!("Complex function expressions not yet supported"));
                    };

                // Evaluate arguments and place in registers (SysV ABI)
                let param_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

                // Handle positional arguments
                let mut positional_args = Vec::new();
                for arg in args {
                    match arg {
                        CallParam::Positional(expr) => positional_args.push(expr),
                        CallParam::Named(_, _) => {
                            return Err(anyhow!("Named parameters not yet supported in codegen"));
                        }
                    }
                }

                // Emit arguments in reverse order and save to stack
                for (i, arg) in positional_args.iter().enumerate().rev() {
                    if i < 6 {
                        self.emit_expression(arg)?;
                        self.emit_text(String::from("    push    %rax"));
                    }
                }

                // Pop arguments into registers in forward order
                for i in 0..positional_args.len().min(6) {
                    self.emit_text(format!("    pop     {}", param_regs[i]));
                }

                // Call the function
                self.emit_text(format!("    call    {}", func_name));

                // Result is in %rax
                Ok(())
            }
        }
    }

    fn emit_match(&mut self, _match_expr: &MatchExpr) -> Result<()> {
        // Match expressions not yet implemented
        Ok(())
    }

    fn render(&self) -> String {
        let mut out = Vec::new();
        out.extend(self.header.iter().cloned());

        out.push(String::new());
        out.push(String::from(".text"));
        if self.text.is_empty() {
            out.push(String::from("# <no text>"));
        } else {
            out.extend(self.text.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".rodata"));
        if self.rodata.is_empty() {
            out.push(String::from("# <no rodata>"));
        } else {
            out.extend(self.rodata.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".data"));
        if self.data.is_empty() {
            out.push(String::from("# <no data>"));
        } else {
            out.extend(self.data.iter().cloned());
        }

        out.push(String::new());
        out.push(String::from(".bss"));
        if self.bss.is_empty() {
            out.push(String::from("# <no bss>"));
        } else {
            out.extend(self.bss.iter().cloned());
        }

        out.join("\n")
    }
}

/// AMD64 backend entrypoint.
pub fn gen_target(module_name: &str, ast: &[Node]) -> Result<String> {
    let ctx = Context::new(module_name);
    let emitter = Emitter::new(&ctx);
    emitter.emit(ast)
}
