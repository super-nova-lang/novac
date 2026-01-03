#![allow(dead_code)]

use std::collections::HashMap;

use parser::nodes::*;

#[derive(Debug, Clone)]
pub enum AnalysisError {
    UndefinedVariable(String, Vec<String>),
    TypeMismatch(Type, Type),
    DuplicateDeclaration(String, (usize, usize)),
    InvalidOperation(String),
    MissingReturnType(String),
}

#[derive(Debug, Clone)]
pub enum AnalysisWarning {
    UnusedVariable(String),
    ShadowedVariable(String),
}

#[derive(Debug, Clone)]
pub enum AnalysisResult {
    Error(AnalysisError),
    Warning(AnalysisWarning),
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub declared_typ: Option<Type>,
    pub inferred_typ: Option<Type>,
    pub used: bool,
    pub scope_level: usize,
    pub location: (usize, usize),
}

type SymbolTable = HashMap<String, Vec<SymbolInfo>>;

#[derive(Debug, Clone)]
pub struct Context {
    symbols: SymbolTable,
    scope_level: usize,
    errors: Vec<AnalysisResult>,
    warnings: Vec<AnalysisResult>,
}

pub fn analyze(ast_nodes: Vec<Node>) -> (Vec<AnalysisResult>, Vec<AnalysisResult>) {
    let mut ctx = create_context();
    for node in ast_nodes {
        analyze_ast(&mut ctx, &node);
    }
    (ctx.errors, ctx.warnings)
}

fn create_context() -> Context {
    Context {
        symbols: HashMap::with_capacity(100),
        scope_level: 0,
        errors: Vec::new(),
        warnings: Vec::new(),
    }
}

fn enter_scope(ctx: &mut Context) {
    ctx.scope_level += 1;
}

fn exit_scope(ctx: &mut Context) {
    // Warn on unused bindings in the current scope (locals only).
    for (name, infos) in ctx.symbols.iter() {
        for info in infos.iter() {
            if info.scope_level == ctx.scope_level && !info.used && ctx.scope_level > 0 {
                ctx.warnings
                    .push(AnalysisResult::Warning(AnalysisWarning::UnusedVariable(
                        name.clone(),
                    )));
            }
        }
    }

    // Drop bindings from this scope; remove empty stacks.
    ctx.symbols.retain(|_, infos| {
        infos.retain(|info| info.scope_level != ctx.scope_level);
        !infos.is_empty()
    });

    ctx.scope_level = ctx.scope_level.saturating_sub(1);
}

fn add_symbol(
    ctx: &mut Context,
    name: impl Into<String>,
    typ: Option<Type>,
    location: (usize, usize),
) {
    let name = name.into();
    let info = SymbolInfo {
        name: name.clone(),
        declared_typ: typ,
        inferred_typ: None,
        used: false,
        scope_level: ctx.scope_level,
        location,
    };

    match ctx.symbols.get_mut(&name) {
        Some(stack) => {
            ctx.warnings
                .push(AnalysisResult::Warning(AnalysisWarning::ShadowedVariable(
                    name.clone(),
                )));
            stack.insert(0, info);
        }
        None => {
            ctx.symbols.insert(name, vec![info]);
        }
    }
}

fn lookup_symbol<'a>(ctx: &'a Context, name: &str) -> Option<&'a SymbolInfo> {
    ctx.symbols.get(name).and_then(|v| v.first())
}

fn lookup_symbol_mut<'a>(ctx: &'a mut Context, name: &str) -> Option<&'a mut SymbolInfo> {
    ctx.symbols.get_mut(name).and_then(|v| v.first_mut())
}

fn set_inferred_type(ctx: &mut Context, name: &str, typ: Type) {
    if let Some(info) = lookup_symbol_mut(ctx, name) {
        info.inferred_typ = Some(typ);
    }
}

fn get_symbol_type(info: &SymbolInfo) -> Type {
    if let Some(t) = &info.declared_typ {
        t.clone()
    } else if let Some(t) = &info.inferred_typ {
        t.clone()
    } else {
        Type::UnitTyp
    }
}

fn types_equal(a: &Type, b: &Type) -> bool {
    use Type::*;
    match (a, b) {
        (User(x), User(y)) => x == y,
        (Generic(x, xs), Generic(y, ys)) => x == y && iter_types_equal(xs, ys),
        (TypeVar(x), TypeVar(y)) => x == y,
        (Builtin(x), Builtin(y)) => x == y,
        (UnitTyp, UnitTyp) => true,
        (ListTyp(x), ListTyp(y)) => types_equal(x, y),
        _ => false,
    }
}

fn iter_types_equal(xs: &[Type], ys: &[Type]) -> bool {
    xs.len() == ys.len() && xs.iter().zip(ys).all(|(a, b)| types_equal(a, b))
}

fn take_first_n<T: Clone>(n: usize, iter: impl Iterator<Item = T>) -> Vec<T> {
    let mut out = Vec::new();
    for item in iter {
        if out.len() >= n {
            break;
        }
        out.push(item);
    }
    out
}

fn suggest_names(ctx: &Context, name: &str) -> Vec<String> {
    if name.is_empty() {
        return Vec::new();
    }
    let first = name.chars().next().unwrap();
    let candidates = ctx
        .symbols
        .keys()
        .filter(|cand| !cand.is_empty() && *cand != name && cand.chars().next() == Some(first))
        .cloned()
        .collect::<Vec<_>>();
    let mut sorted = candidates;
    sorted.sort();
    take_first_n(3, sorted.into_iter())
}

fn mark_used(ctx: &mut Context, name: &str) {
    if let Some(info) = lookup_symbol_mut(ctx, name) {
        info.used = true;
    }
}

fn infer_expression_type(ctx: &mut Context, expr: &Expression) -> Type {
    match expr {
        Expression::CallExpr(call) => infer_call_type(ctx, call),
        Expression::RelationalExpr(rel) => infer_relational_type(ctx, rel),
        Expression::AssignmentExpr(_) => Type::UnitTyp,
        Expression::ListExpr(elems) => {
            for e in elems {
                let _ = infer_expression_type(ctx, e);
            }
            let elem_typ = elems
                .first()
                .map(|e| infer_expression_type(ctx, e))
                .unwrap_or(Type::UnitTyp);
            Type::ListTyp(Box::new(elem_typ))
        }
        Expression::MatchExpr(m) => infer_match_type(ctx, m),
        Expression::StructExpr(..) => Type::User("struct".to_string()),
        Expression::EnumExpr(..) => Type::User("enum".to_string()),
        Expression::DeriveExpr(_) => Type::UnitTyp,
    }
}

fn infer_call_type(ctx: &mut Context, call: &CallExpr) -> Type {
    match call {
        CallExpr::DeclCall(expr, params) => {
            for p in params {
                infer_call_param_type(ctx, p);
            }
            infer_expression_type(ctx, expr)
        }
    }
}

fn infer_call_param_type(ctx: &mut Context, param: &CallParam) {
    match param {
        CallParam::Named(_, expr) | CallParam::Positional(expr) => {
            let _ = infer_expression_type(ctx, expr);
        }
    }
}

fn infer_relational_type(ctx: &mut Context, rel: &RelationalExpr) -> Type {
    match rel {
        RelationalExpr::Eql(l, r)
        | RelationalExpr::Neq(l, r)
        | RelationalExpr::Lt(l, r)
        | RelationalExpr::Gt(l, r)
        | RelationalExpr::Leq(l, r)
        | RelationalExpr::Geq(l, r) => {
            let _ = infer_additive_type(ctx, l);
            let _ = infer_additive_type(ctx, r);
            Type::User("bool".to_string())
        }
        RelationalExpr::RelationalVal(expr) => infer_additive_type(ctx, expr),
    }
}

fn infer_additive_type(ctx: &mut Context, add: &AdditiveExpr) -> Type {
    match add {
        AdditiveExpr::Add(left, right) | AdditiveExpr::Sub(left, right) => {
            let _ = infer_additive_type(ctx, left);
            let _ = infer_multiplicative_type(ctx, right);
            Type::User("i32".to_string())
        }
        AdditiveExpr::AdditiveVal(expr) => infer_multiplicative_type(ctx, expr),
    }
}

fn infer_multiplicative_type(ctx: &mut Context, mul: &MultiplicativeExpr) -> Type {
    match mul {
        MultiplicativeExpr::Mul(left, right)
        | MultiplicativeExpr::Div(left, right)
        | MultiplicativeExpr::Mod(left, right)
        | MultiplicativeExpr::Pow(left, right) => {
            let _ = infer_multiplicative_type(ctx, left);
            let _ = infer_unary_type(ctx, right);
            Type::User("i32".to_string())
        }
        MultiplicativeExpr::MultiplicativeVal(expr) => infer_unary_type(ctx, expr),
    }
}

fn infer_unary_type(ctx: &mut Context, unary: &UnaryExpr) -> Type {
    match unary {
        UnaryExpr::Neg(expr) | UnaryExpr::Not(expr) => {
            let _ = infer_unary_type(ctx, expr);
            Type::User("i32".to_string())
        }
        UnaryExpr::UnaryMember(expr, _) => {
            let _ = infer_unary_type(ctx, expr);
            Type::UnitTyp
        }
        UnaryExpr::UnaryCall(call) => infer_call_type(ctx, call),
        UnaryExpr::UnaryVal(atom) => infer_atom_type(ctx, atom),
    }
}

fn infer_atom_type(ctx: &mut Context, atom: &Atom) -> Type {
    match atom {
        Atom::String(_) => Type::User("string".to_string()),
        Atom::Bool(_) => Type::User("bool".to_string()),
        Atom::Char(_) => Type::User("char".to_string()),
        Atom::Int(_) => Type::User("i32".to_string()),
        Atom::Ident(name) if name == "_" => Type::User("i32".to_string()),
        Atom::Ident(name) => {
            if let Some(info) = lookup_symbol_mut(ctx, name) {
                info.used = true;
                get_symbol_type(info)
            } else {
                let suggestions = suggest_names(ctx, name);
                ctx.errors
                    .push(AnalysisResult::Error(AnalysisError::UndefinedVariable(
                        name.clone(),
                        suggestions,
                    )));
                Type::UnitTyp
            }
        }
        Atom::ImplicitMember(name) => {
            if let Some(info) = lookup_symbol_mut(ctx, name) {
                info.used = true;
                get_symbol_type(info)
            } else {
                let suggestions = suggest_names(ctx, name);
                ctx.errors
                    .push(AnalysisResult::Error(AnalysisError::UndefinedVariable(
                        name.clone(),
                        suggestions,
                    )));
                Type::UnitTyp
            }
        }
        Atom::Grouping(expr) => infer_expression_type(ctx, expr),
        Atom::UnitVal => Type::UnitTyp,
    }
}

fn infer_match_type(_ctx: &mut Context, _expr: &(Box<Expression>, Vec<MatchArm>)) -> Type {
    // TODO: Infer from match arms.
    Type::UnitTyp
}

fn analyze_expression(ctx: &mut Context, expr: &Expression) {
    let _ = infer_expression_type(ctx, expr);
    match expr {
        Expression::ListExpr(elems) => elems.iter().for_each(|e| analyze_expression(ctx, e)),
        Expression::CallExpr(call) => analyze_call(ctx, call),
        Expression::RelationalExpr(rel) => analyze_relational(ctx, rel),
        Expression::AssignmentExpr(assign) => analyze_assignment(ctx, assign),
        Expression::MatchExpr(m) => analyze_match(ctx, m),
        Expression::StructExpr(fields, with_block) => analyze_struct(ctx, fields, with_block),
        Expression::EnumExpr(vars, with_block) => analyze_enum(ctx, vars, with_block),
        Expression::DeriveExpr(body) => analyze_derive(ctx, body),
    }
}

fn analyze_call(ctx: &mut Context, call: &CallExpr) {
    let CallExpr::DeclCall(expr, params) = call;
    analyze_expression(ctx, expr);
    for p in params {
        analyze_call_param(ctx, p);
    }
}

fn analyze_call_param(ctx: &mut Context, param: &CallParam) {
    match param {
        CallParam::Named(_, expr) | CallParam::Positional(expr) => analyze_expression(ctx, expr),
    }
}

fn analyze_relational(ctx: &mut Context, rel: &RelationalExpr) {
    match rel {
        RelationalExpr::Eql(l, r)
        | RelationalExpr::Neq(l, r)
        | RelationalExpr::Lt(l, r)
        | RelationalExpr::Gt(l, r)
        | RelationalExpr::Leq(l, r)
        | RelationalExpr::Geq(l, r) => {
            analyze_additive(ctx, l);
            analyze_additive(ctx, r);
        }
        RelationalExpr::RelationalVal(expr) => analyze_additive(ctx, expr),
    }
}

fn analyze_assignment(ctx: &mut Context, assign: &AssignmentExpr) {
    match assign {
        AssignmentExpr::AddAssign(_, expr)
        | AssignmentExpr::SubAssign(_, expr)
        | AssignmentExpr::MulAssign(_, expr)
        | AssignmentExpr::DivAssign(_, expr) => analyze_additive(ctx, expr),
    }
}

fn analyze_additive(ctx: &mut Context, add: &AdditiveExpr) {
    match add {
        AdditiveExpr::Add(l, r) | AdditiveExpr::Sub(l, r) => {
            analyze_additive(ctx, l);
            analyze_multiplicative(ctx, r);
        }
        AdditiveExpr::AdditiveVal(expr) => analyze_multiplicative(ctx, expr),
    }
}

fn analyze_multiplicative(ctx: &mut Context, mul: &MultiplicativeExpr) {
    match mul {
        MultiplicativeExpr::Mul(l, r)
        | MultiplicativeExpr::Div(l, r)
        | MultiplicativeExpr::Mod(l, r)
        | MultiplicativeExpr::Pow(l, r) => {
            analyze_multiplicative(ctx, l);
            analyze_unary(ctx, r);
        }
        MultiplicativeExpr::MultiplicativeVal(expr) => analyze_unary(ctx, expr),
    }
}

fn analyze_unary(ctx: &mut Context, unary: &UnaryExpr) {
    match unary {
        UnaryExpr::Neg(expr) | UnaryExpr::Not(expr) => analyze_unary(ctx, expr),
        UnaryExpr::UnaryMember(expr, _) => analyze_unary(ctx, expr),
        UnaryExpr::UnaryCall(call) => analyze_call(ctx, call),
        UnaryExpr::UnaryVal(atom) => analyze_atom(ctx, atom),
    }
}

fn analyze_atom(ctx: &mut Context, atom: &Atom) {
    match atom {
        Atom::Ident(name) if name == "_" => {}
        Atom::Ident(name) => {
            if lookup_symbol(ctx, name).is_some() {
                mark_used(ctx, name);
            } else {
                let suggestions = suggest_names(ctx, name);
                ctx.errors
                    .push(AnalysisResult::Error(AnalysisError::UndefinedVariable(
                        name.clone(),
                        suggestions,
                    )));
            }
        }
        Atom::ImplicitMember(name) => {
            if lookup_symbol(ctx, name).is_some() {
                mark_used(ctx, name);
            } else {
                let suggestions = suggest_names(ctx, name);
                ctx.errors
                    .push(AnalysisResult::Error(AnalysisError::UndefinedVariable(
                        name.clone(),
                        suggestions,
                    )));
            }
        }
        Atom::Grouping(expr) => analyze_expression(ctx, expr),
        _ => {} // literals
    }
}

fn analyze_match(ctx: &mut Context, (expr, arms): &(Box<Expression>, Vec<MatchArm>)) {
    analyze_expression(ctx, expr);
    for arm in arms {
        analyze_match_arm(ctx, arm);
    }
}

fn analyze_match_arm(ctx: &mut Context, (param, match_if, body): &MatchArm) {
    enter_scope(ctx);
    analyze_match_param(ctx, param);
    if let Some(cond) = match_if {
        analyze_expression(ctx, cond);
    }
    analyze_match_arm_body(ctx, body);
    exit_scope(ctx);
}

fn analyze_match_param(ctx: &mut Context, param: &MatchParam) {
    match param {
        MatchParam::PatWildcard
        | MatchParam::PatInt(_)
        | MatchParam::PatBool(_)
        | MatchParam::PatString(_) => {}
        MatchParam::PatIdent(name) => add_symbol(ctx, name.clone(), None, (0, 0)),
        MatchParam::PatEnum(_, _, payloads) => {
            payloads.iter().for_each(|p| analyze_match_param(ctx, p))
        }
        MatchParam::PatTuple(pats) => pats.iter().for_each(|p| analyze_match_param(ctx, p)),
        MatchParam::PatStruct(fields) => {
            fields.iter().for_each(|(_, p)| analyze_match_param(ctx, p))
        }
    }
}

fn analyze_match_arm_body(ctx: &mut Context, (stmts, expr_opt): &MatchArmBody) {
    for stmt in stmts {
        analyze_statement(ctx, stmt);
    }
    if let Some(expr) = expr_opt {
        analyze_expression(ctx, expr);
    }
}

fn analyze_struct(ctx: &mut Context, fields: &Vec<StructField>, with_block: &Option<WithBlock>) {
    for (_fname, ftyp, expr_opt) in fields {
        if let Some(expr) = expr_opt {
            analyze_expression(ctx, expr);
            if let Some(id) = extract_ident_from_expr(expr) {
                set_inferred_type(ctx, &id, ftyp.clone());
            }
        }
    }

    if let Some(block) = with_block {
        enter_scope(ctx);
        for node in block {
            analyze_ast(ctx, node);
        }
        exit_scope(ctx);
    }
}

fn analyze_enum(ctx: &mut Context, variants: &Vec<EnumVariant>, with_block: &Option<WithBlock>) {
    for (_name, body_opt) in variants {
        if let Some(VariantBody::StructBody(fields)) = body_opt {
            for (_fname, ftyp, expr_opt) in fields {
                if let Some(expr) = expr_opt {
                    analyze_expression(ctx, expr);
                    if let Some(id) = extract_ident_from_expr(expr) {
                        set_inferred_type(ctx, &id, ftyp.clone());
                    }
                }
            }
        }
    }

    if let Some(block) = with_block {
        enter_scope(ctx);
        for node in block {
            analyze_ast(ctx, node);
        }
        exit_scope(ctx);
    }
}

fn analyze_derive(ctx: &mut Context, body: &Vec<Node>) {
    for node in body {
        analyze_ast(ctx, node);
    }
}

fn analyze_statement(ctx: &mut Context, stmt: &Statement) {
    match stmt {
        Statement::Open(open_stmt) => analyze_open(ctx, open_stmt),
        Statement::Decl(decl) => analyze_decl(ctx, decl),
        Statement::Return(ret) => analyze_return(ctx, ret),
        Statement::If(if_stmt) => analyze_if(ctx, if_stmt),
        Statement::While(while_stmt) => analyze_while(ctx, while_stmt),
        Statement::For(for_stmt) => analyze_for(ctx, for_stmt),
        Statement::Expression(expr) => analyze_expression(ctx, expr),
    }
}

fn analyze_open(ctx: &mut Context, open_stmt: &OpenStmt) {
    if let Some(mod_name) = open_stmt.mods.first() {
        mark_used(ctx, mod_name);
    }

    for elem in &open_stmt.elements {
        let local_name = elem
            .alias
            .clone()
            .unwrap_or_else(|| elem.path.last().cloned().unwrap_or_default());
        add_symbol(ctx, local_name, None, (0, 0));
    }
}

fn analyze_decl(ctx: &mut Context, decl: &DeclStmt) {
    match decl {
        DeclStmt::Decl {
            tags: _,
            generics: _,
            name,
            params,
            explicit_ret,
            body: (stmts, expr_opt),
        } => {
            add_symbol(ctx, name.clone(), explicit_ret.clone(), (0, 0));

            enter_scope(ctx);
            for param in params {
                match param {
                    DeclParam::Untyped(n) => add_symbol(ctx, n.clone(), None, (0, 0)),
                    DeclParam::Variadic(n, typ_opt) => {
                        add_symbol(ctx, n.clone(), typ_opt.clone(), (0, 0))
                    }
                    DeclParam::Typed(n, typ) => {
                        add_symbol(ctx, n.clone(), Some(typ.clone()), (0, 0))
                    }
                    DeclParam::OptionalTyped(n, typ, default) => {
                        add_symbol(ctx, n.clone(), Some(typ.clone()), (0, 0));
                        analyze_expression(ctx, default);
                    }
                    DeclParam::OptionalUntyped(n, default) => {
                        add_symbol(ctx, n.clone(), None, (0, 0));
                        analyze_expression(ctx, default);
                    }
                }
            }

            for stmt in stmts {
                analyze_statement(ctx, stmt);
            }

            let body_type = if let Some(expr) = expr_opt {
                analyze_expression(ctx, expr);
                let mut inferred = infer_expression_type(ctx, expr);
                if let Type::User(n) = &inferred {
                    if let Some(Type::Generic(m, _)) = explicit_ret {
                        if m == n {
                            inferred = Type::Generic(m.clone(), Vec::new());
                        }
                    }
                }
                match expr.as_ref() {
                    Expression::StructExpr(..) => Type::User(name.clone()),
                    Expression::EnumExpr(..) => Type::User(name.clone()),
                    _ => inferred,
                }
            } else {
                Type::UnitTyp
            };

            if let Some(expected) = explicit_ret {
                if !types_equal(expected, &body_type) {
                    ctx.errors
                        .push(AnalysisResult::Error(AnalysisError::TypeMismatch(
                            expected.clone(),
                            body_type.clone(),
                        )));
                }
            }

            set_inferred_type(ctx, name, body_type);
            exit_scope(ctx);
        }
        DeclStmt::CurryDecl {
            tags: _,
            name,
            curried: _,
            input,
        } => {
            add_symbol(ctx, name.clone(), None, (0, 0));
            for expr in input {
                analyze_expression(ctx, expr);
            }
        }
        DeclStmt::ImportDecl {
            name,
            calling_conf: _,
            link_name: _,
        } => {
            add_symbol(ctx, name.clone(), None, (0, 0));
        }
        DeclStmt::ModuleDecl {
            name,
            exports,
            body,
        } => {
            add_symbol(ctx, name.clone(), None, (0, 0));
            for node in body {
                analyze_ast(ctx, node);
            }
            for export in exports {
                match export {
                    ExportStmt::ExportIdent(ident) | ExportStmt::ExportRename(ident, _) => {
                        if let Some(info) = lookup_symbol_mut(ctx, ident) {
                            info.used = true;
                        }
                    }
                }
            }
        }
        DeclStmt::ExportStmt(export) => match export {
            ExportStmt::ExportIdent(ident) | ExportStmt::ExportRename(ident, _) => {
                if let Some(info) = lookup_symbol_mut(ctx, ident) {
                    info.used = true;
                }
            }
        },
    }
}

fn analyze_return(ctx: &mut Context, ret: &ReturnStmt) {
    if let ReturnStmt::WithExpr(expr) = ret {
        analyze_expression(ctx, expr);
    }
}

fn analyze_if(ctx: &mut Context, if_stmt: &IfStmt) {
    analyze_expression(ctx, &if_stmt.cond);
    enter_scope(ctx);
    for node in &if_stmt.body {
        analyze_ast(ctx, node);
    }
    exit_scope(ctx);
    analyze_else(ctx, &if_stmt.elif);
}

fn analyze_while(ctx: &mut Context, while_stmt: &WhileStmt) {
    analyze_expression(ctx, &while_stmt.cond);
    enter_scope(ctx);
    for node in &while_stmt.body {
        analyze_ast(ctx, node);
    }
    exit_scope(ctx);
}

fn analyze_for(ctx: &mut Context, for_stmt: &ForStmt) {
    match for_stmt {
        ForStmt::ForIter(f) => {
            analyze_expression(ctx, &f.iterable);
            enter_scope(ctx);
            add_symbol(ctx, f.var.clone(), None, (0, 0));
            for node in &f.body {
                analyze_ast(ctx, node);
            }
            exit_scope(ctx);
        }
        ForStmt::ForC(f) => {
            analyze_expression(ctx, &f.init);
            enter_scope(ctx);
            add_symbol(ctx, f.var.clone(), None, (0, 0));
            analyze_expression(ctx, &f.cond);
            analyze_expression(ctx, &f.update);
            for node in &f.body {
                analyze_ast(ctx, node);
            }
            exit_scope(ctx);
        }
        ForStmt::ForTuple(f) => {
            analyze_expression(ctx, &f.iterable);
            enter_scope(ctx);
            for var in &f.vars {
                add_symbol(ctx, var.clone(), None, (0, 0));
            }
            for node in &f.body {
                analyze_ast(ctx, node);
            }
            exit_scope(ctx);
        }
    }
}

fn analyze_else(ctx: &mut Context, elif: &ElseStmt) {
    match elif {
        ElseStmt::ElseIf(cond, body, next) => {
            analyze_expression(ctx, cond);
            enter_scope(ctx);
            for node in body {
                analyze_ast(ctx, node);
            }
            exit_scope(ctx);
            analyze_else(ctx, next);
        }
        ElseStmt::Else(body) => {
            enter_scope(ctx);
            for node in body {
                analyze_ast(ctx, node);
            }
            exit_scope(ctx);
        }
        ElseStmt::Nope => {}
    }
}

fn analyze_ast(ctx: &mut Context, node: &Node) {
    match node {
        Node::Statement(stmt) => analyze_statement(ctx, stmt),
        Node::Expression(expr) => analyze_expression(ctx, expr),
        Node::Error(msg) => {
            ctx.errors
                .push(AnalysisResult::Error(AnalysisError::InvalidOperation(
                    msg.clone(),
                )))
        }
    }
}

fn extract_ident_from_expr(expr: &Expression) -> Option<String> {
    // Matches RelationalVal(AdditiveVal(MultiplicativeVal(UnaryVal(Ident id)))) pattern
    if let Expression::RelationalExpr(RelationalExpr::RelationalVal(add)) = expr {
        if let AdditiveExpr::AdditiveVal(mul) = add.as_ref() {
            if let MultiplicativeExpr::MultiplicativeVal(unary) = mul.as_ref() {
                if let UnaryExpr::UnaryVal(Atom::Ident(id)) = unary.as_ref() {
                    return Some(id.clone());
                }
            }
        }
    }
    None
}
