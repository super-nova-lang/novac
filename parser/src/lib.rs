#![allow(dead_code)]

pub mod nodes;

use lexer::token::{Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    span: Span,
}

type ParseResult<T> = Result<T, ParseError>;

impl ParseError {
    fn new(msg: impl Into<String>, span: Span) -> Self {
        ParseError {
            message: msg.into(),
            span,
        }
    }
}

impl ParseError {
    fn message(&self) -> &str {
        &self.message
    }
    fn span(&self) -> &Span {
        &self.span
    }
}

use nodes::*;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Node>> {
    Parser::new(tokens).parse()
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        // Strip doc comments to match OCaml parser behavior
        let tokens = tokens
            .into_iter()
            .filter(|t| !matches!(t.kind, TokenKind::DocComment(_)))
            .collect();

        Parser { tokens, pos: 0 }
    }

    fn parse(&mut self) -> ParseResult<Vec<Node>> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            match self.parse_toplevel() {
                Ok(node) => stmts.push(node),
                Err(err) => {
                    stmts.push(Node::Error(err.message.clone(), err.span.clone()));
                    self.breakout();
                }
            }
        }
        Ok(stmts)
    }

    fn breakout(&mut self) {
        while !self.is_at_end() {
            match self.peek().kind {
                TokenKind::SemiColon => {
                    self.advance();
                    break;
                }
                _ => self.advance(),
            }
        }
    }

    fn parse_toplevel(&mut self) -> ParseResult<Node> {
        let tags = self.parse_tags();
        match self.peek().kind {
            TokenKind::Module => {
                self.advance();
                let name = match self.consume().kind {
                    TokenKind::Ident(s) => s,
                    t => return Err(self.fail(format!("Expected module name, got {:?}", t))),
                };

                self.expect(TokenKind::OpenBrack)?;
                let mut exports = Vec::new();
                let mut body = Vec::new();

                while !matches!(self.peek().kind, TokenKind::CloseBrack | TokenKind::Eof) {
                    match self.peek().kind {
                        TokenKind::Export => {
                            self.advance();
                            match self.peek().kind {
                                TokenKind::Ident(s) => {
                                    self.advance();
                                    exports.push(ExportStmt::ExportIdent(s));
                                }
                                TokenKind::OpenBrack => {
                                    self.advance();
                                    self.parse_exports(&mut exports)?;
                                }
                                t => {
                                    return Err(self.fail(format!(
                                        "Expected identifier or {{ after export, got {:?}",
                                        t
                                    )));
                                }
                            }
                        }
                        _ => match self.parse_toplevel() {
                            Ok(stmt) => body.push(stmt),
                            Err(err) => body.push(Node::Error(err.message, err.span)),
                        },
                    }
                }

                self.expect(TokenKind::CloseBrack)?;
                Ok(Node::Statement(Statement::Decl(DeclStmt::ModuleDecl {
                    name,
                    exports,
                    body,
                })))
            }
            TokenKind::Export => {
                self.advance();
                match self.consume().kind {
                    TokenKind::Ident(s) => Ok(Node::Statement(Statement::Decl(
                        DeclStmt::ExportStmt(ExportStmt::ExportIdent(s)),
                    ))),
                    TokenKind::OpenBrack => {
                        let mut exports = Vec::new();
                        self.parse_exports(&mut exports)?;
                        if let Some(first) = exports.first().cloned() {
                            Ok(Node::Statement(Statement::Decl(DeclStmt::ExportStmt(
                                first,
                            ))))
                        } else {
                            Err(self.fail("Empty export list"))
                        }
                    }
                    t => Err(self.fail(format!(
                        "Expected identifier or {{ after export, got {:?}",
                        t
                    ))),
                }
            }
            TokenKind::Let => self
                .parse_decl_stmt(tags)
                .map(Statement::Decl)
                .map(Node::Statement),
            TokenKind::Ident(_) => {
                let current_pos = self.pos;
                self.advance();
                let is_decl = matches!(
                    self.peek().kind,
                    TokenKind::Colon | TokenKind::DoubleColon | TokenKind::OpenSquare
                );
                self.pos = current_pos;
                if is_decl {
                    self.parse_decl_stmt(tags)
                        .map(Statement::Decl)
                        .map(Node::Statement)
                } else {
                    self.parse_statement().map(Node::Statement)
                }
            }
            _ => self.parse_statement().map(Node::Statement),
        }
    }

    fn parse_exports(&mut self, exports: &mut Vec<ExportStmt>) -> ParseResult<()> {
        loop {
            match self.peek().kind {
                TokenKind::Ident(s) => {
                    self.advance();
                    if matches!(self.peek().kind, TokenKind::As) {
                        self.advance();
                        match self.consume().kind {
                            TokenKind::Ident(alias) => {
                                exports.push(ExportStmt::ExportRename(s, alias))
                            }
                            t => {
                                return Err(self.fail(format!("Expected alias ident, got {:?}", t)));
                            }
                        }
                    } else {
                        exports.push(ExportStmt::ExportIdent(s));
                    }
                    if matches!(self.peek().kind, TokenKind::Comma) {
                        self.advance();
                        continue;
                    }
                }
                TokenKind::CloseBrack => {
                    self.advance();
                }
                t => return Err(self.fail(format!("Unexpected token in export list: {:?}", t))),
            }
            break;
        }
        Ok(())
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek().kind {
            TokenKind::Open => self.parse_open_stmt(),
            TokenKind::Let => self.parse_decl_stmt(Vec::new()).map(Statement::Decl),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Hash => Err(self.fail("Tags can only be applied to declarations")),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_open_stmt(&mut self) -> ParseResult<Statement> {
        self.advance();
        let mods = self.parse_module_path();
        let elements = if matches!(self.peek().kind, TokenKind::With) {
            self.advance();
            self.expect(TokenKind::OpenBrack)?;
            let elems = self.parse_list(&[TokenKind::CloseBrack], |p| p.parse_open_element());
            self.expect(TokenKind::CloseBrack)?;
            elems?
        } else {
            Vec::new()
        };
        Ok(Statement::Open(OpenStmt { mods, elements }))
    }

    fn parse_module_path(&mut self) -> Vec<String> {
        let mut acc = Vec::new();
        loop {
            match self.peek().kind {
                TokenKind::Ident(ident) => {
                    let ident = ident.clone();
                    self.advance();
                    acc.push(ident);
                    if matches!(self.peek().kind, TokenKind::Dot) {
                        self.advance();
                        continue;
                    }
                }
                _ => {}
            }
            break;
        }
        acc
    }

    fn parse_open_element(&mut self) -> ParseResult<OpenStmtElement> {
        let path = self.parse_module_path();
        let alias = if matches!(self.peek().kind, TokenKind::As) {
            self.advance();
            match self.peek().kind {
                TokenKind::Ident(alias) => {
                    let a = alias.clone();
                    self.advance();
                    Some(a)
                }
                _ => None,
            }
        } else {
            None
        };
        Ok(OpenStmtElement { path, alias })
    }

    fn parse_tags(&mut self) -> Vec<Tag> {
        let mut acc = Vec::new();
        while matches!(self.peek().kind, TokenKind::Hash) {
            acc.push(self.parse_tag());
        }
        acc
    }

    fn parse_tag(&mut self) -> Tag {
        self.expect(TokenKind::Hash).unwrap();
        self.expect(TokenKind::OpenSquare).unwrap();
        let tag = match self.peek().kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                if matches!(self.peek().kind, TokenKind::OpenParen) {
                    let call = self.parse_call_expr_with_name(&name).unwrap();
                    Tag::TagCall(call)
                } else {
                    Tag::TagName(name)
                }
            }
            TokenKind::Derive => {
                self.advance();
                if matches!(self.peek().kind, TokenKind::OpenParen) {
                    let call = self.parse_call_expr_with_name("derive").unwrap();
                    Tag::TagCall(call)
                } else {
                    Tag::TagName("derive".to_string())
                }
            }
            _ => panic!("Expected identifier in tag"),
        };
        self.expect(TokenKind::CloseSquare).unwrap();
        tag
    }

    fn parse_decl_stmt(&mut self, tags: Vec<Tag>) -> ParseResult<DeclStmt> {
        if matches!(self.peek().kind, TokenKind::Let) {
            self.advance();
        }

        let name = match self.consume().kind {
            TokenKind::Ident(s) => s,
            t => return Err(self.fail(format!("Expected identifier for declaration, got {:?}", t))),
        };

        let generics = if matches!(self.peek().kind, TokenKind::OpenSquare) {
            self.advance();
            let gens = self.parse_list(&[TokenKind::CloseSquare], |p| match p.consume().kind {
                TokenKind::Ident(s) => Ok(s),
                t => Err(p.fail(format!(
                    "Expected identifier in generic parameter list, got {:?}",
                    t
                ))),
            })?;
            self.expect(TokenKind::CloseSquare)?;
            gens
        } else {
            Vec::new()
        };

        match self.peek().kind {
            TokenKind::DoubleColon => {
                self.advance();
                if matches!(self.peek().kind, TokenKind::Import) {
                    self.advance();
                    self.expect(TokenKind::BackArrow)?;
                    let calling_conf = match self.consume().kind {
                        TokenKind::String(s) => s,
                        t => {
                            return Err(self
                                .fail(format!("Expected calling convention string, got {:?}", t)));
                        }
                    };
                    self.expect(TokenKind::Comma)?;
                    let link_name = match self.consume().kind {
                        TokenKind::String(s) => s,
                        t => {
                            return Err(
                                self.fail(format!("Expected link name string, got {:?}", t))
                            );
                        }
                    };
                    Ok(DeclStmt::ImportDecl {
                        name,
                        calling_conf,
                        link_name,
                    })
                } else {
                    let current_pos = self.pos;
                    let is_curry = match self.consume().kind {
                        TokenKind::Ident(_) => matches!(self.peek().kind, TokenKind::BackArrow),
                        _ => false,
                    };
                    self.pos = current_pos;

                    if is_curry {
                        let curried = match self.consume().kind {
                            TokenKind::Ident(s) => s,
                            _ => unreachable!(),
                        };
                        self.expect(TokenKind::BackArrow)?;
                        let input = self.parse_list(
                            &[TokenKind::SemiColon, TokenKind::Eof, TokenKind::Let],
                            |p| p.parse_expression(),
                        )?;
                        Ok(DeclStmt::CurryDecl {
                            tags,
                            name,
                            curried,
                            input,
                        })
                    } else {
                        let params = if matches!(
                            self.peek().kind,
                            TokenKind::Eql | TokenKind::SkinnyArrow
                        ) {
                            Vec::new()
                        } else if matches!(self.peek().kind, TokenKind::OpenParen) {
                            self.advance();
                            if matches!(self.peek().kind, TokenKind::CloseParen) {
                                self.advance();
                                Vec::new()
                            } else {
                                let params = self.parse_list(&[TokenKind::CloseParen], |p| {
                                    p.parse_decl_param()
                                })?;
                                self.expect(TokenKind::CloseParen)?;
                                params
                            }
                        } else {
                            self.parse_list(&[TokenKind::Eql, TokenKind::SkinnyArrow], |p| {
                                p.parse_decl_param()
                            })?
                        };

                        let explicit_ret = if matches!(self.peek().kind, TokenKind::SkinnyArrow) {
                            self.advance();
                            Some(self.parse_type())
                        } else {
                            None
                        };

                        let body = if matches!(self.peek().kind, TokenKind::Eql | TokenKind::Walrus)
                        {
                            self.advance();
                            self.parse_body()?
                        } else {
                            (Vec::new(), None)
                        };

                        Ok(DeclStmt::Decl {
                            tags,
                            name,
                            generics,
                            params,
                            explicit_ret,
                            body,
                        })
                    }
                }
            }
            TokenKind::BackArrow => {
                self.advance();
                let curried = match self.consume().kind {
                    TokenKind::Ident(s) => s,
                    _ => return Err(self.fail("Expected identifier for currying")),
                };
                let input = self.parse_list(
                    &[TokenKind::SemiColon, TokenKind::Eof, TokenKind::Let],
                    |p| p.parse_expression(),
                )?;
                Ok(DeclStmt::CurryDecl {
                    tags,
                    name,
                    curried,
                    input,
                })
            }
            TokenKind::Colon => {
                self.advance();
                let explicit_ret = Some(self.parse_type());
                let body = if matches!(self.peek().kind, TokenKind::Eql | TokenKind::Walrus) {
                    self.advance();
                    self.parse_body()?
                } else {
                    (Vec::new(), None)
                };
                Ok(DeclStmt::Decl {
                    tags,
                    name,
                    generics: Vec::new(),
                    params: Vec::new(),
                    explicit_ret,
                    body,
                })
            }
            TokenKind::Eql | TokenKind::Walrus => {
                self.advance();
                let body = self.parse_body()?;
                Ok(DeclStmt::Decl {
                    tags,
                    name,
                    generics: Vec::new(),
                    params: Vec::new(),
                    explicit_ret: None,
                    body,
                })
            }
            _ => Err(self.fail(format!("Unexpected token {:?} in declaration", self.peek()))),
        }
    }

    fn parse_decl_param(&mut self) -> ParseResult<DeclParam> {
        let is_optional = matches!(self.peek().kind, TokenKind::Question);
        if is_optional {
            self.advance();
        }
        let name = match self.consume().kind {
            TokenKind::Ident(s) => s,
            t => return Err(self.fail(format!("Expected identifier for parameter, got {:?}", t))),
        };

        let param_type = if matches!(self.peek().kind, TokenKind::Colon) {
            self.advance();
            Some(self.parse_type())
        } else {
            None
        };

        let is_variadic = matches!(self.peek().kind, TokenKind::Ellipsis);
        if is_variadic {
            self.advance();
            if is_optional {
                return Err(self.fail("Optional variadic parameters are not supported"));
            }
            return Ok(DeclParam::Variadic(name, param_type));
        }

        let default_value = if is_optional && matches!(self.peek().kind, TokenKind::Eql) {
            self.advance();
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        match (is_optional, param_type, default_value) {
            (false, Some(t), None) => Ok(DeclParam::Typed(name, t)),
            (false, None, None) => Ok(DeclParam::Untyped(name)),
            (true, Some(t), Some(e)) => Ok(DeclParam::OptionalTyped(name, t, e)),
            (true, None, Some(e)) => Ok(DeclParam::OptionalUntyped(name, e)),
            _ => Err(self.fail("Invalid parameter declaration")),
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.consume().kind {
            TokenKind::Ident(s) if s.starts_with('\'') => Type::TypeVar(s),
            TokenKind::Ident(s) => {
                // Check if this is an uppercase identifier (likely a type variable)
                let is_type_var = s.chars().next().map_or(false, |c| c.is_uppercase());

                let generics = if matches!(self.peek().kind, TokenKind::OpenSquare) {
                    self.advance();
                    let args = self.parse_list(&[TokenKind::CloseSquare], |p| Ok(p.parse_type()));
                    self.expect(TokenKind::CloseSquare).ok();
                    args.unwrap_or_default()
                } else {
                    Vec::new()
                };

                if generics.is_empty() && is_type_var {
                    Type::TypeVar(s)
                } else if generics.is_empty() {
                    Type::User(s)
                } else {
                    Type::Generic(s, generics)
                }
            }
            TokenKind::OpenSquare => {
                let inner = self.parse_type();
                self.expect(TokenKind::CloseSquare).unwrap();
                Type::ListTyp(Box::new(inner))
            }
            TokenKind::OpenParen => {
                self.expect(TokenKind::CloseParen).unwrap();
                Type::UnitTyp
            }
            _ => Type::UnitTyp,
        }
    }

    fn parse_body(&mut self) -> ParseResult<DeclBody> {
        if matches!(self.peek().kind, TokenKind::OpenBrack) {
            self.advance();
            let mut stmts = Vec::new();
            let mut expr = None;
            while !matches!(self.peek().kind, TokenKind::CloseBrack | TokenKind::Eof) {
                let current_pos = self.pos;
                let is_last = match self.parse_expression() {
                    Ok(_) => matches!(self.peek().kind, TokenKind::CloseBrack),
                    Err(_) => false,
                };
                self.pos = current_pos;
                if is_last {
                    expr = Some(Box::new(self.parse_expression()?));
                } else {
                    let stmt = self.parse_statement()?;
                    stmts.push(stmt);
                    if matches!(self.peek().kind, TokenKind::SemiColon) {
                        self.advance();
                    }
                }
            }
            self.expect(TokenKind::CloseBrack)?;
            Ok((stmts, expr))
        } else {
            let expr = Box::new(self.parse_expression()?);
            Ok((Vec::new(), Some(expr)))
        }
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Statement> {
        self.expect(TokenKind::Return)?;
        if matches!(self.peek().kind, TokenKind::SemiColon) || self.is_at_end() {
            Ok(Statement::Return(ReturnStmt::Naked))
        } else {
            let expr = Box::new(self.parse_expression()?);
            Ok(Statement::Return(ReturnStmt::WithExpr(expr)))
        }
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Statement> {
        self.expect(TokenKind::If)?;
        let cond = Box::new(self.parse_expression()?);
        let body = self.parse_body_as_t_list();
        let elif = self.parse_else_stmt();
        Ok(Statement::If(IfStmt { cond, body, elif }))
    }

    fn parse_else_stmt(&mut self) -> ElseStmt {
        if matches!(self.peek().kind, TokenKind::Else) {
            self.advance();
            if matches!(self.peek().kind, TokenKind::If) {
                self.advance();
                let cond = Box::new(self.parse_expression().unwrap());
                let body = self.parse_body_as_t_list();
                let elif = self.parse_else_stmt();
                ElseStmt::ElseIf(cond, body, Box::new(elif))
            } else {
                let body = self.parse_body_as_t_list();
                ElseStmt::Else(body)
            }
        } else {
            ElseStmt::Nope
        }
    }

    fn parse_while_stmt(&mut self) -> ParseResult<Statement> {
        self.expect(TokenKind::While)?;
        let cond = Box::new(self.parse_expression()?);
        let body = self.parse_body_as_t_list();
        Ok(Statement::While(WhileStmt { cond, body }))
    }

    fn parse_for_stmt(&mut self) -> ParseResult<Statement> {
        self.expect(TokenKind::For)?;
        match self.peek().kind {
            TokenKind::Let => {
                self.advance();
                let var = match self.consume().kind {
                    TokenKind::Ident(s) => s,
                    _ => return Err(self.fail("Expected variable name in for loop initialization")),
                };
                self.expect(TokenKind::Walrus)?;
                let init = Box::new(self.parse_expression()?);
                self.expect(TokenKind::SemiColon)?;
                let cond = Box::new(self.parse_expression()?);
                self.expect(TokenKind::SemiColon)?;
                let update = Box::new(self.parse_expression()?);
                let body = self.parse_body_as_t_list();
                Ok(Statement::For(ForStmt::ForC(ForCStmt {
                    var,
                    init,
                    cond,
                    update,
                    body,
                })))
            }
            TokenKind::OpenParen => {
                self.advance();
                let mut vars = Vec::new();
                while !matches!(self.peek().kind, TokenKind::CloseParen) {
                    match self.consume().kind {
                        TokenKind::Ident(s) => vars.push(s),
                        _ => return Err(self.fail("Expected variable name in tuple pattern")),
                    }
                }
                self.expect(TokenKind::CloseParen)?;
                self.expect(TokenKind::In)?;
                let iterable = Box::new(self.parse_expression()?);
                let body = self.parse_body_as_t_list();
                Ok(Statement::For(ForStmt::ForTuple(ForTupleStmt {
                    vars,
                    iterable,
                    body,
                })))
            }
            TokenKind::Ident(_) => {
                let var = match self.consume().kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                self.expect(TokenKind::In)?;
                let iterable = Box::new(self.parse_expression()?);
                let body = self.parse_body_as_t_list();
                Ok(Statement::For(ForStmt::ForIter(ForIterStmt {
                    var,
                    iterable,
                    body,
                })))
            }
            _ => Err(self.fail("Expected 'let', '(', or variable name after 'for'")),
        }
    }

    fn parse_body_as_t_list(&mut self) -> Vec<Node> {
        if matches!(self.peek().kind, TokenKind::OpenBrack) {
            self.advance();
            let mut stmts = Vec::new();
            while !matches!(self.peek().kind, TokenKind::CloseBrack | TokenKind::Eof) {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(Node::Statement(stmt)),
                    Err(err) => stmts.push(Node::Error(err.message, err.span.clone())),
                }
            }
            let _ = self.expect(TokenKind::CloseBrack);
            stmts
        } else {
            match self.parse_statement() {
                Ok(stmt) => vec![Node::Statement(stmt)],
                Err(err) => vec![Node::Error(err.message, err.span)],
            }
        }
    }

    fn parse_expression_stmt(&mut self) -> ParseResult<Statement> {
        let expr = self.parse_expression()?;
        if matches!(self.peek().kind, TokenKind::SemiColon) {
            self.advance();
        }
        Ok(Statement::Expression(Box::new(expr)))
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        match self.peek().kind {
            TokenKind::Struct => self.parse_struct_expr(),
            TokenKind::Enum => self.parse_enum_expr(),
            TokenKind::Derive => self.parse_derive_expr(),
            TokenKind::Match => self.parse_match_expr(),
            TokenKind::OpenSquare => self.parse_list_expr(),
            _ => self.parse_relational_expr(),
        }
    }

    fn parse_list_expr(&mut self) -> ParseResult<Expression> {
        self.expect(TokenKind::OpenSquare)?;
        let elems = if matches!(self.peek().kind, TokenKind::CloseSquare) {
            self.advance();
            Vec::new()
        } else {
            let es = self.parse_list(&[TokenKind::CloseSquare], |p| p.parse_expression())?;
            self.expect(TokenKind::CloseSquare)?;
            es
        };
        Ok(Expression::ListExpr(elems))
    }

    fn parse_match_expr(&mut self) -> ParseResult<Expression> {
        self.expect(TokenKind::Match)?;
        let target = Box::new(self.parse_expression()?);
        self.expect(TokenKind::With)?;
        let mut arms = Vec::new();
        while matches!(self.peek().kind, TokenKind::Bar) {
            self.advance();
            let param = self.parse_match_param();
            let if_opt = if matches!(self.peek().kind, TokenKind::If) {
                self.advance();
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };
            self.expect(TokenKind::SkinnyArrow)?;
            let body = self.parse_match_arm_body()?;
            arms.push((param, if_opt, body));
        }
        Ok(Expression::MatchExpr((target, arms)))
    }

    fn parse_match_param(&mut self) -> MatchParam {
        if matches!(self.peek().kind, TokenKind::OpenParen) {
            self.advance();
            let pats = self
                .parse_list(&[TokenKind::CloseParen], |p| Ok(p.parse_match_param()))
                .unwrap_or_default();
            self.expect(TokenKind::CloseParen).unwrap();
            if pats.len() == 1 {
                pats.into_iter().next().unwrap()
            } else {
                MatchParam::PatTuple(pats)
            }
        } else {
            let expr = self.parse_expression().unwrap();
            self.pattern_of_expr(&expr)
        }
    }

    fn pattern_of_expr(&self, expr: &Expression) -> MatchParam {
        match expr {
            Expression::RelationalExpr(RelationalExpr::RelationalVal(add)) => {
                self.pattern_of_add(add)
            }
            _ => MatchParam::PatWildcard,
        }
    }

    fn pattern_of_add(&self, add: &AdditiveExpr) -> MatchParam {
        match add {
            AdditiveExpr::AdditiveVal(mul) => self.pattern_of_mul(mul),
            _ => MatchParam::PatWildcard,
        }
    }

    fn pattern_of_mul(&self, mul: &MultiplicativeExpr) -> MatchParam {
        match mul {
            MultiplicativeExpr::MultiplicativeVal(unary) => self.pattern_of_unary(unary),
            _ => MatchParam::PatWildcard,
        }
    }

    fn pattern_of_unary(&self, unary: &UnaryExpr) -> MatchParam {
        match unary {
            UnaryExpr::UnaryVal(atom) => self.pattern_of_atom(atom),
            UnaryExpr::UnaryCall(CallExpr::DeclCall(callee, params)) => {
                if let Some((ename, vname)) = self.extract_member(callee.as_ref()) {
                    let payload = params
                        .iter()
                        .map(|p| match p {
                            CallParam::Positional(expr) => self.pattern_of_expr(expr),
                            CallParam::Named(_, _) => MatchParam::PatWildcard,
                        })
                        .collect();
                    MatchParam::PatEnum(ename, vname, payload)
                } else {
                    MatchParam::PatWildcard
                }
            }
            UnaryExpr::UnaryMember(inner, field) => match inner.as_ref() {
                UnaryExpr::UnaryVal(Atom::Ident(ename)) => {
                    MatchParam::PatEnum(ename.clone(), field.clone(), Vec::new())
                }
                _ => MatchParam::PatWildcard,
            },
            _ => MatchParam::PatWildcard,
        }
    }

    fn extract_member(&self, expr: &Expression) -> Option<(String, String)> {
        fn extract_member(expr: &Expression) -> Option<(String, String)> {
            match expr {
                Expression::RelationalExpr(RelationalExpr::RelationalVal(add)) => {
                    extract_member_add(add)
                }
                _ => None,
            }
        }

        fn extract_member_add(add: &AdditiveExpr) -> Option<(String, String)> {
            match add {
                AdditiveExpr::AdditiveVal(mul) => extract_member_mul(mul),
                _ => None,
            }
        }

        fn extract_member_mul(mul: &MultiplicativeExpr) -> Option<(String, String)> {
            match mul {
                MultiplicativeExpr::MultiplicativeVal(unary) => extract_member_unary(unary),
                _ => None,
            }
        }

        fn extract_member_unary(unary: &UnaryExpr) -> Option<(String, String)> {
            if let UnaryExpr::UnaryMember(inner, field) = unary {
                if let UnaryExpr::UnaryVal(Atom::Ident(base)) = inner.as_ref() {
                    return Some((base.clone(), field.clone()));
                }
            }
            None
        }

        extract_member(expr)
    }

    fn pattern_of_atom(&self, atom: &Atom) -> MatchParam {
        match atom {
            Atom::Int(n) => MatchParam::PatInt(*n),
            Atom::Bool(b) => MatchParam::PatBool(*b),
            Atom::String(s) => MatchParam::PatString(s.clone()),
            Atom::Ident(id) if id == "_" => MatchParam::PatWildcard,
            Atom::Ident(id) => MatchParam::PatIdent(id.clone()),
            Atom::Grouping(expr) => self.pattern_of_expr(expr),
            _ => MatchParam::PatWildcard,
        }
    }

    fn parse_match_arm_body(&mut self) -> ParseResult<MatchArmBody> {
        if matches!(self.peek().kind, TokenKind::OpenBrack) {
            self.parse_body()
        } else {
            let e = Box::new(self.parse_expression()?);
            Ok((Vec::new(), Some(e)))
        }
    }

    fn parse_with_block(&mut self) -> Option<WithBlock> {
        if matches!(self.peek().kind, TokenKind::With) {
            self.advance();
            self.expect(TokenKind::OpenBrack).ok()?;
            let mut stmts = Vec::new();
            while !matches!(self.peek().kind, TokenKind::CloseBrack | TokenKind::Eof) {
                match self.parse_toplevel() {
                    Ok(stmt) => {
                        stmts.push(stmt);
                        if matches!(self.peek().kind, TokenKind::SemiColon | TokenKind::Comma) {
                            self.advance();
                        }
                    }
                    Err(err) => stmts.push(Node::Error(err.message, err.span.clone())),
                }
            }
            self.expect(TokenKind::CloseBrack).ok()?;
            Some(stmts)
        } else {
            None
        }
    }

    fn parse_struct_expr(&mut self) -> ParseResult<Expression> {
        self.expect(TokenKind::Struct)?;
        self.expect(TokenKind::OpenBrack)?;
        let fields = self.parse_list(&[TokenKind::CloseBrack], |p| p.parse_struct_field())?;
        self.expect(TokenKind::CloseBrack)?;
        let with_blk = self.parse_with_block();
        Ok(Expression::StructExpr(fields, with_blk))
    }

    fn parse_struct_field(&mut self) -> ParseResult<StructField> {
        let name = match self.consume().kind {
            TokenKind::Ident(s) => s,
            _ => return Err(self.fail("Expected identifier")),
        };
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type();
        let expr = if matches!(self.peek().kind, TokenKind::Eql) {
            self.advance();
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        Ok((name, typ, expr))
    }

    fn parse_enum_expr(&mut self) -> ParseResult<Expression> {
        self.expect(TokenKind::Enum)?;
        self.expect(TokenKind::OpenBrack)?;
        let variants = self.parse_list(&[TokenKind::CloseBrack], |p| p.parse_enum_variant())?;
        self.expect(TokenKind::CloseBrack)?;
        let with_blk = self.parse_with_block();
        Ok(Expression::EnumExpr(variants, with_blk))
    }

    fn parse_enum_variant(&mut self) -> ParseResult<EnumVariant> {
        let name = match self.consume().kind {
            TokenKind::Ident(s) => s,
            t => return Err(self.fail(format!("Expected identifier, got {:?}", t))),
        };
        let body = if matches!(self.peek().kind, TokenKind::DoubleColon) {
            self.advance();
            if matches!(self.peek().kind, TokenKind::Struct) {
                self.advance();
                self.expect(TokenKind::OpenBrack)?;
                let fields =
                    self.parse_list(&[TokenKind::CloseBrack], |p| p.parse_struct_field())?;
                self.expect(TokenKind::CloseBrack)?;
                Some(VariantBody::StructBody(fields))
            } else {
                Some(VariantBody::TypeBody(self.parse_type()))
            }
        } else {
            None
        };
        Ok((name, body))
    }

    fn parse_derive_expr(&mut self) -> ParseResult<Expression> {
        self.expect(TokenKind::Derive)?;
        self.expect(TokenKind::OpenBrack)?;
        let mut stmts = Vec::new();
        while !matches!(self.peek().kind, TokenKind::CloseBrack | TokenKind::Eof) {
            match self.parse_toplevel() {
                Ok(stmt) => {
                    stmts.push(stmt);
                    if matches!(self.peek().kind, TokenKind::SemiColon) {
                        self.advance();
                    }
                }
                Err(err) => stmts.push(Node::Error(err.message, err.span.clone())),
            }
        }
        self.expect(TokenKind::CloseBrack)?;
        Ok(Expression::DeriveExpr(stmts))
    }

    fn parse_relational_expr(&mut self) -> ParseResult<Expression> {
        let left = Box::new(self.parse_additive_expr()?);
        match self.peek().kind {
            TokenKind::DoubleEql => {
                self.advance();
                Ok(Expression::RelationalExpr(RelationalExpr::Eql(
                    left,
                    Box::new(self.parse_additive_expr()?),
                )))
            }
            TokenKind::LesserEql => {
                self.advance();
                Ok(Expression::RelationalExpr(RelationalExpr::Leq(
                    left,
                    Box::new(self.parse_additive_expr()?),
                )))
            }
            TokenKind::GreaterEql => {
                self.advance();
                Ok(Expression::RelationalExpr(RelationalExpr::Geq(
                    left,
                    Box::new(self.parse_additive_expr()?),
                )))
            }
            TokenKind::Lesser => {
                self.advance();
                Ok(Expression::RelationalExpr(RelationalExpr::Lt(
                    left,
                    Box::new(self.parse_additive_expr()?),
                )))
            }
            TokenKind::Greater => {
                self.advance();
                Ok(Expression::RelationalExpr(RelationalExpr::Gt(
                    left,
                    Box::new(self.parse_additive_expr()?),
                )))
            }
            TokenKind::PlusEql => {
                if let Some(name) = Self::assignment_ident(left.as_ref()) {
                    self.advance();
                    let right = Box::new(self.parse_additive_expr()?);
                    Ok(Expression::AssignmentExpr(AssignmentExpr::AddAssign(
                        name, right,
                    )))
                } else {
                    Err(self.fail("Assignment target must be an identifier"))
                }
            }
            TokenKind::MinusEql => {
                if let Some(name) = Self::assignment_ident(left.as_ref()) {
                    self.advance();
                    let right = Box::new(self.parse_additive_expr()?);
                    Ok(Expression::AssignmentExpr(AssignmentExpr::SubAssign(
                        name, right,
                    )))
                } else {
                    Err(self.fail("Assignment target must be an identifier"))
                }
            }
            TokenKind::StarEql => {
                if let Some(name) = Self::assignment_ident(left.as_ref()) {
                    self.advance();
                    let right = Box::new(self.parse_additive_expr()?);
                    Ok(Expression::AssignmentExpr(AssignmentExpr::MulAssign(
                        name, right,
                    )))
                } else {
                    Err(self.fail("Assignment target must be an identifier"))
                }
            }
            TokenKind::SlashEql => {
                if let Some(name) = Self::assignment_ident(left.as_ref()) {
                    self.advance();
                    let right = Box::new(self.parse_additive_expr()?);
                    Ok(Expression::AssignmentExpr(AssignmentExpr::DivAssign(
                        name, right,
                    )))
                } else {
                    Err(self.fail("Assignment target must be an identifier"))
                }
            }
            _ => Ok(Expression::RelationalExpr(RelationalExpr::RelationalVal(
                left,
            ))),
        }
    }

    fn assignment_ident(add: &AdditiveExpr) -> Option<String> {
        if let AdditiveExpr::AdditiveVal(mul) = add {
            if let MultiplicativeExpr::MultiplicativeVal(unary) = mul.as_ref() {
                if let UnaryExpr::UnaryVal(Atom::Ident(name)) = unary.as_ref() {
                    return Some(name.clone());
                }
            }
        }
        None
    }

    fn parse_additive_expr(&mut self) -> ParseResult<AdditiveExpr> {
        let mut left = AdditiveExpr::AdditiveVal(Box::new(self.parse_multiplicative_expr()?));
        loop {
            match self.peek().kind {
                TokenKind::Plus => {
                    self.advance();
                    left = AdditiveExpr::Add(
                        Box::new(left),
                        Box::new(self.parse_multiplicative_expr()?),
                    );
                }
                TokenKind::Dash => {
                    self.advance();
                    left = AdditiveExpr::Sub(
                        Box::new(left),
                        Box::new(self.parse_multiplicative_expr()?),
                    );
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> ParseResult<MultiplicativeExpr> {
        let mut left = MultiplicativeExpr::MultiplicativeVal(Box::new(self.parse_unary_expr()?));
        loop {
            match self.peek().kind {
                TokenKind::Star => {
                    self.advance();
                    left =
                        MultiplicativeExpr::Mul(Box::new(left), Box::new(self.parse_unary_expr()?));
                }
                TokenKind::ForwardSlash => {
                    self.advance();
                    left =
                        MultiplicativeExpr::Div(Box::new(left), Box::new(self.parse_unary_expr()?));
                }
                TokenKind::Mod => {
                    self.advance();
                    left =
                        MultiplicativeExpr::Mod(Box::new(left), Box::new(self.parse_unary_expr()?));
                }
                TokenKind::Carrot => {
                    self.advance();
                    left =
                        MultiplicativeExpr::Pow(Box::new(left), Box::new(self.parse_unary_expr()?));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> ParseResult<UnaryExpr> {
        match self.peek().kind {
            TokenKind::Dash => {
                self.advance();
                Ok(UnaryExpr::Neg(Box::new(self.parse_unary_expr()?)))
            }
            TokenKind::Bang => {
                self.advance();
                Ok(UnaryExpr::Not(Box::new(self.parse_unary_expr()?)))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> ParseResult<UnaryExpr> {
        let mut res = match self.peek().kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                UnaryExpr::UnaryVal(Atom::Ident(name))
            }
            TokenKind::Dot => {
                self.advance();
                match self.consume().kind {
                    TokenKind::Ident(name) => UnaryExpr::UnaryVal(Atom::ImplicitMember(name)),
                    _ => return Err(self.fail("Expected identifier after .")),
                }
            }
            _ => UnaryExpr::UnaryVal(self.parse_atom()?),
        };

        loop {
            match self.peek().kind {
                TokenKind::Dot => {
                    self.advance();
                    match self.consume().kind {
                        TokenKind::Ident(member) => {
                            res = UnaryExpr::UnaryMember(Box::new(res), member)
                        }
                        _ => return Err(self.fail("Expected identifier after .")),
                    }
                }
                TokenKind::OpenParen => {
                    self.advance();
                    let params =
                        self.parse_list(&[TokenKind::CloseParen], |p| p.parse_call_param())?;
                    self.expect(TokenKind::CloseParen)?;
                    let call = CallExpr::DeclCall(Box::new(self.wrap_unary(res.clone())), params);
                    res = UnaryExpr::UnaryCall(call);
                }
                TokenKind::OpenBrack => {
                    // Struct constructor: Name { field: value, ... } or Name { .field = value, ... }
                    self.advance();
                    let fields = self.parse_list(&[TokenKind::CloseBrack], |p| {
                        // Check if this is the shorthand .field = value syntax or regular field: type = value
                        if matches!(p.peek().kind, TokenKind::Dot) {
                            p.advance();
                            let name = match p.consume().kind {
                                TokenKind::Ident(s) => s,
                                _ => return Err(p.fail("Expected identifier after .")),
                            };
                            p.expect(TokenKind::Eql)?;
                            let expr = Box::new(p.parse_expression()?);
                            // For shorthand, we don't have a type, use Unit type
                            Ok((name, Type::UnitTyp, Some(expr)))
                        } else {
                            p.parse_struct_field()
                        }
                    })?;
                    self.expect(TokenKind::CloseBrack)?;
                    let struct_expr = Expression::StructExpr(fields, None);
                    res = UnaryExpr::UnaryVal(Atom::Grouping(Box::new(struct_expr)));
                }
                _ => break,
            }
        }

        Ok(res)
    }

    fn parse_call_expr_with_name(&mut self, name: &str) -> ParseResult<CallExpr> {
        self.expect(TokenKind::OpenParen)?;
        let params = self.parse_list(&[TokenKind::CloseParen], |p| p.parse_call_param())?;
        self.expect(TokenKind::CloseParen)?;
        let target = UnaryExpr::UnaryVal(Atom::Ident(name.to_string()));
        Ok(CallExpr::DeclCall(
            Box::new(self.wrap_unary(target)),
            params,
        ))
    }

    fn parse_call_param(&mut self) -> ParseResult<CallParam> {
        if matches!(self.peek().kind, TokenKind::Tilde) {
            self.advance();
            let name = match self.consume().kind {
                TokenKind::Ident(s) => s,
                _ => return Err(self.fail("Expected identifier for named parameter")),
            };
            self.expect(TokenKind::Eql)?;
            let expr = Box::new(self.parse_expression()?);
            Ok(CallParam::Named(name, expr))
        } else {
            let expr = Box::new(self.parse_expression()?);
            Ok(CallParam::Positional(expr))
        }
    }

    fn parse_atom(&mut self) -> ParseResult<Atom> {
        match self.consume().kind {
            TokenKind::Number(n) => Ok(Atom::Int(n)),
            TokenKind::String(s) => Ok(Atom::String(s)),
            TokenKind::Char(c) => Ok(Atom::Char(c)),
            TokenKind::True => Ok(Atom::Bool(true)),
            TokenKind::False => Ok(Atom::Bool(false)),
            TokenKind::LowDash => Ok(Atom::Ident("_".to_string())),
            TokenKind::OpenParen => {
                if matches!(self.peek().kind, TokenKind::CloseParen) {
                    self.advance();
                    Ok(Atom::UnitVal)
                } else {
                    let expr = self.parse_expression()?;
                    self.expect(TokenKind::CloseParen)?;
                    Ok(Atom::Grouping(Box::new(expr)))
                }
            }
            t => Err(self.fail(format!("Unexpected token {:?} at primary parse", t))),
        }
    }

    fn parse_list<T, F>(&mut self, untils: &[TokenKind], f: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser) -> ParseResult<T>,
    {
        let mut acc = Vec::new();
        while !self.is_at_end() && !untils.contains(&self.peek().kind) {
            let res = f(self)?;
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.advance();
            }
            acc.push(res);
        }
        Ok(acc)
    }

    fn wrap_unary(&self, u: UnaryExpr) -> Expression {
        Expression::RelationalExpr(RelationalExpr::RelationalVal(Box::new(
            AdditiveExpr::AdditiveVal(Box::new(MultiplicativeExpr::MultiplicativeVal(Box::new(u)))),
        )))
    }

    /* basic cursor helpers */
    fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token {
            kind: TokenKind::Eof,
            span: Span {
                file: String::new(),
                line: 0,
                column: 0,
            },
        })
    }

    fn consume(&mut self) -> Token {
        let token = self.peek();
        self.advance();
        token
    }

    fn advance(&mut self) {
        self.pos = self.pos.saturating_add(1);
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || matches!(self.peek().kind, TokenKind::Eof)
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.peek().kind == kind {
            self.advance();
            Ok(())
        } else {
            Err(self.fail(format!("Expected {:?}, got {:?}", kind, self.peek().kind)))
        }
    }

    fn fail(&self, msg: impl Into<String>) -> ParseError {
        ParseError::new(msg, self.peek().span.clone())
    }
}
