#![allow(dead_code, unused_imports)]
use crate::lexer::{
    Lexer,
    token::{Token, TokenKind},
};
use ast::*;
use miette::{Error, LabeledSpan};
use std::{borrow::Cow, fmt};
use tracing::{instrument, trace};

pub mod ast;

pub struct Parser<'de> {
    pub whole: &'de str,
    pub lexer: Lexer<'de>,
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parser")
    }
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    #[instrument]
    pub fn parse(mut self) -> Result<Program<'de>, Error> {
        trace!("Entering");
        let mut items = Vec::new();
        // Keep parsing while we have tokens and can parse a top-level item
        // Top-level items start with attributes (optional) or 'let'
        while self.peek_kind().is_some() {
            // Check if we can parse a top-level item (starts with attributes or 'let')
            if self.check(TokenKind::Pound) || self.check(TokenKind::Let) {
                items.push(self.parse_top_level_item()?);
            } else {
                // No more top-level items to parse
                break;
            }
        }
        Ok(Program { items })
    }

    // Helper methods for token handling

    fn peek_kind(&mut self) -> Option<TokenKind> {
        self.lexer
            .peek()
            .and_then(|r| r.as_ref().ok())
            .map(|t| t.kind)
    }

    fn peek_token(&mut self) -> Option<&Token<'de>> {
        self.lexer.peek().and_then(|r| r.as_ref().ok())
    }

    fn next_token(&mut self) -> Option<Result<Token<'de>, Error>> {
        self.lexer.next()
    }

    fn expect_token(&mut self, kind: TokenKind, expected: &str) -> Result<Token<'de>, Error> {
        match self.next_token() {
            Some(Ok(token)) if token.kind == kind => Ok(token),
            Some(Ok(token)) => Err(self.error_at_token(
                &token,
                &format!("Expected {}, found {:?}", expected, token.kind),
            )),
            Some(Err(e)) => Err(e),
            None => Err(self.error_eof(expected)),
        }
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        self.peek_kind() == Some(kind)
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn error_at_token(&self, token: &Token<'de>, msg: &str) -> Error {
        miette::miette! {
            labels = vec![
                LabeledSpan::at(token.offset..token.offset + token.origin.len(), msg),
            ],
            "{}", msg
        }
        .with_source_code(self.whole.to_string())
    }

    fn error_eof(&self, expected: &str) -> Error {
        miette::miette! {
            labels = vec![
                LabeledSpan::at(self.whole.len()..self.whole.len(), "unexpected end of file"),
            ],
            "Expected {}, but reached end of file", expected
        }
        .with_source_code(self.whole.to_string())
    }

    // Top-level parsing

    #[instrument]
    fn parse_top_level_item(&mut self) -> Result<TopLevelItem<'de>, Error> {
        trace!("Entering");
        // Parse attributes (optional)
        let attrs = self.parse_attrs()?;

        if !self.check(TokenKind::Let) {
            return Err(self.error_eof("let"));
        }

        self.expect_token(TokenKind::Let, "let")?;
        let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
        let name = Cow::Borrowed(name_token.origin);

        // Check if it's a function (has ::) - check this first before consuming :
        if self.check(TokenKind::DoubleColon) {
            return Ok(TopLevelItem::Function(self.parse_function(name)?));
        }

        // Check if it's a type declaration (struct or enum)
        if self.check(TokenKind::Colon) {
            self.next_token(); // consume :
            if self.check(TokenKind::Equal) {
                self.next_token(); // consume =
                // This is `let Name := ...` - type declaration
                if self.check(TokenKind::Struct) {
                    let mut type_decl = self.parse_type_decl(name)?;
                    type_decl.attrs = attrs;
                    return Ok(TopLevelItem::TypeDecl(type_decl));
                } else if self.check(TokenKind::Enum) {
                    let mut type_decl = self.parse_type_decl(name)?;
                    type_decl.attrs = attrs;
                    return Ok(TopLevelItem::TypeDecl(type_decl));
                }
            }
        }

        // Otherwise it's a variable declaration
        // We already consumed `let` and the name, need to check for type annotation
        let type_annotation = if self.check(TokenKind::Colon) {
            self.next_token(); // consume :
            if self.check(TokenKind::Equal) {
                // This is `:=` (type inference)
                self.next_token(); // consume =
                None
            } else {
                // This is `: Type =` (explicit type)
                Some(self.parse_type()?)
            }
        } else {
            None
        };

        if type_annotation.is_some() {
            self.expect_token(TokenKind::Equal, "=")?;
        }

        let value = Box::new(self.parse_expr()?);
        Ok(TopLevelItem::VariableDecl(VariableDecl {
            name,
            type_annotation,
            value,
        }))
    }

    // Function parsing

    #[instrument]
    fn parse_function(&mut self, name: Cow<'de, str>) -> Result<Function<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::DoubleColon, "::")?;

        // Parse generics (optional)
        let generics = if self.check(TokenKind::LessThan) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        // Check for empty parameter list: ()
        let params = if self.check(TokenKind::LeftParen) {
            self.next_token(); // consume (
            if self.consume(TokenKind::RightParen) {
                // Empty parameter list: ()
                Vec::new()
            } else {
                // Not empty, restore by parsing as normal parameters
                // Actually, we've already consumed the (, so we need to handle this differently
                // Let's check if the next token is an identifier or if it's something else
                // For now, let's just parse it as a tuple type and convert
                // Actually, that's wrong. Let me think...
                // If we see ( after ::, it could be:
                // 1. Empty params: ()
                // 2. A tuple type as a parameter? No, that doesn't make sense
                // 3. Actually, parameters are just identifiers, not types
                // So if we see ( and it's not immediately ), we have an error
                return Err(self.error_eof("empty parameter list () or parameter name"));
            }
        } else {
            // Parse parameters normally
            self.parse_function_params()?
        };

        // Parse return type (optional)
        let return_type = if self.consume(TokenKind::RArrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_token(TokenKind::Equal, "=")?;

        // Parse body
        let body = self.parse_expr_list()?;

        Ok(Function {
            name,
            generics,
            params,
            return_type,
            body,
        })
    }

    #[instrument]
    fn parse_generic_params(&mut self) -> Result<Vec<Cow<'de, str>>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LessThan, "<")?;
        let mut params = Vec::new();

        loop {
            let token = self.expect_token(TokenKind::Ident, "identifier")?;
            params.push(Cow::Borrowed(token.origin));

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::GrtrThan, ">")?;
        Ok(params)
    }

    #[instrument]
    fn parse_function_params(&mut self) -> Result<Vec<FunctionParam<'de>>, Error> {
        trace!("Entering");
        let mut params = Vec::new();

        // Parse parameters until we hit = or ->
        // Check if we have any parameters at all
        if self.check(TokenKind::Equal) || self.check(TokenKind::RArrow) {
            return Ok(params);
        }

        // Check for empty tuple parameter list: ()
        if self.check(TokenKind::LeftParen) {
            let start_offset = self.peek_token().map(|t| t.offset).unwrap_or(0);
            self.next_token(); // consume (
            if self.consume(TokenKind::RightParen) {
                // Empty parameter list: ()
                return Ok(params);
            }
            // Not an empty tuple, restore and continue
            // We can't easily restore, so we'll handle this differently
            // Actually, we've already consumed the (, so we need to parse it as a parameter
            // But that doesn't make sense. Let me think...
            // Actually, () in function params means no parameters, which we already handled above
            // So if we see ( and it's not immediately followed by ), we have an error
            return Err(self.error_at_token(
                &Token {
                    origin: "(",
                    offset: start_offset,
                    kind: TokenKind::LeftParen,
                },
                "Unexpected ( in parameter list",
            ));
        }

        // Parse first parameter (required if we get here)
        loop {
            let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
            let name = Cow::Borrowed(name_token.origin);

            let type_annotation = if self.consume(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(FunctionParam {
                name,
                type_annotation,
            });

            // Check if there's a comma - if so, there's another parameter
            // Otherwise, we're done
            if self.check(TokenKind::Comma) {
                self.next_token(); // consume comma
            // Continue to parse next parameter
            } else {
                // No comma means end of parameter list
                break;
            }
        }

        Ok(params)
    }

    // Type parsing

    #[instrument]
    fn parse_type_decl(&mut self, name: Cow<'de, str>) -> Result<TypeDecl<'de>, Error> {
        trace!("Entering");
        // Attributes are parsed before we get here (in parse_top_level_item)
        // We've already consumed `let Name :=`, now we need to parse struct or enum
        let attrs = Vec::new(); // Attributes are set by the caller

        let decl = if self.consume(TokenKind::Struct) {
            TypeDeclKind::Struct(self.parse_struct_decl()?)
        } else if self.consume(TokenKind::Enum) {
            TypeDeclKind::Enum(self.parse_enum_decl()?)
        } else {
            return Err(self.error_eof("struct or enum"));
        };

        Ok(TypeDecl { attrs, name, decl })
    }

    #[instrument]
    fn parse_attrs(&mut self) -> Result<Vec<Attr<'de>>, Error> {
        trace!("Entering");
        let mut attrs = Vec::new();
        while self.check(TokenKind::Pound) {
            attrs.push(self.parse_attr()?);
        }
        Ok(attrs)
    }

    #[instrument]
    fn parse_attr(&mut self) -> Result<Attr<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Pound, "#")?;
        self.expect_token(TokenKind::LeftSquare, "[")?;

        let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
        let name = Cow::Borrowed(name_token.origin);

        let value = if self.consume(TokenKind::Colon) {
            self.expect_token(TokenKind::Equal, "=")?;
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        self.expect_token(TokenKind::RightSquare, "]")?;
        Ok(Attr { name, value })
    }

    #[instrument]
    fn parse_struct_decl(&mut self) -> Result<StructDecl<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftBrace, "{")?;
        let mut fields = Vec::new();

        while !self.check(TokenKind::RightBrace) {
            let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
            let name = Cow::Borrowed(name_token.origin);
            self.expect_token(TokenKind::Colon, ":")?;
            let type_ = self.parse_type()?;
            fields.push(StructField { name, type_ });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::RightBrace, "}")?;

        let impl_block = if self.consume(TokenKind::With) {
            self.expect_token(TokenKind::LeftBrace, "{")?;
            let mut functions = Vec::new();
            while !self.check(TokenKind::RightBrace) {
                functions.push(self.parse_function_in_impl()?);
            }
            self.expect_token(TokenKind::RightBrace, "}")?;
            Some(functions)
        } else {
            None
        };

        Ok(StructDecl { fields, impl_block })
    }

    #[instrument]
    fn parse_function_in_impl(&mut self) -> Result<Function<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Let, "let")?;
        let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
        let name = Cow::Borrowed(name_token.origin);
        self.parse_function(name)
    }

    #[instrument]
    fn parse_enum_decl(&mut self) -> Result<EnumDecl<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftBrace, "{")?;
        let mut variants = Vec::new();

        while !self.check(TokenKind::RightBrace) {
            let attrs = self.parse_attrs()?;
            let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
            let name = Cow::Borrowed(name_token.origin);

            let type_ = if self.consume(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            variants.push(EnumVariant { attrs, name, type_ });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::RightBrace, "}")?;
        Ok(EnumDecl { variants })
    }

    #[instrument]
    fn parse_type(&mut self) -> Result<Type<'de>, Error> {
        trace!("Entering"); // Check for function type first (|Type1, Type2|)
        if self.check(TokenKind::Bar) {
            return Ok(self.parse_function_type()?);
        }

        // Check for inline struct type: struct { ... }
        if self.check(TokenKind::Struct) {
            return self.parse_inline_struct_type();
        }

        // Check for tuple type (Type1, Type2, ...)
        if self.check(TokenKind::LeftParen) {
            self.next_token(); // consume (
            let mut types = Vec::new();
            loop {
                types.push(self.parse_type()?);
                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_token(TokenKind::RightParen, ")")?;
            return Ok(Type::Tuple(types));
        }

        // Parse base type
        let base = if let Some(token) = self.peek_token() {
            match token.kind {
                TokenKind::Ident => {
                    let name_token = self.next_token().unwrap().unwrap();
                    let name = Cow::Borrowed(name_token.origin);
                    Type::Named(name)
                }
                _ => {
                    // Try primitive
                    let prim = self.parse_primitive_type()?;
                    Type::Primitive(prim)
                }
            }
        } else {
            return Err(self.error_eof("type"));
        };

        // Check for generics
        if self.check(TokenKind::LessThan) {
            self.next_token(); // consume <
            let mut args = Vec::new();
            loop {
                args.push(self.parse_type()?);
                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_token(TokenKind::GrtrThan, ">")?;
            Ok(Type::Generic {
                base: Box::new(base),
                args,
            })
        } else {
            Ok(base)
        }
    }

    #[instrument]
    fn parse_inline_struct_type(&mut self) -> Result<Type<'de>, Error> {
        trace!("Entering");
        // Parse inline struct type: struct { field: Type, ... }
        self.expect_token(TokenKind::Struct, "struct")?;
        self.expect_token(TokenKind::LeftBrace, "{")?;
        let mut fields = Vec::new();

        while !self.check(TokenKind::RightBrace) {
            let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
            let name = Cow::Borrowed(name_token.origin);
            self.expect_token(TokenKind::Colon, ":")?;
            let type_ = self.parse_type()?;
            fields.push(StructField { name, type_ });

            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::RightBrace, "}")?;
        Ok(Type::AnonymousStruct(fields))
    }

    #[instrument]
    fn parse_primitive_type(&mut self) -> Result<PrimitiveType, Error> {
        trace!("Entering");
        let token = self.expect_token(TokenKind::Ident, "primitive type")?;
        match token.origin {
            "i8" => Ok(PrimitiveType::I8),
            "i16" => Ok(PrimitiveType::I16),
            "i32" => Ok(PrimitiveType::I32),
            "i64" => Ok(PrimitiveType::I64),
            "isize" => Ok(PrimitiveType::Isize),
            "u8" => Ok(PrimitiveType::U8),
            "u16" => Ok(PrimitiveType::U16),
            "u32" => Ok(PrimitiveType::U32),
            "u64" => Ok(PrimitiveType::U64),
            "usize" => Ok(PrimitiveType::Usize),
            "str" => Ok(PrimitiveType::Str),
            "char" => Ok(PrimitiveType::Char),
            "nil" => Ok(PrimitiveType::Nil),
            "list" => Ok(PrimitiveType::List),
            _ => Err(self.error_at_token(&token, "Expected primitive type")),
        }
    }

    #[instrument]
    fn parse_function_type(&mut self) -> Result<Type<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Bar, "|")?;
        let mut params = Vec::new();

        while !self.check(TokenKind::Bar) {
            params.push(self.parse_type()?);
            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::Bar, "|")?;
        Ok(Type::Function { params })
    }

    // Expression parsing

    #[instrument]
    fn parse_expr_list(&mut self) -> Result<ExprList<'de>, Error> {
        trace!("Entering");
        if self.check(TokenKind::LeftBrace) {
            Ok(ExprList::Block(self.parse_block()?))
        } else {
            Ok(ExprList::Single(Box::new(self.parse_expr()?)))
        }
    }

    #[instrument]
    fn parse_block(&mut self) -> Result<Vec<Stmt<'de>>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftBrace, "{")?;
        let mut stmts = Vec::new();

        while !self.check(TokenKind::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }

        self.expect_token(TokenKind::RightBrace, "}")?;
        Ok(stmts)
    }

    #[instrument]
    fn parse_stmt(&mut self) -> Result<Stmt<'de>, Error> {
        trace!("Entering");
        if self.consume(TokenKind::Return) {
            let expr = if !self.check(TokenKind::SemiColon)
                && !self.check(TokenKind::RightBrace)
                && self.peek_kind().is_some()
            {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            return Ok(Stmt::Return(expr));
        }

        if self.check(TokenKind::Let) {
            return Ok(Stmt::VariableDecl(self.parse_variable_decl()?));
        }

        Ok(Stmt::Expr(Box::new(self.parse_expr()?)))
    }

    #[instrument]
    fn parse_variable_decl(&mut self) -> Result<VariableDecl<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Let, "let")?;
        let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
        let name = Cow::Borrowed(name_token.origin);

        let type_annotation = if self.consume(TokenKind::Colon) {
            if self.check(TokenKind::Equal) {
                // := (type inference)
                self.next_token(); // consume =
                None
            } else {
                // : Type = (explicit type)
                let type_ = self.parse_type()?;
                self.expect_token(TokenKind::Equal, "=")?;
                Some(type_)
            }
        } else {
            None
        };

        let value = Box::new(self.parse_expr()?);
        Ok(VariableDecl {
            name,
            type_annotation,
            value,
        })
    }

    #[instrument]
    fn parse_expr(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        self.parse_expr_binary(0)
    }

    // Operator precedence levels:
    // 0: or
    // 1: and
    // 2: comparison (==, !=, <, >, <=, >=)
    // 3: additive (+, -, <>)
    // 4: multiplicative (*, /, %)
    // 5: exponentiation (^)
    // 6: unary (-, !)

    #[instrument]
    fn parse_expr_binary(&mut self, min_prec: u8) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        let mut left = self.parse_expr_unary()?;

        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::Or) if min_prec <= 0 => BinOp::Or,
                Some(TokenKind::And) if min_prec <= 1 => BinOp::And,
                Some(TokenKind::DoubleEqual) if min_prec <= 2 => BinOp::Eq,
                Some(TokenKind::BangEqual) if min_prec <= 2 => BinOp::Ne,
                Some(TokenKind::LessThan) if min_prec <= 2 => BinOp::Lt,
                Some(TokenKind::GrtrThan) if min_prec <= 2 => BinOp::Gt,
                Some(TokenKind::LessEqual) if min_prec <= 2 => BinOp::Le,
                Some(TokenKind::GrtrEqual) if min_prec <= 2 => BinOp::Ge,
                Some(TokenKind::Concat) if min_prec <= 3 => BinOp::Concat,
                Some(TokenKind::Plus) if min_prec <= 3 => BinOp::Add,
                Some(TokenKind::Minus) if min_prec <= 3 => BinOp::Sub,
                Some(TokenKind::Star) if min_prec <= 4 => BinOp::Mul,
                Some(TokenKind::Slash) if min_prec <= 4 => BinOp::Div,
                Some(TokenKind::Perc) if min_prec <= 4 => BinOp::Rem,
                Some(TokenKind::Carrot) if min_prec <= 5 => BinOp::Pow,
                _ => break,
            };

            let prec = match op {
                BinOp::Or | BinOp::And => 0,
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => 2,
                BinOp::Concat | BinOp::Add | BinOp::Sub => 3,
                BinOp::Mul | BinOp::Div | BinOp::Rem => 4,
                BinOp::Pow => 5,
            };

            self.next_token(); // consume operator
            let right = self.parse_expr_binary(prec + 1)?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    #[instrument]
    fn parse_expr_unary(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        if self.consume(TokenKind::Minus) {
            Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(self.parse_expr_unary()?),
            })
        } else if self.consume(TokenKind::Bang) {
            Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(self.parse_expr_unary()?),
            })
        } else {
            self.parse_expr_primary()
        }
    }

    #[instrument]
    fn parse_expr_primary(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        match self.peek_kind() {
            Some(TokenKind::NumberLit(_)) => {
                let token = self.next_token().unwrap().unwrap();
                if let TokenKind::NumberLit(n) = token.kind {
                    Ok(Expr::Literal(Literal::Number(n)))
                } else {
                    unreachable!()
                }
            }
            Some(TokenKind::StringLit) => {
                let token = self.next_token().unwrap().unwrap();
                Ok(Expr::Literal(Literal::String(Token::unescape(
                    token.origin,
                ))))
            }
            Some(TokenKind::CharLit(c)) => {
                self.next_token();
                Ok(Expr::Literal(Literal::Char(c)))
            }
            Some(TokenKind::True) => {
                self.next_token();
                Ok(Expr::Literal(Literal::Boolean(true)))
            }
            Some(TokenKind::False) => {
                self.next_token();
                Ok(Expr::Literal(Literal::Boolean(false)))
            }
            Some(TokenKind::Nil) => {
                self.next_token();
                Ok(Expr::Literal(Literal::Nil))
            }
            Some(TokenKind::At) => {
                self.next_token(); // consume @
                let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
                let name = Cow::Borrowed(name_token.origin);
                let args = if self.check(TokenKind::LeftParen) {
                    Some(self.parse_arg_list()?)
                } else {
                    None
                };
                Ok(Expr::Literal(Literal::BuiltinCall(BuiltinCall {
                    name,
                    args,
                })))
            }
            Some(TokenKind::Bar) => {
                // Anonymous function
                self.parse_anon_fn()
            }
            Some(TokenKind::Ident) => {
                let token = self.peek_token().unwrap();
                // Check if it's "match"
                if token.origin == "match" {
                    self.parse_match()
                } else {
                    self.parse_expr_ident()
                }
            }
            Some(TokenKind::If) => self.parse_if(),
            Some(TokenKind::LeftParen) => {
                self.next_token(); // consume (
                // Check if it's a tuple
                let expr = self.parse_expr()?;
                if self.consume(TokenKind::Comma) {
                    // It's a tuple
                    let mut exprs = vec![expr];
                    loop {
                        exprs.push(self.parse_expr()?);
                        if !self.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect_token(TokenKind::RightParen, ")")?;
                    Ok(Expr::Tuple(exprs))
                } else {
                    self.expect_token(TokenKind::RightParen, ")")?;
                    Ok(Expr::Paren(Box::new(expr)))
                }
            }
            Some(TokenKind::LeftSquare) => {
                self.next_token(); // consume [
                if self.consume(TokenKind::RightSquare) {
                    Ok(Expr::List(Vec::new()))
                } else {
                    let mut exprs = Vec::new();
                    loop {
                        exprs.push(self.parse_expr()?);
                        if !self.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect_token(TokenKind::RightSquare, "]")?;
                    Ok(Expr::List(exprs))
                }
            }
            Some(TokenKind::Dot) => {
                // Struct literal or enum variant
                self.next_token(); // consume .
                if self.check(TokenKind::LeftBrace) {
                    // Struct literal
                    self.expect_token(TokenKind::LeftBrace, "{")?;
                    let mut fields = Vec::new();
                    while !self.check(TokenKind::RightBrace) {
                        let name_token = if self.consume(TokenKind::Dot) {
                            self.expect_token(TokenKind::Ident, "identifier")?
                        } else {
                            self.expect_token(TokenKind::Ident, "identifier")?
                        };
                        let name = Cow::Borrowed(name_token.origin);
                        self.expect_token(TokenKind::Equal, "=")?;
                        let value = Box::new(self.parse_expr()?);
                        fields.push(StructFieldInit { name, value });
                        if !self.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect_token(TokenKind::RightBrace, "}")?;
                    Ok(Expr::StructLit {
                        type_: None,
                        fields,
                    })
                } else {
                    // Enum variant
                    let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
                    let name = Cow::Borrowed(name_token.origin);
                    let value = if self.check(TokenKind::LeftParen) {
                        Some(self.parse_enum_variant_value()?)
                    } else {
                        None
                    };
                    Ok(Expr::EnumVariant { name, value })
                }
            }
            _ => Err(self.error_eof("expression")),
        }
    }

    #[instrument]
    fn parse_expr_ident(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        let token = self.next_token().unwrap().unwrap();
        let name = Cow::Borrowed(token.origin);
        let mut expr: Expr<'de> = Expr::Ident(name);

        // Handle method calls, function calls, and member access
        loop {
            match self.peek_kind() {
                Some(TokenKind::Colon) => {
                    // Method call: expr:method(args)
                    self.next_token(); // consume :
                    let method_token = self.expect_token(TokenKind::Ident, "method name")?;
                    let method = Cow::Borrowed(method_token.origin);
                    self.expect_token(TokenKind::LeftParen, "(")?;
                    let args = self.parse_arg_list()?;
                    self.expect_token(TokenKind::RightParen, ")")?;
                    expr = Expr::MethodCall {
                        receiver: Box::new(expr),
                        method,
                        args,
                    };
                }
                Some(TokenKind::LeftParen) => {
                    // Function call
                    self.next_token(); // consume (
                    let args = self.parse_arg_list()?;
                    self.expect_token(TokenKind::RightParen, ")")?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args,
                    };
                }
                Some(TokenKind::Dot) => {
                    // Member access
                    self.next_token(); // consume .
                    if let Some(token) = self.peek_token() {
                        if let TokenKind::NumberLit(n) = token.kind {
                            self.next_token(); // consume number
                            expr = Expr::Member {
                                object: Box::new(expr),
                                field: MemberField::Index(n),
                            };
                        } else {
                            let field_token = self.expect_token(TokenKind::Ident, "field name")?;
                            expr = Expr::Member {
                                object: Box::new(expr),
                                field: MemberField::Name(Cow::Borrowed(field_token.origin)),
                            };
                        }
                    } else {
                        return Err(self.error_eof("field name or index"));
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    #[instrument]
    fn parse_arg_list(&mut self) -> Result<Vec<Expr<'de>>, Error> {
        trace!("Entering");
        let mut args = Vec::new();
        while !self.check(TokenKind::RightParen) {
            args.push(self.parse_expr()?);
            if !self.consume(TokenKind::Comma) {
                break;
            }
        }
        Ok(args)
    }

    #[instrument]
    fn parse_enum_variant_value(&mut self) -> Result<EnumVariantValue<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftParen, "(")?;
        let first = self.parse_expr()?;
        if self.consume(TokenKind::Comma) {
            // Tuple variant
            let mut exprs = vec![first];
            loop {
                exprs.push(self.parse_expr()?);
                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_token(TokenKind::RightParen, ")")?;
            Ok(EnumVariantValue::Tuple(exprs))
        } else {
            self.expect_token(TokenKind::RightParen, ")")?;
            Ok(EnumVariantValue::Single(Box::new(first)))
        }
    }

    #[instrument]
    fn parse_anon_fn(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Bar, "|")?;
        let mut params = Vec::new();

        while !self.check(TokenKind::Bar) {
            let param_token = self.expect_token(TokenKind::Ident, "parameter")?;
            params.push(Cow::Borrowed(param_token.origin));
            if !self.consume(TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::Bar, "|")?;
        let body = self.parse_expr_list()?;
        Ok(Expr::AnonFn { params, body })
    }

    #[instrument]
    fn parse_match(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering"); // "match" is parsed as an identifier
        self.next_token(); // consume "match"
        let expr = Box::new(self.parse_expr()?);
        self.expect_token(TokenKind::LeftBrace, "{")?;

        let mut arms = Vec::new();
        while !self.check(TokenKind::RightBrace) {
            arms.push(self.parse_match_arm()?);
        }

        self.expect_token(TokenKind::RightBrace, "}")?;
        Ok(Expr::Match { expr, arms })
    }

    #[instrument]
    fn parse_match_arm(&mut self) -> Result<MatchArm<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::Bar, "|")?;
        let mut patterns = Vec::new();

        loop {
            patterns.push(self.parse_pattern()?);
            if self.consume(TokenKind::Comma) {
                continue;
            }
            break;
        }

        let guard = if self.consume(TokenKind::If) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        self.expect_token(TokenKind::RArrow, "->")?;
        let body = Box::new(self.parse_expr()?);
        self.consume(TokenKind::Comma); // optional trailing comma

        Ok(MatchArm {
            patterns,
            guard,
            body,
        })
    }

    #[instrument]
    fn parse_if(&mut self) -> Result<Expr<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::If, "if")?;
        let condition = Box::new(self.parse_expr()?);
        let then_block = self.parse_block()?;

        let mut elif_blocks = Vec::new();
        while self.consume(TokenKind::Elif) {
            let elif_condition = Box::new(self.parse_expr()?);
            let elif_body = self.parse_block()?;
            elif_blocks.push(ElifBlock {
                condition: elif_condition,
                body: elif_body,
            });
        }

        let else_block = if self.consume(TokenKind::Else) {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Expr::If {
            condition,
            then_block,
            elif_blocks,
            else_block,
        })
    }

    // Pattern parsing

    #[instrument]
    fn parse_pattern(&mut self) -> Result<Pattern<'de>, Error> {
        trace!("Entering");
        match self.peek_kind() {
            Some(TokenKind::NumberLit(_)) => {
                let token = self.next_token().unwrap()?;
                if let TokenKind::NumberLit(n) = token.kind {
                    Ok(Pattern::Literal(Literal::Number(n)))
                } else {
                    unreachable!()
                }
            }
            Some(TokenKind::StringLit) => {
                let token = self.next_token().unwrap()?;
                Ok(Pattern::Literal(Literal::String(Token::unescape(
                    token.origin,
                ))))
            }
            Some(TokenKind::CharLit(c)) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Char(c)))
            }
            Some(TokenKind::True) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Boolean(true)))
            }
            Some(TokenKind::False) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Boolean(false)))
            }
            Some(TokenKind::Nil) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Nil))
            }
            Some(TokenKind::Ident) => {
                let token = self.next_token().unwrap()?;
                // Check for string pattern: ident :: char :: ident
                if self
                    .peek_kind()
                    .is_some_and(|t| t == TokenKind::DoubleColon)
                {
                    // self.check(TokenKind::DoubleColon) {
                    // This is a string pattern
                    self.next_token(); // consume ::
                    // Parse char - can be CharLit or StringLit
                    let delimiter_result = match self.peek_kind() {
                        Some(TokenKind::CharLit(c)) => {
                            self.next_token();
                            Ok(c)
                        }
                        _ => {
                            if self.check(TokenKind::StringLit) {
                                let char_token = self.next_token().unwrap().unwrap();
                                let char_str = Token::unescape(char_token.origin);
                                Ok(char_str.chars().next().unwrap_or(' '))
                            } else {
                                Err(self.error_eof("character literal"))
                            }
                        }
                    };
                    let delimiter = delimiter_result?;
                    self.expect_token(TokenKind::DoubleColon, "::")?;
                    let tail_token = self.expect_token(TokenKind::Ident, "identifier")?;
                    Ok(Pattern::Str(StrPattern::Segments {
                        head: Cow::Borrowed(token.origin),
                        delimiter,
                        tail: Cow::Borrowed(tail_token.origin),
                    }))
                } else {
                    // Regular identifier pattern
                    Ok(Pattern::Ident(Cow::Borrowed(token.origin)))
                }
            }
            Some(TokenKind::LeftSquare) => self.parse_list_pattern(),
            Some(TokenKind::LeftParen) => {
                self.next_token(); // consume (
                let mut patterns = Vec::new();
                loop {
                    patterns.push(self.parse_pattern()?);
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_token(TokenKind::RightParen, ")")?;
                Ok(Pattern::Tuple(patterns))
            }
            Some(TokenKind::Dot) => {
                // Enum pattern or struct pattern
                self.next_token(); // consume .
                if self.check(TokenKind::LeftBrace) {
                    // Struct pattern
                    self.expect_token(TokenKind::LeftBrace, "{")?;
                    let mut fields = Vec::new();
                    while !self.check(TokenKind::RightBrace) {
                        let name_token = if self.consume(TokenKind::Dot) {
                            self.expect_token(TokenKind::Ident, "identifier")?
                        } else {
                            self.expect_token(TokenKind::Ident, "identifier")?
                        };
                        let name = Cow::Borrowed(name_token.origin);

                        if self.consume(TokenKind::Equal) {
                            // Named field: .name = pattern
                            let pattern = self.parse_pattern()?;
                            fields.push(StructFieldPat::Named {
                                name,
                                pattern: Box::new(pattern),
                            });
                        } else {
                            // Shorthand: .name or name
                            fields.push(StructFieldPat::Shorthand(name));
                        }

                        if !self.consume(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect_token(TokenKind::RightBrace, "}")?;
                    Ok(Pattern::Struct(StructPattern {
                        type_: None,
                        fields,
                    }))
                } else {
                    // Enum pattern
                    let name_token = self.expect_token(TokenKind::Ident, "identifier")?;
                    let name = Cow::Borrowed(name_token.origin);
                    let value = if self.check(TokenKind::LeftParen) {
                        Some(self.parse_enum_pattern_value()?)
                    } else {
                        None
                    };
                    Ok(Pattern::Enum(EnumPattern { name, value }))
                }
            }
            _ => {
                // Try wildcard
                if self.peek_token().map(|t| t.origin == "_").unwrap_or(false) {
                    self.next_token();
                    Ok(Pattern::Wildcard)
                } else {
                    Err(self.error_eof("pattern"))
                }
            }
        }
    }

    #[instrument]
    fn parse_list_pattern(&mut self) -> Result<Pattern<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftSquare, "[")?;

        if self.consume(TokenKind::RightSquare) {
            return Ok(Pattern::List(ListPattern::Empty));
        }

        // Check for [.., tail] or [..tail]
        if self.consume(TokenKind::Spread) {
            if self.consume(TokenKind::Comma) {
                // .., tail
                let tail = self.parse_pattern()?;
                self.expect_token(TokenKind::RightSquare, "]")?;
                return Ok(Pattern::List(ListPattern::RestTail {
                    tail: Box::new(tail),
                }));
            }

            // ..tail (no comma)
            if self.check(TokenKind::RightSquare) {
                // unexpected: [..] alone -> treat as empty rest
                self.next_token();
                return Ok(Pattern::List(ListPattern::RestTail {
                    tail: Box::new(Pattern::List(ListPattern::Empty as _)),
                }));
            } else {
                let tail = self.parse_pattern()?;
                self.expect_token(TokenKind::RightSquare, "]")?;
                return Ok(Pattern::List(ListPattern::RestTail {
                    tail: Box::new(tail),
                }));
            }
        }

        // Parse first pattern
        let first = self.parse_pattern()?;

        if self.consume(TokenKind::Comma) {
            // Check for .. pattern
            if self.consume(TokenKind::Spread) {
                // [head, ..] or [head, .., tail]
                if self.check(TokenKind::RightSquare) {
                    // [head, ..]
                    self.next_token(); // consume RightSquare
                    Ok(Pattern::List(ListPattern::HeadRest {
                        head: Box::new(first),
                    }))
                } else if self.consume(TokenKind::Comma) {
                    // [head, .., tail]
                    let tail = self.parse_pattern()?;
                    self.expect_token(TokenKind::RightSquare, "]")?;
                    Ok(Pattern::List(ListPattern::HeadRestTail {
                        head: Box::new(first),
                        tail: Box::new(tail),
                    }))
                } else {
                    // [head, ..tail] (no comma)
                    let tail = self.parse_pattern()?;
                    self.expect_token(TokenKind::RightSquare, "]")?;
                    Ok(Pattern::List(ListPattern::HeadRestTail {
                        head: Box::new(first),
                        tail: Box::new(tail),
                    }))
                }
            } else {
                // Regular list [pat1, pat2, ...]
                let mut patterns = vec![first];
                loop {
                    patterns.push(self.parse_pattern()?);
                    if !self.consume(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_token(TokenKind::RightSquare, "]")?;
                Ok(Pattern::List(ListPattern::Exact(patterns)))
            }
        } else {
            // Single element [pat]
            self.expect_token(TokenKind::RightSquare, "]")?;
            Ok(Pattern::List(ListPattern::Single(Box::new(first))))
        }
    }

    #[instrument]
    fn parse_enum_pattern_value(&mut self) -> Result<EnumPatternValue<'de>, Error> {
        trace!("Entering");
        self.expect_token(TokenKind::LeftParen, "(")?;
        let first = self.parse_pattern()?;
        if self.consume(TokenKind::Comma) {
            // Tuple pattern
            let mut patterns = vec![first];
            loop {
                patterns.push(self.parse_pattern()?);
                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_token(TokenKind::RightParen, ")")?;
            Ok(EnumPatternValue::Tuple(patterns))
        } else {
            self.expect_token(TokenKind::RightParen, ")")?;
            Ok(EnumPatternValue::Single(Box::new(first)))
        }
    }
}
