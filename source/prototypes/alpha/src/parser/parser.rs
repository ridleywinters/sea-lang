use std::path::PathBuf;

use crate::parser::ast::*;
use crate::parser::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    file: PathBuf,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: PathBuf) -> Self {
        Parser {
            tokens,
            pos: 0,
            file,
        }
    }

    fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        let token = self.current();
        if std::mem::discriminant(&token.kind) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(format!("Expected {:?}, found {:?}", expected, token.kind)))
        }
    }

    fn error(&self, message: String) -> ParseError {
        let token = self.current();
        ParseError {
            message,
            location: Some(SourceLocation {
                file: self.file.clone(),
                line: token.line,
                column: token.column,
            }),
        }
    }

    pub fn parse_file(&mut self) -> Result<(FileAST, Vec<FunctionDef>), ParseError> {
        let mut functions = Vec::new();

        while self.current().kind != TokenKind::Eof {
            functions.push(self.parse_function()?);
        }

        Ok((
            FileAST {
                path: self.file.clone(),
            },
            functions,
        ))
    }

    fn parse_function(&mut self) -> Result<FunctionDef, ParseError> {
        let is_exported = if self.current().kind == TokenKind::Export {
            self.advance();
            true
        } else {
            false
        };

        let location = SourceLocation {
            file: self.file.clone(),
            line: self.current().line,
            column: self.current().column,
        };

        self.expect(TokenKind::Function)?;

        let name = match &self.current().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(self.error("Expected function name".to_string())),
        };
        self.advance();

        self.expect(TokenKind::LeftParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(TokenKind::RightParen)?;

        let return_type = if self.current().kind != TokenKind::LeftBrace {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::LeftBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::RightBrace)?;

        Ok(FunctionDef {
            name,
            location,
            params,
            return_type,
            body,
            is_exported,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();

        if self.current().kind == TokenKind::RightParen {
            return Ok(params);
        }

        loop {
            let name = match &self.current().kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(self.error("Expected parameter name".to_string())),
            };
            self.advance();

            self.expect(TokenKind::Colon)?;
            let type_expr = self.parse_type_expr()?;

            params.push(Parameter { name, type_expr });

            if self.current().kind != TokenKind::Comma {
                break;
            }
            self.advance();
        }

        Ok(params)
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        match &self.current().kind {
            TokenKind::Identifier(name) => {
                let type_name = name.clone();
                self.advance();
                Ok(TypeExpr::Named(type_name))
            }
            _ => Err(self.error("Expected type".to_string())),
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut statements = Vec::new();

        while self.current().kind != TokenKind::RightBrace && self.current().kind != TokenKind::Eof
        {
            statements.push(self.parse_statement()?);
        }

        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.current().kind {
            TokenKind::Return => {
                self.advance();
                let expr = if self.current().kind != TokenKind::Semicolon
                    && self.current().kind != TokenKind::RightBrace
                {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                if self.current().kind == TokenKind::Semicolon {
                    self.advance();
                }
                Ok(Statement::Return(expr))
            }
            TokenKind::Identifier(_) => self.try_parse_variable_decl_or_expression(),
            _ => {
                let expr = self.parse_expression()?;
                if self.current().kind == TokenKind::Semicolon {
                    self.advance();
                }
                Ok(Statement::Expression(expr))
            }
        }
    }

    fn try_parse_variable_decl_or_expression(&mut self) -> Result<Statement, ParseError> {
        let name = match &self.current().kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => unreachable!(),
        };
        self.advance();

        if let TokenKind::Identifier(_) = &self.current().kind {
            let type_expr = self.parse_type_expr()?;
            self.expect(TokenKind::ColonEq)?;
            let initializer = self.parse_expression()?;
            if self.current().kind == TokenKind::Semicolon {
                self.advance();
            }
            return Ok(Statement::VariableDecl {
                name,
                type_expr,
                initializer,
            });
        }

        let mut expr = Expr::Identifier(name);

        while self.current().kind == TokenKind::LeftParen {
            self.advance();
            let arguments = self.parse_argument_list()?;
            self.expect(TokenKind::RightParen)?;
            expr = Expr::Call {
                function: Box::new(expr),
                arguments,
            };
        }

        expr = self.parse_binary_continuation(expr)?;

        if self.current().kind == TokenKind::Semicolon {
            self.advance();
        }
        Ok(Statement::Expression(expr))
    }

    fn parse_binary_continuation(&mut self, left: Expr) -> Result<Expr, ParseError> {
        self.parse_comparison_continuation(left)
    }

    fn parse_comparison_continuation(&mut self, mut left: Expr) -> Result<Expr, ParseError> {
        left = self.parse_term_continuation(left)?;

        loop {
            let op = match &self.current().kind {
                TokenKind::EqEq => BinaryOperator::Eq,
                TokenKind::Ne => BinaryOperator::Ne,
                TokenKind::Lt => BinaryOperator::Lt,
                TokenKind::Gt => BinaryOperator::Gt,
                TokenKind::Le => BinaryOperator::Le,
                TokenKind::Ge => BinaryOperator::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_term()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term_continuation(&mut self, mut left: Expr) -> Result<Expr, ParseError> {
        left = self.parse_factor_continuation(left)?;

        loop {
            let op = match &self.current().kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_factor_continuation(&mut self, mut left: Expr) -> Result<Expr, ParseError> {
        loop {
            let op = match &self.current().kind {
                TokenKind::Star => BinaryOperator::Mul,
                TokenKind::Slash => BinaryOperator::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_call()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;

        loop {
            let op = match &self.current().kind {
                TokenKind::EqEq => BinaryOperator::Eq,
                TokenKind::Ne => BinaryOperator::Ne,
                TokenKind::Lt => BinaryOperator::Lt,
                TokenKind::Gt => BinaryOperator::Gt,
                TokenKind::Le => BinaryOperator::Le,
                TokenKind::Ge => BinaryOperator::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_term()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        loop {
            let op = match &self.current().kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_call()?;

        loop {
            let op = match &self.current().kind {
                TokenKind::Star => BinaryOperator::Mul,
                TokenKind::Slash => BinaryOperator::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_call()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        while self.current().kind == TokenKind::LeftParen {
            self.advance();
            let arguments = self.parse_argument_list()?;
            self.expect(TokenKind::RightParen)?;
            expr = Expr::Call {
                function: Box::new(expr),
                arguments,
            };
        }

        Ok(expr)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        if self.current().kind == TokenKind::RightParen {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);
            if self.current().kind != TokenKind::Comma {
                break;
            }
            self.advance();
        }

        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.current().kind {
            TokenKind::IntegerLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::Literal(Literal::Integer(n)))
            }
            TokenKind::FloatLiteral(f) => {
                let f = *f;
                self.advance();
                Ok(Expr::Literal(Literal::Float(f)))
            }
            TokenKind::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Literal(Literal::String(s)))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(true)))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(false)))
            }
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(Expr::Identifier(name))
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RightParen)?;
                Ok(expr)
            }
            _ => Err(self.error(format!("Unexpected token: {:?}", self.current().kind))),
        }
    }
}
