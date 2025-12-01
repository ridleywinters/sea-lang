use std::path::PathBuf;

use crate::parser::ast::*;
use crate::parser::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    file: PathBuf,
    source_lines: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: PathBuf, source: &str) -> Self {
        Parser {
            tokens,
            pos: 0,
            file,
            source_lines: source.lines().map(String::from).collect(),
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
        let source_line = self.source_lines.get(token.line.saturating_sub(1)).cloned();
        ParseError {
            message,
            location: Some(SourceLocation {
                file: self.file.clone(),
                line: token.line,
                column: token.column,
            }),
            file: Some(self.file.clone()),
            source_line,
            suggestion: None,
        }
    }

    fn error_with_suggestion(&self, message: String, suggestion: String) -> ParseError {
        let mut err = self.error(message);
        err.suggestion = Some(suggestion);
        err
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
        // <export?> function <name> ( <params> ) <return_type?> { <body> }
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

    /// Parses a parameter list.
    ///
    /// Grammar: `<param> ( , <param> )*` where `<param>` is `<name> <type>`
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

            if self.current().kind == TokenKind::Colon {
                return Err(self.error_with_suggestion(
                    "Unexpected ':' between parameter name and type".to_string(),
                    "remove the colon; use 'name type' format instead of 'name: type'".to_string(),
                ));
            }

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
        let mut expr = self.parse_unary()?;

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

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.current().kind == TokenKind::Minus {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryOp {
                op: UnaryOperator::Neg,
                operand: Box::new(operand),
            });
        }
        self.parse_primary()
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
            TokenKind::IntegerLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Literal(Literal::Integer(s)))
            }
            TokenKind::FloatLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Literal(Literal::Decimal(s)))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::lexer::Lexer;

    fn parse_source(source: &str) -> Result<Vec<FunctionDef>, ParseError> {
        let mut lexer = Lexer::new(source, PathBuf::from("test.sea"));
        let tokens = lexer.tokenize_all().unwrap();
        let mut parser = Parser::new(tokens, PathBuf::from("test.sea"), source);
        let (_, functions) = parser.parse_file()?;
        Ok(functions)
    }

    #[test]
    fn test_parameter_without_colon() {
        let source = "function add(a i32, b i32) i32 { return a + b }";
        let functions = parse_source(source).expect("Should parse successfully");

        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].name, "add");
        assert_eq!(functions[0].params.len(), 2);
        assert_eq!(functions[0].params[0].name, "a");
        assert_eq!(functions[0].params[1].name, "b");
    }

    #[test]
    fn test_parameter_with_colon_error() {
        let source = "function add(a: i32) { }";
        let err = parse_source(source).expect_err("Should fail with colon error");

        assert!(err.message.contains("Unexpected ':'"));
        assert!(err.suggestion.is_some());
        assert!(err.suggestion.unwrap().contains("remove the colon"));
    }

    #[test]
    fn test_empty_parameter_list() {
        let source = "function main() { }";
        let functions = parse_source(source).expect("Should parse successfully");

        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].name, "main");
        assert!(functions[0].params.is_empty());
    }

    #[test]
    fn test_negative_integer_literal() {
        let source = "function main() { return -106 }";
        let functions = parse_source(source).expect("Should parse successfully");

        assert_eq!(functions.len(), 1);
        let stmt = &functions[0].body.statements[0];
        match stmt {
            Statement::Return(Some(expr)) => match expr {
                Expr::UnaryOp { op, operand } => {
                    assert!(matches!(op, UnaryOperator::Neg));
                    match operand.as_ref() {
                        Expr::Literal(Literal::Integer(s)) => assert_eq!(s, "106"),
                        _ => panic!("Expected integer literal"),
                    }
                }
                _ => panic!("Expected unary op"),
            },
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_negative_in_expression() {
        let source = "function main() { return 10 + -5 }";
        let functions = parse_source(source).expect("Should parse successfully");

        assert_eq!(functions.len(), 1);
        let stmt = &functions[0].body.statements[0];
        match stmt {
            Statement::Return(Some(expr)) => match expr {
                Expr::BinaryOp { left, op, right } => {
                    match left.as_ref() {
                        Expr::Literal(Literal::Integer(s)) => assert_eq!(s, "10"),
                        _ => panic!("Expected integer literal on left"),
                    }
                    assert!(matches!(op, BinaryOperator::Add));
                    match right.as_ref() {
                        Expr::UnaryOp { op, operand } => {
                            assert!(matches!(op, UnaryOperator::Neg));
                            match operand.as_ref() {
                                Expr::Literal(Literal::Integer(s)) => assert_eq!(s, "5"),
                                _ => panic!("Expected integer literal"),
                            }
                        }
                        _ => panic!("Expected unary op on right"),
                    }
                }
                _ => panic!("Expected binary op"),
            },
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_double_negative() {
        let source = "function main() { return --42 }";
        let functions = parse_source(source).expect("Should parse successfully");

        assert_eq!(functions.len(), 1);
        let stmt = &functions[0].body.statements[0];
        match stmt {
            Statement::Return(Some(expr)) => match expr {
                Expr::UnaryOp { op, operand } => {
                    assert!(matches!(op, UnaryOperator::Neg));
                    match operand.as_ref() {
                        Expr::UnaryOp { op, operand } => {
                            assert!(matches!(op, UnaryOperator::Neg));
                            match operand.as_ref() {
                                Expr::Literal(Literal::Integer(s)) => assert_eq!(s, "42"),
                                _ => panic!("Expected integer literal"),
                            }
                        }
                        _ => panic!("Expected nested unary op"),
                    }
                }
                _ => panic!("Expected unary op"),
            },
            _ => panic!("Expected return statement"),
        }
    }
}
