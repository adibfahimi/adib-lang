use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionCall(FunctionCall),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    SetVariable(SetVariableStatement),
    Assignment(Assignment),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Block(Vec<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    NullLiteral,
    Identifier(String),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    FunctionCall(FunctionCall),
    ArrayLiteral(Vec<Expression>),
    PropertyAccess {
        object: Box<Expression>,
        property: String,
    },
    ArrayIndexing {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

// Operator Enums
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

// Statement Structures
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Option<Expression>,
    pub is_const: bool,
}

#[derive(Debug, Clone)]
pub struct SetVariableStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: Box<Option<Statement>>,
    pub condition: Expression,
    pub update: Box<Option<Statement>>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

// Parser Implementation
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self, String> {
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
        })
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Program { body: Vec::new() };

        while self.current_token.kind != TokenKind::Eof {
            if self.current_token.kind == TokenKind::SemiColon {
                self.consume(TokenKind::SemiColon)?;
                continue;
            }
            program.body.push(self.parse_statement()?);
        }

        Ok(program)
    }

    // Statement Parsing Methods
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current_token.kind {
            TokenKind::Identifier => self.parse_identifier_statement(),
            TokenKind::Var | TokenKind::Const => self.parse_variable_declaration(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Function => self.parse_function_declaration(),
            TokenKind::LBrace => self.parse_block(),
            _ => self.error(format!("unexpected token {:?}", self.current_token.kind)),
        }
    }

    fn parse_identifier_statement(&mut self) -> Result<Statement, String> {
        let name = self.current_token.value.clone();
        self.consume(TokenKind::Identifier)?;

        match self.current_token.kind {
            TokenKind::LParen => self.parse_function_call(name),
            TokenKind::Assign => self.parse_set_variable_statement(name),
            _ => self.error(format!(
                "unexpected token after identifier: {:?}",
                self.current_token.kind
            )),
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, String> {
        let is_const = self.current_token.kind == TokenKind::Const;
        self.consume(if is_const {
            TokenKind::Const
        } else {
            TokenKind::Var
        })?;
        let name = self.expect_identifier()?;

        let value = if self.current_token.kind == TokenKind::Assign {
            self.consume(TokenKind::Assign)?;
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Statement::VariableDeclaration(VariableDeclaration {
            name,
            value,
            is_const,
        }))
    }

    fn parse_set_variable_statement(&mut self, name: String) -> Result<Statement, String> {
        self.consume(TokenKind::Assign)?;
        let value = self.parse_expression()?;
        Ok(Statement::SetVariable(SetVariableStatement { name, value }))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::If)?;
        self.consume(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RParen)?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.current_token.kind == TokenKind::Else {
            self.consume(TokenKind::Else)?;
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::While)?;
        self.consume(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RParen)?;

        let body = Box::new(self.parse_statement()?);

        Ok(Statement::While(WhileStatement { condition, body }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::For)?;
        self.consume(TokenKind::LParen)?;

        let init = if self.current_token.kind != TokenKind::SemiColon {
            Some(self.parse_statement()?)
        } else {
            None
        };
        self.consume(TokenKind::SemiColon)?;

        let condition = if self.current_token.kind != TokenKind::SemiColon {
            self.parse_expression()?
        } else {
            Expression::BooleanLiteral(true)
        };
        self.consume(TokenKind::SemiColon)?;

        let update = if self.current_token.kind != TokenKind::RParen {
            Some(self.parse_update_expression()?)
        } else {
            None
        };
        self.consume(TokenKind::RParen)?;

        let body = Box::new(self.parse_statement()?);

        Ok(Statement::For(ForStatement {
            init: Box::new(init),
            condition,
            update: Box::new(update),
            body,
        }))
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Function)?;
        let name = self.expect_identifier()?;
        self.consume(TokenKind::LParen)?;
        let parameters = self.parse_parameters()?;
        self.consume(TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);

        Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            body,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Return)?;
        let value = match self.current_token.kind {
            TokenKind::RBrace | TokenKind::Eof | TokenKind::SemiColon => None,
            _ => Some(self.parse_expression()?),
        };
        Ok(Statement::Return(value))
    }

    fn parse_block(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while self.current_token.kind != TokenKind::RBrace {
            if self.current_token.kind == TokenKind::Eof {
                return self.error("unexpected end of file: expected `}`");
            }
            if self.current_token.kind == TokenKind::SemiColon {
                self.consume(TokenKind::SemiColon)?;
                continue;
            }
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenKind::RBrace)?;
        Ok(Statement::Block(statements))
    }

    // Expression Parsing Methods
    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_and()?;
        while self.current_token.kind == TokenKind::Or {
            self.consume(TokenKind::Or)?;
            let right = self.parse_and()?;
            expr = binary(expr, BinaryOperator::Or, right);
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality()?;
        while self.current_token.kind == TokenKind::And {
            self.consume(TokenKind::And)?;
            let right = self.parse_equality()?;
            expr = binary(expr, BinaryOperator::And, right);
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Eq => BinaryOperator::Eq,
                TokenKind::NotEq => BinaryOperator::NotEq,
                _ => break,
            };
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_comparison()?;
            expr = binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_addition()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Lt => BinaryOperator::Lt,
                TokenKind::LtEq => BinaryOperator::LtEq,
                TokenKind::Gt => BinaryOperator::Gt,
                TokenKind::GtEq => BinaryOperator::GtEq,
                _ => break,
            };
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_addition()?;
            expr = binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_multiplication()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => break,
            };
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_multiplication()?;
            expr = binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Mul => BinaryOperator::Mul,
                TokenKind::Div => BinaryOperator::Div,
                TokenKind::Mod => BinaryOperator::Mod,
                _ => break,
            };
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_unary()?;
            expr = binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        let operator = match self.current_token.kind {
            TokenKind::Minus => UnaryOperator::Minus,
            TokenKind::Not => UnaryOperator::Not,
            _ => return self.parse_postfix(),
        };
        self.consume(self.current_token.kind.clone())?;
        let operand = self.parse_unary()?;
        Ok(Expression::UnaryOperation {
            operator,
            operand: Box::new(operand),
        })
    }

    fn parse_postfix(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.current_token.kind {
                TokenKind::LParen => {
                    let Expression::Identifier(name) = expr else {
                        return self.error("only named functions can be called");
                    };
                    self.consume(TokenKind::LParen)?;
                    let arguments = self.parse_arguments()?;
                    self.consume(TokenKind::RParen)?;
                    expr = Expression::FunctionCall(FunctionCall { name, arguments });
                }
                TokenKind::Dot => {
                    self.consume(TokenKind::Dot)?;
                    let property = self.expect_identifier()?;
                    expr = Expression::PropertyAccess {
                        object: Box::new(expr),
                        property,
                    };
                }
                TokenKind::LBracket => {
                    self.consume(TokenKind::LBracket)?;
                    let index = self.parse_expression()?;
                    self.consume(TokenKind::RBracket)?;
                    expr = Expression::ArrayIndexing {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        match self.current_token.kind {
            TokenKind::String => {
                let value = self.current_token.value.clone();
                self.consume(TokenKind::String)?;
                Ok(Expression::StringLiteral(value))
            }
            TokenKind::Number => {
                let line = self.current_token.line;
                let column = self.current_token.column;
                let value = self
                    .current_token
                    .value
                    .parse()
                    .map_err(|_| format!("{}:{} | invalid number", line, column))?;
                self.consume(TokenKind::Number)?;
                Ok(Expression::NumberLiteral(value))
            }
            TokenKind::True => {
                self.consume(TokenKind::True)?;
                Ok(Expression::BooleanLiteral(true))
            }
            TokenKind::False => {
                self.consume(TokenKind::False)?;
                Ok(Expression::BooleanLiteral(false))
            }
            TokenKind::Null => {
                self.consume(TokenKind::Null)?;
                Ok(Expression::NullLiteral)
            }
            TokenKind::Identifier => {
                let name = self.current_token.value.clone();
                self.consume(TokenKind::Identifier)?;
                Ok(Expression::Identifier(name))
            }
            TokenKind::LParen => {
                self.consume(TokenKind::LParen)?;
                let expr = self.parse_expression()?;
                self.consume(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::LBracket => self.parse_array_literal(),
            _ => self.error(format!(
                "unexpected token in expression: {:?}",
                self.current_token.kind
            )),
        }
    }

    // Helper Methods
    fn parse_function_call(&mut self, name: String) -> Result<Statement, String> {
        self.consume(TokenKind::LParen)?;
        let arguments = self.parse_arguments()?;
        self.consume(TokenKind::RParen)?;
        Ok(Statement::FunctionCall(FunctionCall { name, arguments }))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        self.consume(TokenKind::LBracket)?;
        let elements = self.parse_arguments()?;
        self.consume(TokenKind::RBracket)?;
        Ok(Expression::ArrayLiteral(elements))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut arguments = Vec::new();
        if self.current_token.kind == TokenKind::RParen
            || self.current_token.kind == TokenKind::RBracket
        {
            return Ok(arguments);
        }

        loop {
            arguments.push(self.parse_expression()?);
            if self.current_token.kind != TokenKind::Comma {
                break;
            }
            self.consume(TokenKind::Comma)?;
            if self.current_token.kind == TokenKind::RParen
                || self.current_token.kind == TokenKind::RBracket
            {
                break;
            }
        }
        Ok(arguments)
    }

    fn parse_parameters(&mut self) -> Result<Vec<String>, String> {
        let mut parameters = Vec::new();
        if self.current_token.kind == TokenKind::RParen {
            return Ok(parameters);
        }

        loop {
            parameters.push(self.expect_identifier()?);
            if self.current_token.kind != TokenKind::Comma {
                break;
            }
            self.consume(TokenKind::Comma)?;
            if self.current_token.kind == TokenKind::RParen {
                break;
            }
        }
        Ok(parameters)
    }

    fn parse_update_expression(&mut self) -> Result<Statement, String> {
        let name = self.expect_identifier()?;

        match self.current_token.kind {
            TokenKind::Plus => {
                self.consume(TokenKind::Plus)?;
                self.consume(TokenKind::Plus)?;
                Ok(Statement::SetVariable(SetVariableStatement {
                    name: name.clone(),
                    value: Expression::BinaryOperation {
                        left: Box::new(Expression::Identifier(name)),
                        operator: BinaryOperator::Add,
                        right: Box::new(Expression::NumberLiteral(1.0)),
                    },
                }))
            }
            TokenKind::Minus => {
                self.consume(TokenKind::Minus)?;
                self.consume(TokenKind::Minus)?;
                Ok(Statement::SetVariable(SetVariableStatement {
                    name: name.clone(),
                    value: Expression::BinaryOperation {
                        left: Box::new(Expression::Identifier(name)),
                        operator: BinaryOperator::Sub,
                        right: Box::new(Expression::NumberLiteral(1.0)),
                    },
                }))
            }
            TokenKind::Assign => {
                self.consume(TokenKind::Assign)?;
                let value = self.parse_expression()?;
                Ok(Statement::Assignment(Assignment { name, value }))
            }
            _ => self.error(format!(
                "unexpected token in for loop update: {:?}",
                self.current_token.kind
            )),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        if let TokenKind::Identifier = self.current_token.kind {
            let name = self.current_token.value.clone();
            self.consume(TokenKind::Identifier)?;
            Ok(name)
        } else {
            self.error(format!(
                "expected identifier, got {:?}",
                self.current_token.kind
            ))
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<(), String> {
        if self.current_token.kind == expected_kind {
            self.current_token = self.lexer.next_token()?;
            Ok(())
        } else {
            self.error(format!(
                "expected {:?}, but got {:?}",
                expected_kind, self.current_token.kind
            ))
        }
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T, String> {
        Err(format!(
            "{}:{} | {}",
            self.current_token.line,
            self.current_token.column,
            message.into()
        ))
    }
}

fn binary(left: Expression, operator: BinaryOperator, right: Expression) -> Expression {
    Expression::BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Program {
        let mut lexer = Lexer::from_source(source);
        let mut parser = Parser::new(&mut lexer).expect("parser should initialize");
        parser.parse().expect("source should parse")
    }

    #[test]
    fn multiplication_binds_tighter_than_addition() {
        let program = parse("print(1 + 2 * 3)");

        let Statement::FunctionCall(call) = &program.body[0] else {
            panic!("expected function call");
        };
        let Expression::BinaryOperation {
            operator: BinaryOperator::Add,
            right,
            ..
        } = &call.arguments[0]
        else {
            panic!("expected addition at the root");
        };
        assert!(matches!(
            **right,
            Expression::BinaryOperation {
                operator: BinaryOperator::Mul,
                ..
            }
        ));
    }

    #[test]
    fn comparisons_bind_tighter_than_logic() {
        let program = parse("print(1 < 2 and 2 < 3)");

        let Statement::FunctionCall(call) = &program.body[0] else {
            panic!("expected function call");
        };
        assert!(matches!(
            call.arguments[0],
            Expression::BinaryOperation {
                operator: BinaryOperator::And,
                ..
            }
        ));
    }

    #[test]
    fn allows_bare_return() {
        let program = parse("function noop() { return }");
        let Statement::FunctionDeclaration(declaration) = &program.body[0] else {
            panic!("expected function declaration");
        };
        let Statement::Block(body) = declaration.body.as_ref() else {
            panic!("expected function body");
        };
        assert!(matches!(&body[0], Statement::Return(None)));
    }
}
