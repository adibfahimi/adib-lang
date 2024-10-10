use crate::lexer::{Lexer, Token, TokenKind};
use std::collections::HashMap;

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
    Return(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),
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
    variables: HashMap<String, Expression>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self, String> {
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
            variables: HashMap::new(),
        })
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Program { body: Vec::new() };

        while self.current_token.kind != TokenKind::Eof {
            match self.parse_statement() {
                Ok(statement) => program.body.push(statement),
                Err(e) => return Err(e),
            }
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
            _ => Err(format!("Unexpected token: {:?}", self.current_token)),
        }
    }

    fn parse_identifier_statement(&mut self) -> Result<Statement, String> {
        let name = self.current_token.value.clone();
        self.consume(TokenKind::Identifier)?;

        match self.current_token.kind {
            TokenKind::LParen => self.parse_function_call(name),
            TokenKind::Assign => self.parse_set_variable_statement(name),
            _ => Err(format!(
                "Unexpected token after identifier: {:?}",
                self.current_token
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

        self.variables.insert(
            name.clone(),
            value
                .clone()
                .unwrap_or_else(|| Expression::Identifier(name.clone())),
        );

        Ok(Statement::VariableDeclaration(VariableDeclaration {
            name,
            value,
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
        let expr = self.parse_expression()?;
        Ok(Statement::Return(expr))
    }

    fn parse_block(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while self.current_token.kind != TokenKind::RBrace {
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenKind::RBrace)?;
        Ok(Statement::Block(statements))
    }

    // Expression Parsing Methods
    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_binary_operation()
    }

    fn parse_binary_operation(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_unary()?;

        while let Some(op) = self.parse_binary_operator() {
            let right = self.parse_unary()?;
            left = Expression::BinaryOperation {
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            };
        }

        self.parse_property_access_or_indexing(left)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        if let Some(op) = self.parse_unary_operator() {
            let operand = self.parse_unary()?;
            Ok(Expression::UnaryOperation {
                operator: op,
                operand: Box::new(operand),
            })
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        let expr = match self.current_token.kind {
            TokenKind::String => {
                let value = self.current_token.value.clone();
                self.consume(TokenKind::String)?;
                Expression::StringLiteral(value)
            }
            TokenKind::Number => {
                let value = self
                    .current_token
                    .value
                    .parse()
                    .map_err(|_| "Invalid number".to_string())?;
                self.consume(TokenKind::Number)?;
                Expression::NumberLiteral(value)
            }
            TokenKind::True => {
                self.consume(TokenKind::True)?;
                Expression::BooleanLiteral(true)
            }
            TokenKind::False => {
                self.consume(TokenKind::False)?;
                Expression::BooleanLiteral(false)
            }
            TokenKind::Identifier => {
                let name = self.current_token.value.clone();
                self.consume(TokenKind::Identifier)?;
                if self.current_token.kind == TokenKind::LParen {
                    self.parse_function_call_expression(name)?
                } else {
                    Expression::Identifier(name)
                }
            }
            TokenKind::LParen => {
                self.consume(TokenKind::LParen)?;
                let expr = self.parse_expression()?;
                self.consume(TokenKind::RParen)?;
                expr
            }
            TokenKind::LBracket => self.parse_array_literal()?,
            _ => {
                return Err(format!(
                    "Unexpected token in expression: {:?}",
                    self.current_token
                ))
            }
        };

        self.parse_property_access_or_indexing(expr)
    }

    // Helper Methods
    fn parse_property_access_or_indexing(
        &mut self,
        mut expr: Expression,
    ) -> Result<Expression, String> {
        loop {
            match self.current_token.kind {
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

    fn parse_function_call(&mut self, name: String) -> Result<Statement, String> {
        self.consume(TokenKind::LParen)?;
        let arguments = self.parse_arguments()?;
        self.consume(TokenKind::RParen)?;
        Ok(Statement::FunctionCall(FunctionCall { name, arguments }))
    }

    fn parse_function_call_expression(&mut self, name: String) -> Result<Expression, String> {
        self.consume(TokenKind::LParen)?;
        let arguments = self.parse_arguments()?;
        self.consume(TokenKind::RParen)?;
        Ok(Expression::FunctionCall(FunctionCall { name, arguments }))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        self.consume(TokenKind::LBracket)?;
        let elements = self.parse_arguments()?;
        self.consume(TokenKind::RBracket)?;
        Ok(Expression::ArrayLiteral(elements))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut arguments = Vec::new();
        if self.current_token.kind != TokenKind::RParen
            && self.current_token.kind != TokenKind::RBracket
        {
            loop {
                let arg = self.parse_expression()?;
                arguments.push(arg);
                if self.current_token.kind != TokenKind::Comma {
                    break;
                }
                self.consume(TokenKind::Comma)?;
            }
        }
        Ok(arguments)
    }

    fn parse_parameters(&mut self) -> Result<Vec<String>, String> {
        let mut parameters = Vec::new();
        if self.current_token.kind != TokenKind::RParen {
            loop {
                let param = self.expect_identifier()?;
                parameters.push(param);
                if self.current_token.kind != TokenKind::Comma {
                    break;
                }
                self.consume(TokenKind::Comma)?;
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
            _ => Err(format!(
                "Unexpected token in for loop update: {:?}",
                self.current_token
            )),
        }
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let op = match self.current_token.kind {
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Sub),
            TokenKind::Mul => Some(BinaryOperator::Mul),
            TokenKind::Div => Some(BinaryOperator::Div),
            TokenKind::Mod => Some(BinaryOperator::Mod),
            TokenKind::Eq => Some(BinaryOperator::Eq),
            TokenKind::NotEq => Some(BinaryOperator::NotEq),
            TokenKind::Lt => Some(BinaryOperator::Lt),
            TokenKind::LtEq => Some(BinaryOperator::LtEq),
            TokenKind::Gt => Some(BinaryOperator::Gt),
            TokenKind::GtEq => Some(BinaryOperator::GtEq),
            TokenKind::And => Some(BinaryOperator::And),
            TokenKind::Or => Some(BinaryOperator::Or),
            _ => None,
        };

        if op.is_some() {
            self.consume(self.current_token.kind.clone()).ok();
        }

        op
    }

    fn parse_unary_operator(&mut self) -> Option<UnaryOperator> {
        let op = match self.current_token.kind {
            TokenKind::Minus => Some(UnaryOperator::Minus),
            TokenKind::Not => Some(UnaryOperator::Not),
            _ => None,
        };

        if op.is_some() {
            self.consume(self.current_token.kind.clone()).ok();
        }

        op
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        if let TokenKind::Identifier = self.current_token.kind {
            let name = self.current_token.value.clone();
            self.consume(TokenKind::Identifier)?;
            Ok(name)
        } else {
            Err(format!("Expected identifier, got {:?}", self.current_token))
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<(), String> {
        if self.current_token.kind == expected_kind {
            self.current_token = self.lexer.next_token()?;
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, but got {:?}",
                expected_kind, self.current_token.kind
            ))
        }
    }
}
