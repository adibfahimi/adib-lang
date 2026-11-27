use crate::ast::*;
use crate::error::{Diagnostic, Position};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self, Diagnostic> {
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
        })
    }

    pub fn parse(&mut self) -> Result<Program, Diagnostic> {
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
    fn parse_statement(&mut self) -> Result<Statement, Diagnostic> {
        let position = self.position();
        let node = self.parse_statement_inner()?;
        Ok(Spanned::new(node, position))
    }

    fn parse_statement_inner(&mut self) -> Result<StatementKind, Diagnostic> {
        match self.current_token.kind {
            TokenKind::Identifier => self.parse_expression_statement(),
            TokenKind::Var | TokenKind::Const => self.parse_variable_declaration(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Function => self.parse_function_declaration(),
            TokenKind::LBrace => self.parse_block(),
            TokenKind::Break => {
                self.consume(TokenKind::Break)?;
                Ok(StatementKind::Break)
            }
            TokenKind::Continue => {
                self.consume(TokenKind::Continue)?;
                Ok(StatementKind::Continue)
            }
            _ => self.error(format!(
                "unexpected {}; expected a statement",
                describe_token(&self.current_token)
            )),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<StatementKind, Diagnostic> {
        let position = self.position();
        let expression = self.parse_expression()?;

        let current_kind = self.current_token.kind.clone();
        let (operator, increments) = match current_kind {
            TokenKind::Assign => (AssignmentOperator::Assign, false),
            TokenKind::PlusAssign => (AssignmentOperator::Add, false),
            TokenKind::MinusAssign => (AssignmentOperator::Sub, false),
            TokenKind::MulAssign => (AssignmentOperator::Mul, false),
            TokenKind::DivAssign => (AssignmentOperator::Div, false),
            TokenKind::ModAssign => (AssignmentOperator::Mod, false),
            TokenKind::PlusPlus => (AssignmentOperator::Add, true),
            TokenKind::MinusMinus => (AssignmentOperator::Sub, true),
            _ => {
                if let ExpressionKind::FunctionCall(call) = expression.node {
                    return Ok(StatementKind::FunctionCall(call));
                }
                return self.error_at(
                    position,
                    "expected an assignment or a function call statement",
                );
            }
        };

        self.consume(current_kind)?;
        let target = self.make_assignment_target(expression, position)?;
        let value = if increments {
            self.number_expression(1.0, position)
        } else {
            self.parse_expression()?
        };

        Ok(StatementKind::Assignment(Assignment {
            target,
            operator,
            value,
        }))
    }

    fn make_assignment_target(
        &self,
        expression: Expression,
        position: Position,
    ) -> Result<AssignTarget, Diagnostic> {
        let mut indices = Vec::new();
        let mut current = expression;

        loop {
            match current.node {
                ExpressionKind::ArrayIndexing { array, index } => {
                    indices.push(*index);
                    current = *array;
                }
                ExpressionKind::Identifier(root) => {
                    if indices.is_empty() {
                        return Ok(AssignTarget::Variable(root));
                    }
                    indices.reverse();
                    return Ok(AssignTarget::Index { root, indices });
                }
                _ => {
                    return self.error_at(
                        position,
                        "can only assign to a variable or an array element",
                    )
                }
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<StatementKind, Diagnostic> {
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

        Ok(StatementKind::VariableDeclaration(VariableDeclaration {
            name,
            value,
            is_const,
        }))
    }

    fn parse_if_statement(&mut self) -> Result<StatementKind, Diagnostic> {
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

        Ok(StatementKind::If(IfStatement {
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<StatementKind, Diagnostic> {
        self.consume(TokenKind::While)?;
        self.consume(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RParen)?;

        let body = Box::new(self.parse_statement()?);

        Ok(StatementKind::While(WhileStatement { condition, body }))
    }

    fn parse_for_statement(&mut self) -> Result<StatementKind, Diagnostic> {
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
            Spanned::new(ExpressionKind::BooleanLiteral(true), self.position())
        };
        self.consume(TokenKind::SemiColon)?;

        let update = if self.current_token.kind != TokenKind::RParen {
            Some(self.parse_expression_statement_with_span()?)
        } else {
            None
        };
        self.consume(TokenKind::RParen)?;

        let body = Box::new(self.parse_statement()?);

        Ok(StatementKind::For(ForStatement {
            init: Box::new(init),
            condition,
            update: Box::new(update),
            body,
        }))
    }

    fn parse_expression_statement_with_span(&mut self) -> Result<Statement, Diagnostic> {
        let position = self.position();
        let node = self.parse_expression_statement()?;
        Ok(Spanned::new(node, position))
    }

    fn parse_function_declaration(&mut self) -> Result<StatementKind, Diagnostic> {
        self.consume(TokenKind::Function)?;
        let name = self.expect_identifier()?;
        self.consume(TokenKind::LParen)?;
        let parameters = self.parse_parameters()?;
        self.consume(TokenKind::RParen)?;
        let body = Box::new(self.parse_statement()?);

        Ok(StatementKind::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            body,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<StatementKind, Diagnostic> {
        self.consume(TokenKind::Return)?;
        let value = match self.current_token.kind {
            TokenKind::RBrace | TokenKind::Eof | TokenKind::SemiColon => None,
            _ => Some(self.parse_expression()?),
        };
        Ok(StatementKind::Return(value))
    }

    fn parse_block(&mut self) -> Result<StatementKind, Diagnostic> {
        self.consume(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while self.current_token.kind != TokenKind::RBrace {
            if self.current_token.kind == TokenKind::Eof {
                return self.error("unexpected end of file; expected `}`");
            }
            if self.current_token.kind == TokenKind::SemiColon {
                self.consume(TokenKind::SemiColon)?;
                continue;
            }
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenKind::RBrace)?;
        Ok(StatementKind::Block(statements))
    }

    // Expression Parsing Methods
    fn parse_expression(&mut self) -> Result<Expression, Diagnostic> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_and()?;
        while self.current_token.kind == TokenKind::Or {
            let position = self.position();
            self.consume(TokenKind::Or)?;
            let right = self.parse_and()?;
            expr = binary(expr, BinaryOperator::Or, right, position);
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_equality()?;
        while self.current_token.kind == TokenKind::And {
            let position = self.position();
            self.consume(TokenKind::And)?;
            let right = self.parse_equality()?;
            expr = binary(expr, BinaryOperator::And, right, position);
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_comparison()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Eq => BinaryOperator::Eq,
                TokenKind::NotEq => BinaryOperator::NotEq,
                _ => break,
            };
            let position = self.position();
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_comparison()?;
            expr = binary(expr, operator, right, position);
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_addition()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Lt => BinaryOperator::Lt,
                TokenKind::LtEq => BinaryOperator::LtEq,
                TokenKind::Gt => BinaryOperator::Gt,
                TokenKind::GtEq => BinaryOperator::GtEq,
                _ => break,
            };
            let position = self.position();
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_addition()?;
            expr = binary(expr, operator, right, position);
        }
        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_multiplication()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => break,
            };
            let position = self.position();
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_multiplication()?;
            expr = binary(expr, operator, right, position);
        }
        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expression, Diagnostic> {
        let mut expr = self.parse_unary()?;
        loop {
            let operator = match self.current_token.kind {
                TokenKind::Mul => BinaryOperator::Mul,
                TokenKind::Div => BinaryOperator::Div,
                TokenKind::Mod => BinaryOperator::Mod,
                _ => break,
            };
            let position = self.position();
            self.consume(self.current_token.kind.clone())?;
            let right = self.parse_unary()?;
            expr = binary(expr, operator, right, position);
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, Diagnostic> {
        let operator = match self.current_token.kind {
            TokenKind::Minus => UnaryOperator::Minus,
            TokenKind::Not => UnaryOperator::Not,
            _ => return self.parse_postfix(),
        };
        let position = self.position();
        self.consume(self.current_token.kind.clone())?;
        let operand = self.parse_unary()?;
        Ok(Spanned::new(
            ExpressionKind::UnaryOperation {
                operator,
                operand: Box::new(operand),
            },
            position,
        ))
    }

    fn parse_postfix(&mut self) -> Result<Expression, Diagnostic> {
        let expression_position = self.position();
        let mut expr = self.parse_primary()?;
        loop {
            match self.current_token.kind {
                TokenKind::LParen => {
                    let Some(name) = identifier_name(expr) else {
                        return self.error("only named functions can be called");
                    };
                    self.consume(TokenKind::LParen)?;
                    let arguments = self.parse_arguments()?;
                    self.consume(TokenKind::RParen)?;
                    expr = Spanned::new(
                        ExpressionKind::FunctionCall(FunctionCall { name, arguments }),
                        expression_position,
                    );
                }
                TokenKind::Dot => {
                    let position = self.position();
                    self.consume(TokenKind::Dot)?;
                    let property = self.expect_identifier()?;
                    expr = Spanned::new(
                        ExpressionKind::PropertyAccess {
                            object: Box::new(expr),
                            property,
                        },
                        position,
                    );
                }
                TokenKind::LBracket => {
                    let position = self.position();
                    self.consume(TokenKind::LBracket)?;
                    let index = self.parse_expression()?;
                    self.consume(TokenKind::RBracket)?;
                    expr = Spanned::new(
                        ExpressionKind::ArrayIndexing {
                            array: Box::new(expr),
                            index: Box::new(index),
                        },
                        position,
                    );
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, Diagnostic> {
        let position = self.position();

        if self.current_token.kind == TokenKind::LParen {
            self.consume(TokenKind::LParen)?;
            let expr = self.parse_expression()?;
            self.consume(TokenKind::RParen)?;
            return Ok(expr);
        }

        let node = match self.current_token.kind {
            TokenKind::String => {
                let value = self.current_token.value.clone();
                self.consume(TokenKind::String)?;
                ExpressionKind::StringLiteral(value)
            }
            TokenKind::Number => {
                let line = self.current_token.line;
                let column = self.current_token.column;
                let value =
                    self.current_token.value.parse().map_err(|_| {
                        Diagnostic::at("invalid number", Position::new(line, column))
                    })?;
                self.consume(TokenKind::Number)?;
                ExpressionKind::NumberLiteral(value)
            }
            TokenKind::True => {
                self.consume(TokenKind::True)?;
                ExpressionKind::BooleanLiteral(true)
            }
            TokenKind::False => {
                self.consume(TokenKind::False)?;
                ExpressionKind::BooleanLiteral(false)
            }
            TokenKind::Null => {
                self.consume(TokenKind::Null)?;
                ExpressionKind::NullLiteral
            }
            TokenKind::Identifier => {
                let name = self.current_token.value.clone();
                self.consume(TokenKind::Identifier)?;
                ExpressionKind::Identifier(name)
            }
            TokenKind::LBracket => self.parse_array_literal()?,
            _ => {
                return self.error(format!(
                    "unexpected {} in expression",
                    describe_token(&self.current_token)
                ))
            }
        };

        Ok(Spanned::new(node, position))
    }

    // Helper Methods
    fn number_expression(&self, value: f64, position: Position) -> Expression {
        Spanned::new(ExpressionKind::NumberLiteral(value), position)
    }

    fn parse_array_literal(&mut self) -> Result<ExpressionKind, Diagnostic> {
        self.consume(TokenKind::LBracket)?;
        let elements = self.parse_arguments()?;
        self.consume(TokenKind::RBracket)?;
        Ok(ExpressionKind::ArrayLiteral(elements))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, Diagnostic> {
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

    fn parse_parameters(&mut self) -> Result<Vec<String>, Diagnostic> {
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

    fn expect_identifier(&mut self) -> Result<String, Diagnostic> {
        if let TokenKind::Identifier = self.current_token.kind {
            let name = self.current_token.value.clone();
            self.consume(TokenKind::Identifier)?;
            Ok(name)
        } else {
            self.error(format!(
                "expected an identifier, found {}",
                describe_token(&self.current_token)
            ))
        }
    }

    fn consume(&mut self, expected_kind: TokenKind) -> Result<(), Diagnostic> {
        if self.current_token.kind == expected_kind {
            self.current_token = self.lexer.next_token()?;
            Ok(())
        } else {
            self.error(format!(
                "expected {}, found {}",
                expected_kind.label(),
                describe_token(&self.current_token)
            ))
        }
    }

    fn position(&self) -> Position {
        Position::new(self.current_token.line, self.current_token.column)
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T, Diagnostic> {
        self.error_at(self.position(), message)
    }

    fn error_at<T>(&self, position: Position, message: impl Into<String>) -> Result<T, Diagnostic> {
        Err(Diagnostic::at(message, position))
    }
}

fn identifier_name(expression: Expression) -> Option<String> {
    match expression.node {
        ExpressionKind::Identifier(name) => Some(name),
        _ => None,
    }
}

fn binary(
    left: Expression,
    operator: BinaryOperator,
    right: Expression,
    position: Position,
) -> Expression {
    Spanned::new(
        ExpressionKind::BinaryOperation {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        },
        position,
    )
}

fn describe_token(token: &Token) -> String {
    match token.kind {
        TokenKind::Identifier => format!("identifier `{}`", token.value),
        TokenKind::Number => format!("number `{}`", token.value),
        TokenKind::String => format!("string {:?}", token.value),
        TokenKind::Eof => "end of file".to_string(),
        _ => token.kind.label().to_string(),
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

        let StatementKind::FunctionCall(call) = &program.body[0].node else {
            panic!("expected function call");
        };
        let ExpressionKind::BinaryOperation {
            operator: BinaryOperator::Add,
            right,
            ..
        } = &call.arguments[0].node
        else {
            panic!("expected addition at the root");
        };
        assert!(matches!(
            right.node,
            ExpressionKind::BinaryOperation {
                operator: BinaryOperator::Mul,
                ..
            }
        ));
    }

    #[test]
    fn comparisons_bind_tighter_than_logic() {
        let program = parse("print(1 < 2 and 2 < 3)");

        let StatementKind::FunctionCall(call) = &program.body[0].node else {
            panic!("expected function call");
        };
        assert!(matches!(
            call.arguments[0].node,
            ExpressionKind::BinaryOperation {
                operator: BinaryOperator::And,
                ..
            }
        ));
    }

    #[test]
    fn allows_bare_return() {
        let program = parse("function noop() { return }");
        let StatementKind::FunctionDeclaration(declaration) = &program.body[0].node else {
            panic!("expected function declaration");
        };
        let StatementKind::Block(body) = &declaration.body.node else {
            panic!("expected function body");
        };
        assert!(matches!(body[0].node, StatementKind::Return(None)));
    }

    #[test]
    fn flattens_nested_index_assignment_targets() {
        let program = parse("grid[row][column] = 42");
        let StatementKind::Assignment(assignment) = &program.body[0].node else {
            panic!("expected assignment");
        };
        let AssignTarget::Index { root, indices } = &assignment.target else {
            panic!("expected index target");
        };
        assert_eq!(root, "grid");
        assert_eq!(indices.len(), 2);
        assert_eq!(assignment.operator, AssignmentOperator::Assign);
    }
}
