use crate::ast::*;
use crate::builtins::{self, Builtin, BuiltinOutcome};
use crate::environment::{AssignResult, EnvRef, Environment};
use crate::error::{Diagnostic, Position, StackFrame};
use crate::value::{as_index, FunctionValue, Value};
use std::rc::Rc;

enum Flow {
    Normal,
    Return(Value),
    Break(Option<Position>),
    Continue(Option<Position>),
}

pub struct Interpreter {
    env: EnvRef,
    output: String,
    call_stack: Vec<StackFrame>,
    current_position: Option<Position>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
            output: String::new(),
            call_stack: Vec::new(),
            current_position: None,
        }
    }

    pub fn eval(&mut self, program: &Program) -> Result<(), Diagnostic> {
        for statement in &program.body {
            let position = Some(statement.position);
            match self.exec_statement(statement)? {
                Flow::Normal => {}
                Flow::Return(_) => {
                    return Err(Diagnostic::maybe_at(
                        "`return` outside of a function",
                        position,
                    ))
                }
                Flow::Break(position) => {
                    return Err(Diagnostic::maybe_at("`break` outside of a loop", position))
                }
                Flow::Continue(position) => {
                    return Err(Diagnostic::maybe_at(
                        "`continue` outside of a loop",
                        position,
                    ))
                }
            }
        }
        Ok(())
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    fn exec_statement(&mut self, statement: &Statement) -> Result<Flow, Diagnostic> {
        let previous = self.current_position;
        self.current_position = Some(statement.position);
        let result = self.exec_statement_kind(&statement.node, statement.position);
        self.current_position = previous;
        result.map_err(|mut error| {
            if error.position.is_none() {
                error.position = Some(statement.position);
            }
            error
        })
    }

    fn exec_statement_kind(
        &mut self,
        statement: &StatementKind,
        position: Position,
    ) -> Result<Flow, Diagnostic> {
        match statement {
            StatementKind::FunctionCall(func_call) => {
                self.eval_function_call(func_call, position)?;
                Ok(Flow::Normal)
            }
            StatementKind::VariableDeclaration(var_decl) => {
                let value = match &var_decl.value {
                    Some(value) => self.eval_expression(value)?,
                    None => Value::Null,
                };
                self.env
                    .borrow_mut()
                    .define(var_decl.name.clone(), value, var_decl.is_const);
                Ok(Flow::Normal)
            }
            StatementKind::Assignment(assignment) => {
                self.exec_assignment(assignment)?;
                Ok(Flow::Normal)
            }
            StatementKind::If(if_stmt) => self.eval_if_statement(if_stmt),
            StatementKind::While(while_stmt) => self.eval_while_statement(while_stmt),
            StatementKind::For(for_stmt) => self.eval_for_statement(for_stmt),
            StatementKind::FunctionDeclaration(func_decl) => {
                let function = FunctionValue {
                    declaration: Rc::new(func_decl.clone()),
                    closure: Rc::clone(&self.env),
                };
                self.env.borrow_mut().define(
                    func_decl.name.clone(),
                    Value::Function(Rc::new(function)),
                    false,
                );
                Ok(Flow::Normal)
            }
            StatementKind::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.eval_expression(expr)?,
                    None => Value::Null,
                };
                Ok(Flow::Return(value))
            }
            StatementKind::Block(statements) => self.exec_block(statements),
            StatementKind::Break => Ok(Flow::Break(Some(position))),
            StatementKind::Continue => Ok(Flow::Continue(Some(position))),
        }
    }

    fn exec_block(&mut self, statements: &[Statement]) -> Result<Flow, Diagnostic> {
        let outer = Rc::clone(&self.env);
        self.env = Environment::with_parent(&outer);

        let mut result = Ok(Flow::Normal);
        for statement in statements {
            match self.exec_statement(statement) {
                Ok(Flow::Normal) => {}
                outcome => {
                    result = outcome;
                    break;
                }
            }
        }

        self.env = outer;
        result
    }

    fn exec_assignment(&mut self, assignment: &Assignment) -> Result<(), Diagnostic> {
        match &assignment.target {
            AssignTarget::Variable(name) => {
                let current = if assignment.operator == AssignmentOperator::Assign {
                    None
                } else {
                    Some(self.lookup_variable(name)?)
                };
                let right = self.eval_expression(&assignment.value)?;
                let value = self.apply_assignment(assignment.operator, current, right)?;
                self.assign_variable(name, value)
            }
            AssignTarget::Index { root, indices } => {
                let mut positions = Vec::with_capacity(indices.len());
                for index in indices {
                    let position = index.position;
                    let value = self.eval_expression(index)?;
                    positions.push(as_index(&value, Some(position))?);
                }

                let current = if assignment.operator == AssignmentOperator::Assign {
                    None
                } else {
                    Some(self.read_index_path(root, &positions)?)
                };
                let right = self.eval_expression(&assignment.value)?;
                let value = self.apply_assignment(assignment.operator, current, right)?;
                self.write_index_path(root, &positions, value)
            }
        }
    }

    fn apply_assignment(
        &self,
        operator: AssignmentOperator,
        current: Option<Value>,
        right: Value,
    ) -> Result<Value, Diagnostic> {
        match operator.binary_operator() {
            None => Ok(right),
            Some(binary_operator) => {
                self.eval_binary_op(&binary_operator, current.unwrap_or(Value::Null), right)
            }
        }
    }

    fn read_index_path(&mut self, root: &str, positions: &[usize]) -> Result<Value, Diagnostic> {
        let mut value = self.lookup_variable(root)?;
        for position in positions {
            value = self.eval_array_indexing(value, Value::Number(*position as f64))?;
        }
        Ok(value)
    }

    fn write_index_path(
        &mut self,
        root: &str,
        positions: &[usize],
        value: Value,
    ) -> Result<(), Diagnostic> {
        let mut root_value = self.lookup_variable(root)?;
        self.set_index_path(&mut root_value, positions, value)?;

        if self.env.borrow_mut().set_existing(root, root_value) {
            Ok(())
        } else {
            Err(self.error(format!("variable not declared: {}", root)))
        }
    }

    fn set_index_path(
        &self,
        value: &mut Value,
        positions: &[usize],
        new_value: Value,
    ) -> Result<(), Diagnostic> {
        let Some((position, rest)) = positions.split_first() else {
            *value = new_value;
            return Ok(());
        };

        match value {
            Value::Array(array) => {
                if *position >= array.len() {
                    return Err(self.error(format!(
                        "index out of bounds: {} (length is {})",
                        position,
                        array.len()
                    )));
                }
                if rest.is_empty() {
                    array[*position] = new_value;
                    Ok(())
                } else {
                    self.set_index_path(&mut array[*position], rest, new_value)
                }
            }
            other => Err(self.error(format!("cannot index into {}", other.type_name()))),
        }
    }

    fn lookup_variable(&self, name: &str) -> Result<Value, Diagnostic> {
        self.env
            .borrow()
            .get(name)
            .ok_or_else(|| self.error(format!("undefined variable: {}", name)))
    }

    fn assign_variable(&mut self, name: &str, value: Value) -> Result<(), Diagnostic> {
        match self.env.borrow_mut().assign(name, value) {
            AssignResult::Ok => Ok(()),
            AssignResult::Constant => {
                Err(self.error(format!("cannot assign to constant `{}`", name)))
            }
            AssignResult::NotFound => Err(self.error(format!("variable not declared: {}", name))),
        }
    }

    fn eval_if_statement(&mut self, if_stmt: &IfStatement) -> Result<Flow, Diagnostic> {
        let condition = self.eval_expression(&if_stmt.condition)?;
        if self.expect_boolean(condition, "if condition")? {
            self.exec_statement(&if_stmt.then_branch)
        } else if let Some(else_branch) = &if_stmt.else_branch {
            self.exec_statement(else_branch)
        } else {
            Ok(Flow::Normal)
        }
    }

    fn eval_while_statement(&mut self, while_stmt: &WhileStatement) -> Result<Flow, Diagnostic> {
        loop {
            let condition = self.eval_expression(&while_stmt.condition)?;
            if !self.expect_boolean(condition, "while condition")? {
                break;
            }
            match self.exec_statement(&while_stmt.body)? {
                Flow::Normal | Flow::Continue(_) => {}
                Flow::Break(_) => break,
                Flow::Return(value) => return Ok(Flow::Return(value)),
            }
        }
        Ok(Flow::Normal)
    }

    fn eval_for_statement(&mut self, for_stmt: &ForStatement) -> Result<Flow, Diagnostic> {
        if let Some(init) = for_stmt.init.as_ref() {
            match self.exec_statement(init)? {
                Flow::Normal => {}
                flow => return Ok(flow),
            }
        }

        loop {
            let condition = self.eval_expression(&for_stmt.condition)?;
            if !self.expect_boolean(condition, "for condition")? {
                break;
            }

            match self.exec_statement(&for_stmt.body)? {
                Flow::Normal | Flow::Continue(_) => {}
                Flow::Break(_) => break,
                Flow::Return(value) => return Ok(Flow::Return(value)),
            }

            if let Some(update) = for_stmt.update.as_ref() {
                match self.exec_statement(update)? {
                    Flow::Normal | Flow::Continue(_) => {}
                    Flow::Break(_) => break,
                    Flow::Return(value) => return Ok(Flow::Return(value)),
                }
            }
        }
        Ok(Flow::Normal)
    }

    fn eval_arguments(&mut self, call: &FunctionCall) -> Result<Vec<Value>, Diagnostic> {
        let mut arguments = Vec::with_capacity(call.arguments.len());
        for argument in &call.arguments {
            arguments.push(self.eval_expression(argument)?);
        }
        Ok(arguments)
    }

    fn eval_function_call(
        &mut self,
        func_call: &FunctionCall,
        call_position: Position,
    ) -> Result<Value, Diagnostic> {
        if let Some(builtin) = Builtin::from_name(&func_call.name) {
            let arguments = self.eval_arguments(func_call)?;
            return match builtins::call(builtin, &arguments, call_position) {
                Ok(BuiltinOutcome::Value(value)) => Ok(value),
                Ok(BuiltinOutcome::Print(output)) => {
                    self.output.push_str(&output);
                    Ok(Value::Null)
                }
                Err(error) => Err(error.with_stack(self.call_stack.clone())),
            };
        }

        let callee = self.env.borrow().get(&func_call.name).ok_or_else(|| {
            Diagnostic::at(
                format!("undefined function: {}", func_call.name),
                call_position,
            )
            .with_stack(self.call_stack.clone())
        })?;

        let Value::Function(function) = callee else {
            return Err(Diagnostic::at(
                format!("`{}` is not a function", func_call.name),
                call_position,
            )
            .with_stack(self.call_stack.clone()));
        };

        let arguments = self.eval_arguments(func_call)?;
        self.call_function(&function, arguments, call_position)
    }

    fn call_function(
        &mut self,
        function: &Rc<FunctionValue>,
        arguments: Vec<Value>,
        call_position: Position,
    ) -> Result<Value, Diagnostic> {
        let declaration = &function.declaration;

        if declaration.parameters.len() != arguments.len() {
            let mut error = Diagnostic::at(
                format!(
                    "function `{}` expects {} argument(s), but got {}",
                    declaration.name,
                    declaration.parameters.len(),
                    arguments.len()
                ),
                call_position,
            );
            error.stack = self.call_stack.clone();
            return Err(error);
        }

        let outer = Rc::clone(&self.env);
        self.env = Environment::with_parent(&function.closure);

        for (parameter, argument) in declaration.parameters.iter().zip(arguments) {
            self.env
                .borrow_mut()
                .define(parameter.clone(), argument, false);
        }

        self.call_stack.push(StackFrame {
            name: declaration.name.clone(),
            position: Some(call_position),
        });

        let result = self.exec_statement(&declaration.body);
        let frames = self.call_stack.clone();
        self.call_stack.pop();
        self.env = outer;

        match result {
            Ok(Flow::Return(value)) => Ok(value),
            Ok(Flow::Normal) => Ok(Value::Null),
            Ok(Flow::Break(position)) => {
                Err(Diagnostic::maybe_at("`break` outside of a loop", position).with_stack(frames))
            }
            Ok(Flow::Continue(position)) => Err(Diagnostic::maybe_at(
                "`continue` outside of a loop",
                position,
            )
            .with_stack(frames)),
            Err(error) => Err(error.with_stack(frames)),
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Result<Value, Diagnostic> {
        let previous = self.current_position;
        self.current_position = Some(expression.position);
        let result = self.eval_expression_kind(&expression.node, expression.position);
        self.current_position = previous;
        result.map_err(|mut error| {
            if error.position.is_none() {
                error.position = Some(expression.position);
            }
            error
        })
    }

    fn eval_expression_kind(
        &mut self,
        expression: &ExpressionKind,
        position: Position,
    ) -> Result<Value, Diagnostic> {
        match expression {
            ExpressionKind::StringLiteral(s) => Ok(Value::String(s.clone())),
            ExpressionKind::NumberLiteral(n) => Ok(Value::Number(*n)),
            ExpressionKind::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            ExpressionKind::NullLiteral => Ok(Value::Null),
            ExpressionKind::Identifier(name) => self.lookup_variable(name),
            ExpressionKind::BinaryOperation {
                left,
                operator,
                right,
            } => {
                if matches!(operator, BinaryOperator::And | BinaryOperator::Or) {
                    let left_value = self.eval_expression(left)?;
                    let left_bool = self.expect_boolean(left_value, "logical operator")?;
                    let short_circuits = match operator {
                        BinaryOperator::And => !left_bool,
                        BinaryOperator::Or => left_bool,
                        _ => unreachable!(),
                    };
                    if short_circuits {
                        return Ok(Value::Boolean(left_bool));
                    }
                    let right_value = self.eval_expression(right)?;
                    let right_bool = self.expect_boolean(right_value, "logical operator")?;
                    return Ok(Value::Boolean(right_bool));
                }

                let left_value = self.eval_expression(left)?;
                let right_value = self.eval_expression(right)?;
                self.eval_binary_op(operator, left_value, right_value)
            }
            ExpressionKind::UnaryOperation { operator, operand } => {
                let operand_value = self.eval_expression(operand)?;
                self.eval_unary_op(operator, operand_value)
            }
            ExpressionKind::FunctionCall(func_call) => self.eval_function_call(func_call, position),
            ExpressionKind::ArrayLiteral(elements) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.eval_expression(element)?);
                }
                Ok(Value::Array(values))
            }
            ExpressionKind::PropertyAccess { object, property } => {
                let object_value = self.eval_expression(object)?;
                self.eval_property_access(object_value, property)
            }
            ExpressionKind::ArrayIndexing { array, index } => {
                let array_value = self.eval_expression(array)?;
                let index_value = self.eval_expression(index)?;
                self.eval_array_indexing(array_value, index_value)
            }
        }
    }

    fn eval_property_access(&self, object: Value, property: &str) -> Result<Value, Diagnostic> {
        if property == "length" {
            return Err(self.error("`.length` is not a valid property, use `.len` instead"));
        }

        match (&object, property) {
            (Value::Array(array), "len") => Ok(Value::Number(array.len() as f64)),
            (Value::String(string), "len") => Ok(Value::Number(string.chars().count() as f64)),
            _ => Err(self.error(format!(
                "cannot read property `{}` of {}",
                property,
                object.type_name()
            ))),
        }
    }

    fn eval_array_indexing(&self, array: Value, index: Value) -> Result<Value, Diagnostic> {
        let position = as_index(&index, self.current_position)?;
        match array {
            Value::Array(array) => array.get(position).cloned().ok_or_else(|| {
                self.error(format!(
                    "index out of bounds: {} (length is {})",
                    position,
                    array.len()
                ))
            }),
            Value::String(string) => string.chars().nth(position).map_or_else(
                || {
                    Err(self.error(format!(
                        "index out of bounds: {} (length is {})",
                        position,
                        string.chars().count()
                    )))
                },
                |character| Ok(Value::String(character.to_string())),
            ),
            other => Err(self.error(format!("cannot index into {}", other.type_name()))),
        }
    }

    fn eval_binary_op(
        &self,
        operator: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, Diagnostic> {
        match (operator, &left, &right) {
            (BinaryOperator::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOperator::Sub, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOperator::Mul, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOperator::Div, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err(self.error("division by zero"))
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (BinaryOperator::Mod, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err(self.error("division by zero"))
                } else {
                    Ok(Value::Number(a % b))
                }
            }
            (BinaryOperator::Add, Value::String(a), Value::String(b)) => {
                Ok(Value::String(format!("{}{}", a, b)))
            }
            (BinaryOperator::Add, Value::Array(a), Value::Array(b)) => {
                let mut combined = a.clone();
                combined.extend(b.iter().cloned());
                Ok(Value::Array(combined))
            }
            (BinaryOperator::Eq, _, _) => Ok(Value::Boolean(left == right)),
            (BinaryOperator::NotEq, _, _) => Ok(Value::Boolean(left != right)),
            (BinaryOperator::Lt, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
            (BinaryOperator::LtEq, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Boolean(a <= b))
            }
            (BinaryOperator::Gt, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
            (BinaryOperator::GtEq, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Boolean(a >= b))
            }
            (BinaryOperator::Lt, Value::String(a), Value::String(b)) => Ok(Value::Boolean(a < b)),
            (BinaryOperator::LtEq, Value::String(a), Value::String(b)) => {
                Ok(Value::Boolean(a <= b))
            }
            (BinaryOperator::Gt, Value::String(a), Value::String(b)) => Ok(Value::Boolean(a > b)),
            (BinaryOperator::GtEq, Value::String(a), Value::String(b)) => {
                Ok(Value::Boolean(a >= b))
            }
            _ => Err(self.error(format!(
                "invalid binary operation: {} {} {}",
                left.type_name(),
                operator.symbol(),
                right.type_name()
            ))),
        }
    }

    fn eval_unary_op(&self, operator: &UnaryOperator, operand: Value) -> Result<Value, Diagnostic> {
        match (operator, &operand) {
            (UnaryOperator::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOperator::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            _ => Err(self.error(format!(
                "invalid unary operation: {} {}",
                operator.symbol(),
                operand.type_name()
            ))),
        }
    }

    fn expect_boolean(&self, value: Value, context: &str) -> Result<bool, Diagnostic> {
        match value {
            Value::Boolean(value) => Ok(value),
            other => Err(self.error(format!(
                "{} expects a boolean, got {}",
                context,
                other.type_name()
            ))),
        }
    }

    fn error(&self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::maybe_at(message, self.current_position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run(source: &str) -> Result<String, String> {
        let mut lexer = Lexer::from_source(source);
        let mut parser = Parser::new(&mut lexer).map_err(|error| error.render(source))?;
        let program = parser.parse().map_err(|error| error.render(source))?;
        let mut interpreter = Interpreter::new();
        interpreter
            .eval(&program)
            .map_err(|error| error.render(source))?;
        Ok(interpreter.take_output())
    }

    #[test]
    fn functions_can_recurse() {
        let source = "
            function fib(n) {
                if (n < 2) { return n }
                return fib(n - 1) + fib(n - 2)
            }
            print(fib(10))
        ";
        assert_eq!(run(source).unwrap(), "55\n");
    }

    #[test]
    fn functions_capture_their_environment() {
        let source = "
            function make_adder(x) {
                function add(y) { return x + y }
                return add
            }
            const add_five = make_adder(5)
            print(add_five(3))
        ";
        assert_eq!(run(source).unwrap(), "8\n");
    }

    #[test]
    fn return_leaves_a_loop() {
        let source = "
            function first_even(nums) {
                for (var i = 0; i < nums.len; i++) {
                    if (nums[i] % 2 == 0) { return nums[i] }
                }
                return -1
            }
            print(first_even([3, 5, 8, 9]))
        ";
        assert_eq!(run(source).unwrap(), "8\n");
    }

    #[test]
    fn constants_cannot_be_reassigned() {
        let error = run("const answer = 42\nanswer = 0").unwrap_err();
        assert!(error.contains("constant"));
    }

    #[test]
    fn logic_short_circuits() {
        let source = "
            function touch() { print(\"touched\") return true }
            print(false and touch())
            print(true or touch())
        ";
        assert_eq!(run(source).unwrap(), "false\ntrue\n");
    }

    #[test]
    fn break_and_continue_work() {
        let source = "
            var values = []
            for (var i = 0; i < 10; i++) {
                if (i == 2) { continue }
                if (i == 5) { break }
                values = push(values, i)
            }
            print(values)
        ";
        assert_eq!(run(source).unwrap(), "[0, 1, 3, 4]\n");
    }

    #[test]
    fn assignment_operators_work() {
        let source = "
            var x = 10
            x += 5
            x -= 3
            x *= 2
            x /= 4
            x %= 4
            x++
            x--
            print(x)
        ";
        assert_eq!(run(source).unwrap(), "2\n");
    }

    #[test]
    fn index_assignment_works() {
        let source = "
            var values = [1, 2, 3]
            values[0] = 10
            values[1] += 5
            values[2]++
            print(values)

            const grid = [[1, 2], [3, 4]]
            grid[1][0] = 30
            grid[0][1] += 5
            print(grid)
        ";
        assert_eq!(run(source).unwrap(), "[10, 7, 4]\n[[1, 7], [30, 4]]\n");
    }

    #[test]
    fn standard_library_works() {
        let source = "
            const values = [1, 2, 3]
            print(push(values, 4))
            print(values)
            print(pop(values))
            print(last(values))
            print(slice(values, 1))
            print(join(values, \"-\"))
            print(split(\"a,b\", \",\"))
            print(to_string(42))
            print(parse_number(\" 3.5 \"))
            print(parse_number(\"nope\"))
            print(abs(-7), floor(3.9), ceil(3.1), round(2.4), sqrt(81))
            print(min(3, 5), max(3, 5))
            print(len(\"héllo\"))
            print(type_of(values))
        ";
        assert_eq!(
            run(source).unwrap(),
            "[1, 2, 3, 4]\n[1, 2, 3]\n[1, 2]\n3\n[2, 3]\n1-2-3\n[a, b]\n42\n3.5\nnull\n7 3 4 2 9\n3 5\n5\narray\n"
        );
    }
}
