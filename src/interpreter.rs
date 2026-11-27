use crate::error::{Diagnostic, Position, StackFrame};
use crate::parser::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

type EnvRef = Rc<RefCell<Environment>>;

struct Environment {
    values: HashMap<String, Value>,
    constants: HashSet<String>,
    parent: Option<EnvRef>,
}

impl Environment {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            values: HashMap::new(),
            constants: HashSet::new(),
            parent: None,
        }))
    }

    fn with_parent(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            values: HashMap::new(),
            constants: HashSet::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    fn define(&mut self, name: String, value: Value, is_const: bool) {
        if is_const {
            self.constants.insert(name.clone());
        } else {
            self.constants.remove(&name);
        }
        self.values.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.values.get(name) {
            return Some(value.clone());
        }
        self.parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(name))
    }

    fn assign(&mut self, name: &str, value: Value) -> AssignResult {
        if self.constants.contains(name) {
            return AssignResult::Constant;
        }
        if let Some(slot) = self.values.get_mut(name) {
            *slot = value;
            return AssignResult::Ok;
        }
        match &self.parent {
            Some(parent) => parent.borrow_mut().assign(name, value),
            None => AssignResult::NotFound,
        }
    }

    /// Update the value of an existing binding without changing const-ness.
    ///
    /// This is used for index assignment: a `const` array cannot be rebound, but
    /// its elements are still mutable.
    fn set_existing(&mut self, name: &str, value: Value) -> bool {
        if let Some(slot) = self.values.get_mut(name) {
            *slot = value;
            return true;
        }
        match &self.parent {
            Some(parent) => parent.borrow_mut().set_existing(name, value),
            None => false,
        }
    }
}

enum AssignResult {
    Ok,
    Constant,
    NotFound,
}

#[derive(Clone)]
pub struct FunctionValue {
    declaration: Rc<FunctionDeclaration>,
    closure: EnvRef,
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.declaration.name)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<FunctionValue>),
    Array(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Boolean(_) => "boolean",
            Value::Function(_) => "function",
            Value::Array(_) => "array",
        }
    }
}

pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => s.clone(),
        Value::Boolean(b) => b.to_string(),
        Value::Function(_) => "function".to_string(),
        Value::Array(arr) => {
            let elements: Vec<String> = arr.iter().map(value_to_string).collect();
            format!("[{}]", elements.join(", "))
        }
    }
}

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
            let position = statement_position(statement);
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
        match statement {
            Statement::Spanned(inner, position) => {
                let previous = self.current_position;
                self.current_position = Some(*position);
                let result = self.exec_statement(inner);
                self.current_position = previous;
                result.map_err(|mut error| {
                    if error.position.is_none() {
                        error.position = Some(*position);
                    }
                    error
                })
            }
            Statement::FunctionCall(func_call) => {
                self.eval_function_call(func_call)?;
                Ok(Flow::Normal)
            }
            Statement::VariableDeclaration(var_decl) => {
                let value = match &var_decl.value {
                    Some(value) => self.eval_expression(value)?,
                    None => Value::Null,
                };
                self.env
                    .borrow_mut()
                    .define(var_decl.name.clone(), value, var_decl.is_const);
                Ok(Flow::Normal)
            }
            Statement::Assignment(assignment) => {
                self.exec_assignment(assignment)?;
                Ok(Flow::Normal)
            }
            Statement::If(if_stmt) => self.eval_if_statement(if_stmt),
            Statement::While(while_stmt) => self.eval_while_statement(while_stmt),
            Statement::For(for_stmt) => self.eval_for_statement(for_stmt),
            Statement::FunctionDeclaration(func_decl) => {
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
            Statement::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.eval_expression(expr)?,
                    None => Value::Null,
                };
                Ok(Flow::Return(value))
            }
            Statement::Block(statements) => self.exec_block(statements),
            Statement::Break => Ok(Flow::Break(self.current_position)),
            Statement::Continue => Ok(Flow::Continue(self.current_position)),
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
                    let position = expression_position(index).or(self.current_position);
                    let value = self.eval_expression(index)?;
                    positions.push(self.as_index(&value, position)?);
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
        let binary_operator = match operator {
            AssignmentOperator::Assign => return Ok(right),
            AssignmentOperator::Add => BinaryOperator::Add,
            AssignmentOperator::Sub => BinaryOperator::Sub,
            AssignmentOperator::Mul => BinaryOperator::Mul,
            AssignmentOperator::Div => BinaryOperator::Div,
            AssignmentOperator::Mod => BinaryOperator::Mod,
        };
        self.eval_binary_op(&binary_operator, current.unwrap_or(Value::Null), right)
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

    fn as_index(&self, value: &Value, position: Option<Position>) -> Result<usize, Diagnostic> {
        let Value::Number(position_value) = value else {
            return Err(Diagnostic::maybe_at(
                format!("index must be a number, got {}", value.type_name()),
                position.or(self.current_position),
            ));
        };

        if *position_value < 0.0 || position_value.fract() != 0.0 {
            return Err(Diagnostic::maybe_at(
                format!(
                    "index must be a non-negative integer, got {}",
                    position_value
                ),
                position.or(self.current_position),
            ));
        }

        Ok(*position_value as usize)
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

    fn eval_function_call(&mut self, func_call: &FunctionCall) -> Result<Value, Diagnostic> {
        if let Some(result) = self.eval_builtin(func_call) {
            return result.map_err(|error| error.with_stack(self.call_stack.clone()));
        }

        let callee = self.env.borrow().get(&func_call.name).ok_or_else(|| {
            Diagnostic::at(
                format!("undefined function: {}", func_call.name),
                func_call.position(),
            )
            .with_stack(self.call_stack.clone())
        })?;

        let Value::Function(function) = callee else {
            return Err(Diagnostic::at(
                format!("`{}` is not a function", func_call.name),
                func_call.position(),
            )
            .with_stack(self.call_stack.clone()));
        };

        let mut arguments = Vec::with_capacity(func_call.arguments.len());
        for argument in &func_call.arguments {
            arguments.push(self.eval_expression(argument)?);
        }

        self.call_function(&function, arguments, func_call.position())
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

    fn eval_expression(&mut self, expr: &Expression) -> Result<Value, Diagnostic> {
        match expr {
            Expression::Spanned(inner, position) => {
                let previous = self.current_position;
                self.current_position = Some(*position);
                let result = self.eval_expression(inner);
                self.current_position = previous;
                result.map_err(|mut error| {
                    if error.position.is_none() {
                        error.position = Some(*position);
                    }
                    error
                })
            }
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
            Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            Expression::NullLiteral => Ok(Value::Null),
            Expression::Identifier(name) => self.lookup_variable(name),
            Expression::BinaryOperation {
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
            Expression::UnaryOperation { operator, operand } => {
                let operand_value = self.eval_expression(operand)?;
                self.eval_unary_op(operator, operand_value)
            }
            Expression::FunctionCall(func_call) => self.eval_function_call(func_call),
            Expression::ArrayLiteral(elements) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.eval_expression(element)?);
                }
                Ok(Value::Array(values))
            }
            Expression::PropertyAccess { object, property } => {
                let object_value = self.eval_expression(object)?;
                self.eval_property_access(object_value, property)
            }
            Expression::ArrayIndexing { array, index } => {
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
        let position = self.as_index(&index, self.current_position)?;
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
                binary_symbol(operator),
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
                unary_symbol(operator),
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

    // ---------- Built-in functions ----------

    fn eval_builtin(&mut self, call: &FunctionCall) -> Option<Result<Value, Diagnostic>> {
        let result = match call.name.as_str() {
            "print" => self.builtin_print(call),
            "len" => self.builtin_len(call),
            "push" => self.builtin_push(call),
            "pop" => self.builtin_pop(call),
            "last" => self.builtin_last(call),
            "slice" => self.builtin_slice(call),
            "join" => self.builtin_join(call),
            "split" => self.builtin_split(call),
            "to_string" => self.builtin_to_string(call),
            "parse_number" => self.builtin_parse_number(call),
            "type_of" => self.builtin_type_of(call),
            "abs" | "floor" | "ceil" | "round" | "sqrt" => self.builtin_math(call),
            "min" => self.builtin_min_max(call, true),
            "max" => self.builtin_min_max(call, false),
            _ => return None,
        };
        Some(result)
    }

    fn eval_arguments(&mut self, call: &FunctionCall) -> Result<Vec<Value>, Diagnostic> {
        let mut arguments = Vec::with_capacity(call.arguments.len());
        for argument in &call.arguments {
            arguments.push(self.eval_expression(argument)?);
        }
        Ok(arguments)
    }

    fn require_arity(
        &self,
        call: &FunctionCall,
        arguments: &[Value],
        expected: usize,
    ) -> Result<(), Diagnostic> {
        if arguments.len() == expected {
            Ok(())
        } else {
            Err(Diagnostic::at(
                format!(
                    "`{}` expects {} argument(s), got {}",
                    call.name,
                    expected,
                    arguments.len()
                ),
                call.position(),
            ))
        }
    }

    fn require_arity_range(
        &self,
        call: &FunctionCall,
        arguments: &[Value],
        min: usize,
        max: usize,
    ) -> Result<(), Diagnostic> {
        if (min..=max).contains(&arguments.len()) {
            Ok(())
        } else {
            Err(Diagnostic::at(
                format!(
                    "`{}` expects between {} and {} arguments, got {}",
                    call.name,
                    min,
                    max,
                    arguments.len()
                ),
                call.position(),
            ))
        }
    }

    fn type_error(
        &self,
        call: &FunctionCall,
        argument: usize,
        expected: &str,
        value: &Value,
    ) -> Diagnostic {
        Diagnostic::at(
            format!(
                "`{}` expects {} as argument {}, got {}",
                call.name,
                expected,
                argument + 1,
                value.type_name()
            ),
            call.position(),
        )
    }

    fn builtin_index(
        &self,
        call: &FunctionCall,
        value: &Value,
        argument: usize,
    ) -> Result<usize, Diagnostic> {
        let Value::Number(position) = value else {
            return Err(self.type_error(call, argument, "an index", value));
        };
        if *position < 0.0 || position.fract() != 0.0 {
            return Err(Diagnostic::at(
                format!("index must be a non-negative integer, got {}", position),
                call.position(),
            ));
        }
        Ok(*position as usize)
    }

    fn builtin_print(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        let parts: Vec<String> = arguments.iter().map(value_to_string).collect();
        self.output.push_str(&parts.join(" "));
        self.output.push('\n');
        Ok(Value::Null)
    }

    fn builtin_len(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        match &arguments[0] {
            Value::Array(array) => Ok(Value::Number(array.len() as f64)),
            Value::String(string) => Ok(Value::Number(string.chars().count() as f64)),
            other => Err(self.type_error(call, 0, "an array or string", other)),
        }
    }

    /// Arrays are value types, so `push` returns a new array.
    fn builtin_push(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 2)?;
        let Value::Array(mut array) = arguments[0].clone() else {
            return Err(self.type_error(call, 0, "an array", &arguments[0]));
        };
        array.push(arguments[1].clone());
        Ok(Value::Array(array))
    }

    /// Arrays are value types, so `pop` returns a new array without its last element.
    fn builtin_pop(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        let Value::Array(mut array) = arguments[0].clone() else {
            return Err(self.type_error(call, 0, "an array", &arguments[0]));
        };
        if array.pop().is_none() {
            return Err(Diagnostic::at(
                "`pop` cannot remove from an empty array",
                call.position(),
            ));
        }
        Ok(Value::Array(array))
    }

    fn builtin_last(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        let Value::Array(array) = &arguments[0] else {
            return Err(self.type_error(call, 0, "an array", &arguments[0]));
        };
        array
            .last()
            .cloned()
            .ok_or_else(|| Diagnostic::at("`last` cannot read an empty array", call.position()))
    }

    fn builtin_slice(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity_range(call, &arguments, 2, 3)?;

        let length = match &arguments[0] {
            Value::Array(array) => array.len(),
            Value::String(string) => string.chars().count(),
            other => return Err(self.type_error(call, 0, "an array or string", other)),
        };

        let start = self.builtin_index(call, &arguments[1], 1)?.min(length);
        let end = if arguments.len() == 3 {
            self.builtin_index(call, &arguments[2], 2)?.min(length)
        } else {
            length
        };

        if end <= start {
            return match &arguments[0] {
                Value::Array(_) => Ok(Value::Array(Vec::new())),
                Value::String(_) => Ok(Value::String(String::new())),
                _ => unreachable!(),
            };
        }

        match &arguments[0] {
            Value::Array(array) => Ok(Value::Array(array[start..end].to_vec())),
            Value::String(string) => Ok(Value::String(
                string.chars().skip(start).take(end - start).collect(),
            )),
            _ => unreachable!(),
        }
    }

    fn builtin_join(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 2)?;
        let Value::Array(array) = &arguments[0] else {
            return Err(self.type_error(call, 0, "an array", &arguments[0]));
        };
        let Value::String(separator) = &arguments[1] else {
            return Err(self.type_error(call, 1, "a string", &arguments[1]));
        };
        let parts: Vec<String> = array.iter().map(value_to_string).collect();
        Ok(Value::String(parts.join(separator)))
    }

    fn builtin_split(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 2)?;
        let Value::String(text) = &arguments[0] else {
            return Err(self.type_error(call, 0, "a string", &arguments[0]));
        };
        let Value::String(separator) = &arguments[1] else {
            return Err(self.type_error(call, 1, "a string", &arguments[1]));
        };

        if separator.is_empty() {
            return Ok(Value::Array(
                text.chars()
                    .map(|character| Value::String(character.to_string()))
                    .collect(),
            ));
        }

        Ok(Value::Array(
            text.split(separator.as_str())
                .map(|part| Value::String(part.to_string()))
                .collect(),
        ))
    }

    fn builtin_to_string(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        Ok(Value::String(value_to_string(&arguments[0])))
    }

    fn builtin_parse_number(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        let Value::String(text) = &arguments[0] else {
            return Err(self.type_error(call, 0, "a string", &arguments[0]));
        };
        Ok(text
            .trim()
            .parse::<f64>()
            .map(Value::Number)
            .unwrap_or(Value::Null))
    }

    fn builtin_type_of(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        Ok(Value::String(arguments[0].type_name().to_string()))
    }

    fn builtin_math(&mut self, call: &FunctionCall) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 1)?;
        let Value::Number(number) = arguments[0] else {
            return Err(self.type_error(call, 0, "a number", &arguments[0]));
        };

        let result = match call.name.as_str() {
            "abs" => number.abs(),
            "floor" => number.floor(),
            "ceil" => number.ceil(),
            "round" => number.round(),
            "sqrt" => {
                if number < 0.0 {
                    return Err(Diagnostic::at(
                        "cannot take the square root of a negative number",
                        call.position(),
                    ));
                }
                number.sqrt()
            }
            _ => unreachable!(),
        };

        Ok(Value::Number(result))
    }

    fn builtin_min_max(&mut self, call: &FunctionCall, minimum: bool) -> Result<Value, Diagnostic> {
        let arguments = self.eval_arguments(call)?;
        self.require_arity(call, &arguments, 2)?;
        let Value::Number(left) = arguments[0] else {
            return Err(self.type_error(call, 0, "a number", &arguments[0]));
        };
        let Value::Number(right) = arguments[1] else {
            return Err(self.type_error(call, 1, "a number", &arguments[1]));
        };
        Ok(Value::Number(if minimum {
            left.min(right)
        } else {
            left.max(right)
        }))
    }
}

fn statement_position(statement: &Statement) -> Option<Position> {
    match statement {
        Statement::Spanned(_, position) => Some(*position),
        _ => None,
    }
}

fn expression_position(expression: &Expression) -> Option<Position> {
    match expression {
        Expression::Spanned(_, position) => Some(*position),
        Expression::FunctionCall(call) => Some(call.position()),
        _ => None,
    }
}

fn binary_symbol(operator: &BinaryOperator) -> &'static str {
    match operator {
        BinaryOperator::Add => "+",
        BinaryOperator::Sub => "-",
        BinaryOperator::Mul => "*",
        BinaryOperator::Div => "/",
        BinaryOperator::Mod => "%",
        BinaryOperator::Eq => "==",
        BinaryOperator::NotEq => "!=",
        BinaryOperator::Lt => "<",
        BinaryOperator::LtEq => "<=",
        BinaryOperator::Gt => ">",
        BinaryOperator::GtEq => ">=",
        BinaryOperator::And => "and",
        BinaryOperator::Or => "or",
    }
}

fn unary_symbol(operator: &UnaryOperator) -> &'static str {
    match operator {
        UnaryOperator::Minus => "-",
        UnaryOperator::Not => "!",
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
