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
    fn type_name(&self) -> &'static str {
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
}

pub struct Interpreter {
    env: EnvRef,
    output: String,
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
        }
    }

    pub fn eval(&mut self, program: &Program) -> Result<(), String> {
        for statement in &program.body {
            match self.exec_statement(statement)? {
                Flow::Normal => {}
                Flow::Return(_) => return Err("`return` outside of a function".to_string()),
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

    fn exec_statement(&mut self, statement: &Statement) -> Result<Flow, String> {
        match statement {
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
            Statement::SetVariable(set_var) => {
                let value = self.eval_expression(&set_var.value)?;
                self.assign_variable(&set_var.name, value)?;
                Ok(Flow::Normal)
            }
            Statement::Assignment(assignment) => {
                let value = self.eval_expression(&assignment.value)?;
                self.assign_variable(&assignment.name, value)?;
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
        }
    }

    fn exec_block(&mut self, statements: &[Statement]) -> Result<Flow, String> {
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

    fn assign_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        match self.env.borrow_mut().assign(name, value) {
            AssignResult::Ok => Ok(()),
            AssignResult::Constant => Err(format!("cannot assign to constant `{}`", name)),
            AssignResult::NotFound => Err(format!("variable not declared: {}", name)),
        }
    }

    fn eval_if_statement(&mut self, if_stmt: &IfStatement) -> Result<Flow, String> {
        let condition = self.eval_expression(&if_stmt.condition)?;
        if self.expect_boolean(condition, "if condition")? {
            self.exec_statement(&if_stmt.then_branch)
        } else if let Some(else_branch) = &if_stmt.else_branch {
            self.exec_statement(else_branch)
        } else {
            Ok(Flow::Normal)
        }
    }

    fn eval_while_statement(&mut self, while_stmt: &WhileStatement) -> Result<Flow, String> {
        loop {
            let condition = self.eval_expression(&while_stmt.condition)?;
            if !self.expect_boolean(condition, "while condition")? {
                break;
            }
            match self.exec_statement(&while_stmt.body)? {
                Flow::Normal => {}
                flow => return Ok(flow),
            }
        }
        Ok(Flow::Normal)
    }

    fn eval_for_statement(&mut self, for_stmt: &ForStatement) -> Result<Flow, String> {
        if let Some(init) = for_stmt.init.as_ref() {
            self.exec_statement(init)?;
        }

        loop {
            let condition = self.eval_expression(&for_stmt.condition)?;
            if !self.expect_boolean(condition, "for condition")? {
                break;
            }

            match self.exec_statement(&for_stmt.body)? {
                Flow::Normal => {}
                flow => return Ok(flow),
            }

            if let Some(update) = for_stmt.update.as_ref() {
                self.exec_statement(update)?;
            }
        }
        Ok(Flow::Normal)
    }

    fn eval_function_call(&mut self, func_call: &FunctionCall) -> Result<Value, String> {
        if func_call.name == "print" {
            let mut parts = Vec::with_capacity(func_call.arguments.len());
            for arg in &func_call.arguments {
                parts.push(value_to_string(&self.eval_expression(arg)?));
            }
            self.output.push_str(&parts.join(" "));
            self.output.push('\n');
            return Ok(Value::Null);
        }

        let callee = self
            .env
            .borrow()
            .get(&func_call.name)
            .ok_or_else(|| format!("undefined function: {}", func_call.name))?;

        let Value::Function(function) = callee else {
            return Err(format!("`{}` is not a function", func_call.name));
        };

        let mut arguments = Vec::with_capacity(func_call.arguments.len());
        for argument in &func_call.arguments {
            arguments.push(self.eval_expression(argument)?);
        }

        self.call_function(&function, arguments)
    }

    fn call_function(
        &mut self,
        function: &Rc<FunctionValue>,
        arguments: Vec<Value>,
    ) -> Result<Value, String> {
        let declaration = &function.declaration;

        if declaration.parameters.len() != arguments.len() {
            return Err(format!(
                "function `{}` expects {} argument(s), but got {}",
                declaration.name,
                declaration.parameters.len(),
                arguments.len()
            ));
        }

        let outer = Rc::clone(&self.env);
        self.env = Environment::with_parent(&function.closure);

        for (parameter, argument) in declaration.parameters.iter().zip(arguments) {
            self.env
                .borrow_mut()
                .define(parameter.clone(), argument, false);
        }

        let result = self.exec_statement(&declaration.body);
        self.env = outer;

        match result? {
            Flow::Return(value) => Ok(value),
            Flow::Normal => Ok(Value::Null),
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
            Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            Expression::NullLiteral => Ok(Value::Null),
            Expression::Identifier(name) => self
                .env
                .borrow()
                .get(name)
                .ok_or_else(|| format!("undefined variable: {}", name)),
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

    fn eval_property_access(&self, object: Value, property: &str) -> Result<Value, String> {
        if property == "length" {
            return Err("`.length` is not a valid property, use `.len` instead".to_string());
        }

        match (&object, property) {
            (Value::Array(array), "len") => Ok(Value::Number(array.len() as f64)),
            (Value::String(string), "len") => Ok(Value::Number(string.chars().count() as f64)),
            _ => Err(format!(
                "cannot read property `{}` of {}",
                property,
                object.type_name()
            )),
        }
    }

    fn eval_array_indexing(&self, array: Value, index: Value) -> Result<Value, String> {
        let Value::Number(position) = index else {
            return Err(format!("index must be a number, got {}", index.type_name()));
        };

        if position < 0.0 || position.fract() != 0.0 {
            return Err(format!(
                "index must be a non-negative integer, got {}",
                position
            ));
        }

        let position = position as usize;

        match array {
            Value::Array(array) => array.get(position).cloned().ok_or_else(|| {
                format!(
                    "index out of bounds: {} (length is {})",
                    position,
                    array.len()
                )
            }),
            Value::String(string) => string.chars().nth(position).map_or_else(
                || {
                    Err(format!(
                        "index out of bounds: {} (length is {})",
                        position,
                        string.chars().count()
                    ))
                },
                |character| Ok(Value::String(character.to_string())),
            ),
            other => Err(format!("cannot index into {}", other.type_name())),
        }
    }

    fn eval_binary_op(
        &self,
        operator: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, String> {
        match (operator, &left, &right) {
            (BinaryOperator::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOperator::Sub, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOperator::Mul, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOperator::Div, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (BinaryOperator::Mod, Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    Err("division by zero".to_string())
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
            _ => Err(format!(
                "invalid binary operation: {} {:?} {}",
                left.type_name(),
                operator,
                right.type_name()
            )),
        }
    }

    fn eval_unary_op(&self, operator: &UnaryOperator, operand: Value) -> Result<Value, String> {
        match (operator, &operand) {
            (UnaryOperator::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOperator::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            _ => Err(format!(
                "invalid unary operation: {:?} {}",
                operator,
                operand.type_name()
            )),
        }
    }

    fn expect_boolean(&self, value: Value, context: &str) -> Result<bool, String> {
        match value {
            Value::Boolean(value) => Ok(value),
            other => Err(format!(
                "{} expects a boolean, got {}",
                context,
                other.type_name()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run(source: &str) -> Result<String, String> {
        let mut lexer = Lexer::from_source(source);
        let mut parser = Parser::new(&mut lexer)?;
        let program = parser.parse()?;
        let mut interpreter = Interpreter::new();
        interpreter.eval(&program)?;
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
}
