use crate::parser::*;
use std::collections::HashMap;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
pub fn my_println(s: &str) {
    println!("{}", s);
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[cfg(target_arch = "wasm32")]
pub fn my_println(s: &str) {
    log(s);
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(FunctionDeclaration),
    Array(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            _ => false,
        }
    }
}

fn value_to_string(value: &Value) -> String {
    match value {
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

pub struct Interpreter {
    variables: HashMap<String, Value>,
    return_value: Option<Value>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            return_value: None,
        }
    }

    pub fn eval(&mut self, program: &Program) -> Result<(), String> {
        for statement in &program.body {
            self.eval_statement(statement)?;
        }
        Ok(())
    }

    fn eval_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::FunctionCall(func_call) => {
                let _ = self.eval_function_call(func_call)?;
                Ok(())
            }
            Statement::VariableDeclaration(var_decl) => self.eval_variable_declaration(var_decl),
            Statement::SetVariable(set_var) => self.eval_set_variable_statement(set_var),
            Statement::Assignment(assignment) => self.eval_assignment(assignment),
            Statement::If(if_stmt) => self.eval_if_statement(if_stmt),
            Statement::While(while_stmt) => self.eval_while_statement(while_stmt),
            Statement::For(for_stmt) => self.eval_for_statement(for_stmt),
            Statement::FunctionDeclaration(func_decl) => self.eval_function_declaration(func_decl),
            Statement::Return(ret) => self.eval_return(ret),
            Statement::Block(statements) => self.eval_block(statements),
        }
    }

    fn eval_property_access(&self, object: Value, property: &str) -> Result<Value, String> {
        if property == "length" {
            return Err(".length is not a valid property\nplease use .len instead\n".to_string());
        }

        match (object.clone(), property) {
            (Value::Array(arr), "len") => Ok(Value::Number(arr.len() as f64)),
            (Value::String(s), "len") => Ok(Value::Number(s.len() as f64)),
            _ => Err(format!(
                "Invalid property access: {:?}.{}",
                object, property
            )),
        }
    }

    fn eval_array_indexing(&self, array: Value, index: Value) -> Result<Value, String> {
        match (array.clone(), index.clone()) {
            (Value::Array(arr), Value::Number(i)) => {
                let idx = i as usize;
                if idx < arr.len() {
                    Ok(arr[idx].clone())
                } else {
                    Err(format!("Index out of bounds: {}", i))
                }
            }
            _ => Err(format!("Invalid array indexing: {:?}[{:?}]", array, index)),
        }
    }

    fn eval_function_declaration(&mut self, func_decl: &FunctionDeclaration) -> Result<(), String> {
        self.variables
            .insert(func_decl.name.clone(), Value::Function(func_decl.clone()));
        Ok(())
    }

    fn eval_function_call(&mut self, func_call: &FunctionCall) -> Result<Value, String> {
        match func_call.name.as_str() {
            "print" => {
                let mut output = String::new();

                let args: Result<Vec<Value>, String> = func_call
                    .arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg))
                    .collect();
                for arg in args? {
                    output += &value_to_string(&arg);
                }

                my_println(output.as_str());

                Ok(Value::Number(0.0))
            }
            _ => {
                if let Some(Value::Function(func_decl)) =
                    self.variables.get(&func_call.name).cloned()
                {
                    let mut new_scope = Interpreter::new();

                    // Bind arguments to parameters
                    for (param, arg) in func_decl.parameters.iter().zip(func_call.arguments.iter())
                    {
                        let arg_value = self.eval_expression(arg)?;
                        new_scope.variables.insert(param.clone(), arg_value);
                    }

                    new_scope.eval_statement(&func_decl.body)?;

                    // Return the result
                    if let Some(return_value) = new_scope.return_value {
                        Ok(return_value)
                    } else {
                        Err(format!(
                            "Function {} did not return a value",
                            func_call.name
                        ))
                    }
                } else {
                    Err(format!("Undefined function: {}", func_call.name))
                }
            }
        }
    }

    fn eval_return(&mut self, expr: &Expression) -> Result<(), String> {
        let value = self.eval_expression(expr)?;
        self.return_value = Some(value);
        Ok(())
    }

    fn eval_variable_declaration(&mut self, var_decl: &VariableDeclaration) -> Result<(), String> {
        let value = if let Some(value) = &var_decl.value {
            self.eval_expression(value)?
        } else {
            // If no value is provided, create a default value based on the type
            match var_decl.name.as_str() {
                "number" => Value::Number(0.0),
                "string" => Value::String(String::new()),
                "boolean" => Value::Boolean(false),
                _ => Value::Number(0.0), // Default to number if type is unknown
            }
        };
        self.variables.insert(var_decl.name.clone(), value);
        Ok(())
    }

    fn eval_set_variable_statement(
        &mut self,
        set_var: &SetVariableStatement,
    ) -> Result<(), String> {
        if !self.variables.contains_key(&set_var.name) {
            return Err(format!("Variable not declared: {}", set_var.name));
        }
        let value = self.eval_expression(&set_var.value)?;
        self.variables.insert(set_var.name.clone(), value);
        Ok(())
    }

    fn eval_assignment(&mut self, assignment: &Assignment) -> Result<(), String> {
        let value = self.eval_expression(&assignment.value)?;
        if self.variables.contains_key(&assignment.name) {
            self.variables.insert(assignment.name.clone(), value);
            Ok(())
        } else {
            Err(format!("Variable not declared: {}", assignment.name))
        }
    }

    fn eval_if_statement(&mut self, if_stmt: &IfStatement) -> Result<(), String> {
        let condition = self.eval_expression(&if_stmt.condition)?;
        if let Value::Boolean(true) = condition {
            self.eval_statement(&if_stmt.then_branch)
        } else if let Some(else_branch) = &if_stmt.else_branch {
            self.eval_statement(else_branch)
        } else {
            Ok(())
        }
    }

    fn eval_while_statement(&mut self, while_stmt: &WhileStatement) -> Result<(), String> {
        while let Value::Boolean(true) = self.eval_expression(&while_stmt.condition)? {
            self.eval_statement(&while_stmt.body)?;
        }
        Ok(())
    }

    fn eval_for_statement(&mut self, for_stmt: &ForStatement) -> Result<(), String> {
        if let Some(init) = for_stmt.init.as_ref() {
            self.eval_statement(init)?;
        }

        while let Value::Boolean(true) = self.eval_expression(&for_stmt.condition)? {
            self.eval_statement(&for_stmt.body)?;
            if let Some(update) = for_stmt.update.as_ref() {
                self.eval_statement(update)?;
            }
        }
        Ok(())
    }

    fn eval_block(&mut self, statements: &[Statement]) -> Result<(), String> {
        for statement in statements {
            self.eval_statement(statement)?;
            if self.return_value.is_some() {
                break;
            }
        }
        Ok(())
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::NumberLiteral(n) => Ok(Value::Number(*n)),
            Expression::BooleanLiteral(b) => Ok(Value::Boolean(*b)),
            Expression::Identifier(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable: {}", name)),
            Expression::BinaryOperation {
                left,
                operator,
                right,
            } => {
                let left_val = self.eval_expression(left)?;
                let right_val = self.eval_expression(right)?;
                self.eval_binary_op(operator, left_val, right_val)
            }
            Expression::UnaryOperation { operator, operand } => {
                let operand_val = self.eval_expression(operand)?;
                self.eval_unary_op(operator, operand_val)
            }
            Expression::FunctionCall(func_call) => self.eval_function_call(func_call),
            Expression::ArrayLiteral(elements) => {
                let evaluated_elements: Result<Vec<Value>, String> = elements
                    .iter()
                    .map(|elem| self.eval_expression(elem))
                    .collect();
                Ok(Value::Array(evaluated_elements?))
            }
            Expression::PropertyAccess { object, property } => {
                let obj_value = self.eval_expression(object)?;
                self.eval_property_access(obj_value, property)
            }
            Expression::ArrayIndexing { array, index } => {
                let array_value = self.eval_expression(array)?;
                let index_value = self.eval_expression(index)?;
                self.eval_array_indexing(array_value, index_value)
            }
        }
    }

    fn eval_binary_op(
        &self,
        op: &BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, String> {
        match (op, left.clone(), right.clone()) {
            (BinaryOperator::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOperator::Sub, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOperator::Mul, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOperator::Div, Value::Number(a), Value::Number(b)) => {
                if b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Number(a / b))
                }
            }
            (BinaryOperator::Eq, a, b) => Ok(Value::Boolean(a == b)),
            (BinaryOperator::NotEq, a, b) => Ok(Value::Boolean(a != b)),
            (BinaryOperator::Lt, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
            (BinaryOperator::LtEq, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Boolean(a <= b))
            }
            (BinaryOperator::Gt, Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
            (BinaryOperator::GtEq, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Boolean(a >= b))
            }
            (BinaryOperator::And, Value::Boolean(a), Value::Boolean(b)) => {
                Ok(Value::Boolean(a && b))
            }
            (BinaryOperator::Or, Value::Boolean(a), Value::Boolean(b)) => {
                Ok(Value::Boolean(a || b))
            }
            (BinaryOperator::Add, Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
            (BinaryOperator::Add, Value::Array(mut a), Value::Array(b)) => {
                a.extend(b);
                Ok(Value::Array(a))
            }
            _ => Err(format!(
                "Invalid binary operation: {:?} {:?} {:?}",
                left, op, right
            )),
        }
    }

    fn eval_unary_op(&self, op: &UnaryOperator, operand: Value) -> Result<Value, String> {
        match (op, operand.clone()) {
            (UnaryOperator::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOperator::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
            _ => Err(format!("Invalid unary operation: {:?} {:?}", op, operand)),
        }
    }
}
