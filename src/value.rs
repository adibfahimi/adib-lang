use crate::ast::FunctionDeclaration;
use crate::environment::EnvRef;
use crate::error::{Diagnostic, Position};
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct FunctionValue {
    pub(crate) declaration: Rc<FunctionDeclaration>,
    pub(crate) closure: EnvRef,
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Function({})", self.declaration.name)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Value {
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
    pub(crate) fn type_name(&self) -> &'static str {
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

pub(crate) fn value_to_string(value: &Value) -> String {
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

/// Validate and convert a runtime value into an array or string index.
pub(crate) fn as_index(value: &Value, position: Option<Position>) -> Result<usize, Diagnostic> {
    let Value::Number(number) = value else {
        return Err(Diagnostic::maybe_at(
            format!("index must be a number, got {}", value.type_name()),
            position,
        ));
    };

    if *number < 0.0 || number.fract() != 0.0 {
        return Err(Diagnostic::maybe_at(
            format!("index must be a non-negative integer, got {}", number),
            position,
        ));
    }

    Ok(*number as usize)
}
