use crate::error::{Diagnostic, Position};
use crate::value::{as_index, value_to_string, Value};

#[derive(Debug, Clone, Copy)]
pub(crate) enum Builtin {
    Print,
    Len,
    Push,
    Pop,
    Last,
    Slice,
    Join,
    Split,
    ToString,
    ParseNumber,
    TypeOf,
    Abs,
    Floor,
    Ceil,
    Round,
    Sqrt,
    Min,
    Max,
}

impl Builtin {
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        let builtin = match name {
            "print" => Builtin::Print,
            "len" => Builtin::Len,
            "push" => Builtin::Push,
            "pop" => Builtin::Pop,
            "last" => Builtin::Last,
            "slice" => Builtin::Slice,
            "join" => Builtin::Join,
            "split" => Builtin::Split,
            "to_string" => Builtin::ToString,
            "parse_number" => Builtin::ParseNumber,
            "type_of" => Builtin::TypeOf,
            "abs" => Builtin::Abs,
            "floor" => Builtin::Floor,
            "ceil" => Builtin::Ceil,
            "round" => Builtin::Round,
            "sqrt" => Builtin::Sqrt,
            "min" => Builtin::Min,
            "max" => Builtin::Max,
            _ => return None,
        };
        Some(builtin)
    }

    fn name(self) -> &'static str {
        match self {
            Builtin::Print => "print",
            Builtin::Len => "len",
            Builtin::Push => "push",
            Builtin::Pop => "pop",
            Builtin::Last => "last",
            Builtin::Slice => "slice",
            Builtin::Join => "join",
            Builtin::Split => "split",
            Builtin::ToString => "to_string",
            Builtin::ParseNumber => "parse_number",
            Builtin::TypeOf => "type_of",
            Builtin::Abs => "abs",
            Builtin::Floor => "floor",
            Builtin::Ceil => "ceil",
            Builtin::Round => "round",
            Builtin::Sqrt => "sqrt",
            Builtin::Min => "min",
            Builtin::Max => "max",
        }
    }
}

pub(crate) enum BuiltinOutcome {
    Value(Value),
    Print(String),
}

pub(crate) fn call(
    builtin: Builtin,
    arguments: &[Value],
    position: Position,
) -> Result<BuiltinOutcome, Diagnostic> {
    let value = match builtin {
        Builtin::Print => return Ok(BuiltinOutcome::Print(format_print(arguments))),
        Builtin::Len => len(arguments, position)?,
        Builtin::Push => push(arguments, position)?,
        Builtin::Pop => pop(arguments, position)?,
        Builtin::Last => last(arguments, position)?,
        Builtin::Slice => slice(arguments, position)?,
        Builtin::Join => join(arguments, position)?,
        Builtin::Split => split(arguments, position)?,
        Builtin::ToString => to_string(arguments, position)?,
        Builtin::ParseNumber => parse_number(arguments, position)?,
        Builtin::TypeOf => type_of(arguments, position)?,
        Builtin::Abs | Builtin::Floor | Builtin::Ceil | Builtin::Round | Builtin::Sqrt => {
            math(builtin, arguments, position)?
        }
        Builtin::Min | Builtin::Max => min_max(builtin, arguments, position)?,
    };

    Ok(BuiltinOutcome::Value(value))
}

fn format_print(arguments: &[Value]) -> String {
    let parts: Vec<String> = arguments.iter().map(value_to_string).collect();
    format!("{}\n", parts.join(" "))
}

fn len(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Len, arguments, 1, position)?;
    match &arguments[0] {
        Value::Array(array) => Ok(Value::Number(array.len() as f64)),
        Value::String(string) => Ok(Value::Number(string.chars().count() as f64)),
        other => Err(type_error(
            Builtin::Len,
            0,
            "an array or string",
            other,
            position,
        )),
    }
}

/// Arrays are value types, so `push` returns a new array.
fn push(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Push, arguments, 2, position)?;
    let Value::Array(mut array) = arguments[0].clone() else {
        return Err(type_error(
            Builtin::Push,
            0,
            "an array",
            &arguments[0],
            position,
        ));
    };
    array.push(arguments[1].clone());
    Ok(Value::Array(array))
}

/// Arrays are value types, so `pop` returns a new array without its last element.
fn pop(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Pop, arguments, 1, position)?;
    let Value::Array(mut array) = arguments[0].clone() else {
        return Err(type_error(
            Builtin::Pop,
            0,
            "an array",
            &arguments[0],
            position,
        ));
    };
    if array.pop().is_none() {
        return Err(Diagnostic::at(
            "`pop` cannot remove from an empty array",
            position,
        ));
    }
    Ok(Value::Array(array))
}

fn last(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Last, arguments, 1, position)?;
    let Value::Array(array) = &arguments[0] else {
        return Err(type_error(
            Builtin::Last,
            0,
            "an array",
            &arguments[0],
            position,
        ));
    };
    array
        .last()
        .cloned()
        .ok_or_else(|| Diagnostic::at("`last` cannot read an empty array", position))
}

fn slice(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity_range(Builtin::Slice, arguments, 2, 3, position)?;

    let length = match &arguments[0] {
        Value::Array(array) => array.len(),
        Value::String(string) => string.chars().count(),
        other => {
            return Err(type_error(
                Builtin::Slice,
                0,
                "an array or string",
                other,
                position,
            ))
        }
    };

    let start = as_index(&arguments[1], Some(position))?.min(length);
    let end = if arguments.len() == 3 {
        as_index(&arguments[2], Some(position))?.min(length)
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

fn join(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Join, arguments, 2, position)?;
    let Value::Array(array) = &arguments[0] else {
        return Err(type_error(
            Builtin::Join,
            0,
            "an array",
            &arguments[0],
            position,
        ));
    };
    let Value::String(separator) = &arguments[1] else {
        return Err(type_error(
            Builtin::Join,
            1,
            "a string",
            &arguments[1],
            position,
        ));
    };
    let parts: Vec<String> = array.iter().map(value_to_string).collect();
    Ok(Value::String(parts.join(separator)))
}

fn split(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::Split, arguments, 2, position)?;
    let Value::String(text) = &arguments[0] else {
        return Err(type_error(
            Builtin::Split,
            0,
            "a string",
            &arguments[0],
            position,
        ));
    };
    let Value::String(separator) = &arguments[1] else {
        return Err(type_error(
            Builtin::Split,
            1,
            "a string",
            &arguments[1],
            position,
        ));
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

fn to_string(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::ToString, arguments, 1, position)?;
    Ok(Value::String(value_to_string(&arguments[0])))
}

fn parse_number(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::ParseNumber, arguments, 1, position)?;
    let Value::String(text) = &arguments[0] else {
        return Err(type_error(
            Builtin::ParseNumber,
            0,
            "a string",
            &arguments[0],
            position,
        ));
    };
    Ok(text
        .trim()
        .parse::<f64>()
        .map(Value::Number)
        .unwrap_or(Value::Null))
}

fn type_of(arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(Builtin::TypeOf, arguments, 1, position)?;
    Ok(Value::String(arguments[0].type_name().to_string()))
}

fn math(builtin: Builtin, arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(builtin, arguments, 1, position)?;
    let Value::Number(number) = arguments[0] else {
        return Err(type_error(builtin, 0, "a number", &arguments[0], position));
    };

    let result = match builtin {
        Builtin::Abs => number.abs(),
        Builtin::Floor => number.floor(),
        Builtin::Ceil => number.ceil(),
        Builtin::Round => number.round(),
        Builtin::Sqrt => {
            if number < 0.0 {
                return Err(Diagnostic::at(
                    "cannot take the square root of a negative number",
                    position,
                ));
            }
            number.sqrt()
        }
        _ => unreachable!(),
    };

    Ok(Value::Number(result))
}

fn min_max(builtin: Builtin, arguments: &[Value], position: Position) -> Result<Value, Diagnostic> {
    require_arity(builtin, arguments, 2, position)?;
    let Value::Number(left) = arguments[0] else {
        return Err(type_error(builtin, 0, "a number", &arguments[0], position));
    };
    let Value::Number(right) = arguments[1] else {
        return Err(type_error(builtin, 1, "a number", &arguments[1], position));
    };
    Ok(Value::Number(if matches!(builtin, Builtin::Min) {
        left.min(right)
    } else {
        left.max(right)
    }))
}

fn require_arity(
    builtin: Builtin,
    arguments: &[Value],
    expected: usize,
    position: Position,
) -> Result<(), Diagnostic> {
    if arguments.len() == expected {
        Ok(())
    } else {
        Err(Diagnostic::at(
            format!(
                "`{}` expects {} argument(s), got {}",
                builtin.name(),
                expected,
                arguments.len()
            ),
            position,
        ))
    }
}

fn require_arity_range(
    builtin: Builtin,
    arguments: &[Value],
    min: usize,
    max: usize,
    position: Position,
) -> Result<(), Diagnostic> {
    if (min..=max).contains(&arguments.len()) {
        Ok(())
    } else {
        Err(Diagnostic::at(
            format!(
                "`{}` expects between {} and {} arguments, got {}",
                builtin.name(),
                min,
                max,
                arguments.len()
            ),
            position,
        ))
    }
}

fn type_error(
    builtin: Builtin,
    argument: usize,
    expected: &str,
    value: &Value,
    position: Position,
) -> Diagnostic {
    Diagnostic::at(
        format!(
            "`{}` expects {} as argument {}, got {}",
            builtin.name(),
            expected,
            argument + 1,
            value.type_name()
        ),
        position,
    )
}
