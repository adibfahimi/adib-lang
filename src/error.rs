use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrame {
    pub name: String,
    pub position: Option<Position>,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub position: Option<Position>,
    pub stack: Vec<StackFrame>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            position: None,
            stack: Vec::new(),
        }
    }

    pub fn at(message: impl Into<String>, position: Position) -> Self {
        Self {
            message: message.into(),
            position: Some(position),
            stack: Vec::new(),
        }
    }

    pub fn maybe_at(message: impl Into<String>, position: Option<Position>) -> Self {
        Self {
            message: message.into(),
            position,
            stack: Vec::new(),
        }
    }

    pub fn with_position(mut self, position: Position) -> Self {
        if self.position.is_none() {
            self.position = Some(position);
        }
        self
    }

    pub fn with_frame(mut self, name: impl Into<String>, position: Option<Position>) -> Self {
        self.stack.push(StackFrame {
            name: name.into(),
            position,
        });
        self
    }

    pub fn with_stack(mut self, stack: Vec<StackFrame>) -> Self {
        if self.stack.is_empty() {
            self.stack = stack;
        }
        self
    }

    /// Render the diagnostic with the source line and a caret under the column.
    pub fn render(&self, source: &str) -> String {
        let mut output = String::new();
        output.push_str("error: ");
        output.push_str(&self.message);
        output.push('\n');

        if let Some(position) = self.position {
            output.push_str(&format!("  --> {}:{}\n", position.line, position.column));

            if let Some(line) = source.lines().nth(position.line.saturating_sub(1)) {
                let number = position.line.to_string();
                let gutter = " ".repeat(number.len());
                output.push_str(&format!("{} |\n", gutter));
                output.push_str(&format!("{} | {}\n", number, line));
                output.push_str(&format!(
                    "{} | {}^\n",
                    gutter,
                    " ".repeat(position.column.saturating_sub(1))
                ));
            }
        }

        if !self.stack.is_empty() {
            output.push_str("\nstack trace:\n");
            for frame in self.stack.iter().rev() {
                match frame.position {
                    Some(position) => output.push_str(&format!(
                        "  at `{}` ({}:{})\n",
                        frame.name, position.line, position.column
                    )),
                    None => output.push_str(&format!("  at `{}`\n", frame.name)),
                }
            }
        }

        output.trim_end().to_string()
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "error: {}", self.message)?;
        if let Some(position) = self.position {
            write!(formatter, "\n  --> {}:{}", position.line, position.column)?;
        }
        if !self.stack.is_empty() {
            writeln!(formatter)?;
            writeln!(formatter, "stack trace:")?;
            for frame in self.stack.iter().rev() {
                match frame.position {
                    Some(position) => {
                        writeln!(
                            formatter,
                            "  at `{}` ({}:{})",
                            frame.name, position.line, position.column
                        )?;
                    }
                    None => writeln!(formatter, "  at `{}`", frame.name)?,
                }
            }
        }
        Ok(())
    }
}
