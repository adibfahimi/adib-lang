use crate::error::Position;

/// A syntax node together with the source position where it begins.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub position: Position,
}

impl<T> Spanned<T> {
    pub fn new(node: T, position: Position) -> Self {
        Self { node, position }
    }
}

pub type Statement = Spanned<StatementKind>;
pub type Expression = Spanned<ExpressionKind>;

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    FunctionCall(FunctionCall),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    Assignment(Assignment),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Block(Vec<Statement>),
    Return(Option<Expression>),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
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

impl BinaryOperator {
    pub fn symbol(&self) -> &'static str {
        match self {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl UnaryOperator {
    pub fn symbol(&self) -> &'static str {
        match self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "!",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl AssignmentOperator {
    pub fn binary_operator(self) -> Option<BinaryOperator> {
        match self {
            AssignmentOperator::Assign => None,
            AssignmentOperator::Add => Some(BinaryOperator::Add),
            AssignmentOperator::Sub => Some(BinaryOperator::Sub),
            AssignmentOperator::Mul => Some(BinaryOperator::Mul),
            AssignmentOperator::Div => Some(BinaryOperator::Div),
            AssignmentOperator::Mod => Some(BinaryOperator::Mod),
        }
    }
}

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
    pub target: AssignTarget,
    pub operator: AssignmentOperator,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum AssignTarget {
    Variable(String),
    Index {
        root: String,
        indices: Vec<Expression>,
    },
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
