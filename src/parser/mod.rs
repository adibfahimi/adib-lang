pub mod parse_array;
pub mod parse_identifier;
pub mod parse_if;
pub mod parse_keyword;
pub mod parse_number;
pub mod parse_object;
pub mod parse_operator;
pub mod parse_string;

use crate::lexer::Token;
use core::panic;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Number(i64),
    Str(String),
    Identifier(String),
    Object(Vec<(String, Box<Expr>)>),
    Array(Vec<Expr>),
    ArrayIndex {
        name: String,
        index: Box<Expr>,
    },

    Member {
        name: String,
        member: String,
    },

    Op {
        op: char,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Comparison {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    Variable {
        name: String,
        value: Box<Expr>,
    },

    SetVariable {
        name: String,
        value: Box<Expr>,
    },

    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },

    FunctionDef {
        name: String,
        args: Vec<Token>,
        body: Vec<Expr>,
    },

    Return(Box<Expr>),

    Conditional {
        cond: Box<Expr>,
        then_block: Vec<Expr>,
        else_block: Vec<Expr>,
    },

    While {
        cond: Box<Expr>,
        block: Vec<Expr>,
    },

    For {
        init: Box<Expr>,
        cond: Box<Expr>,
        step: Box<Expr>,
        block: Vec<Expr>,
    },

    Nop,
}

use Token::*;

pub(crate) fn parse_expr(tokens: &[Token]) -> (Expr, usize) {
    match tokens[0] {
        Bool(b) => (Expr::Bool(b), 1),
        Num(a) => parse_number::parse_number(tokens, a),
        Str(ref s) => parse_string::parse_string(tokens, s.clone()),

        Keyword(ref key) => parse_keyword::parse_keyword(tokens, key),
        Operator(op) => parse_operator::parse_operator(tokens, op),
        Identifier(ref s) => parse_identifier::parse_identifier(tokens, s.clone()),

        SemiColon => (Expr::Nop, 1),
        Paren('[') => parse_array::parse_array(tokens),
        _ => panic!("Unexpected token: {:?}", tokens[0]),
    }
}

pub fn parse(tokens: &[Token]) -> Vec<Expr> {
    let mut exprs = vec![];
    let mut i = 0;
    while i < tokens.len() {
        let (expr, n) = parse_expr(&tokens[i..]);
        exprs.push(expr);
        i += n;
    }
    exprs
}
