use crate::{lexer::tokens::TokenKind, Token};
use std::cell::RefCell;

type Block = Vec<Stmt>;

#[derive(Debug)]
pub struct Program(pub Block);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Boolean,
    Function(Vec<Type>, Box<Type>),
    Void,
    Pointer(Box<Type>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Block),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Var(String, Expr, RefCell<Option<Type>>),
    Assign(String, Expr),
    Loop(
        Option<Box<Stmt>>,
        Option<Expr>,
        Option<Box<Stmt>>,
        Box<Stmt>,
    ),
    Ret(Option<Expr>),
    Function(String, Vec<(String, Type)>, Type, Box<Stmt>),
    Expr(Expr),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprKind, pub RefCell<Option<Type>>);

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(LiteralValue),
    Ident(String),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct BinaryOp(pub Box<Expr>, pub Op, pub Box<Expr>);
#[derive(Debug, Clone)]
pub struct UnaryOp(pub Op, pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    And,
    Or,
    Not,
}

impl From<&Token> for Op {
    fn from(value: &Token) -> Self {
        Self::from(&value.kind)
    }
}

impl From<&TokenKind> for Op {
    fn from(tok: &TokenKind) -> Self {
        use TokenKind::*;
        match tok {
            Plus => Self::Add,
            Minus => Self::Sub,
            Asterisk => Self::Mul,
            Slash => Self::Div,
            Eq => Self::Eq,
            NotEq => Self::Neq,
            LThan => Self::Lt,
            LThanEq => Self::Lte,
            GThan => Self::Gt,
            GThanEq => Self::Gte,
            And => Self::And,
            Or => Self::Or,
            Bang => Self::Not,
            _ => panic!("This is not a valid operator! {:?}", tok),
        }
    }
}
