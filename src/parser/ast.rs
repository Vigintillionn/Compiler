use std::cell::RefCell;
use crate::Token;

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
  Loop(Option<Box<Stmt>>, Option<Expr>, Option<Box<Stmt>>, Box<Stmt>),
  Ret(Option<Expr>),
  Function(String, Vec<(String, Type)>, Type, Box<Stmt>),
  Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
  Integer(i64),
  Float(f64),
  String(String),
  Boolean(bool)
}

#[derive(Debug, Clone)]
pub struct Expr(pub ExprKind, pub RefCell<Option<Type>>);

#[derive(Debug, Clone)]
pub enum ExprKind {
  Literal(LiteralValue),
  Ident(String),
  BinaryOp(BinaryOp),
  Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct BinaryOp(pub Box<Expr>, pub Op, pub Box<Expr>);

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
  fn from(tok: &Token) -> Self {
    match tok {
      Token::Plus => Self::Add,
      Token::Minus => Self::Sub,
      Token::Asterisk => Self::Mul,
      Token::Slash => Self::Div,
      Token::Eq => Self::Eq,
      Token::NotEq => Self::Neq,
      Token::LThan => Self::Lt,
      Token::LThanEq => Self::Lte,
      Token::GThan => Self::Gt,
      Token::GThanEq => Self::Gte,
      _ => panic!("This is not a valid operator!")
    }
  }
}