use std::cell::RefCell;
use crate::Token;

type Block = Vec<Stmt>;

#[derive(Debug)]
pub struct Program(pub Block);

#[derive(Debug, PartialEq)]
pub enum Type {
  Int,
  Float,
  String,
  Boolean,
  Function(Vec<Type>, Box<Type>),
  Void,
  Pointer(Box<Type>),
}

#[derive(Debug)]
pub enum Stmt {
  Block(Block),
  If(Expr, Box<Stmt>, Box<Option<Stmt>>),
  Var(String, Expr, RefCell<Option<Type>>),
  Assign(String, Expr),
  Loop(Option<Box<Stmt>>, Option<Expr>, Option<Box<Stmt>>, Box<Stmt>),
  Ret(Option<Expr>),
  Function(String, Vec<(String, Type)>, Type, Box<Stmt>),
}

#[derive(Debug)]
pub enum LiteralValue {
  Integer(i64),
  Float(f64),
  String(String),
  Boolean(bool)
}

#[derive(Debug)]
// pub struct Expr {
//   pub kind: ExprKind,
//   pub ty: RefCell<Option<Type>>,
// }
pub struct Expr(pub ExprKind, pub RefCell<Option<Type>>);

#[derive(Debug)]
pub enum ExprKind {
  Literal(LiteralValue),
  Ident(String),
  BinaryOp(Box<Expr>, Op, Box<Expr>),
  Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum Op {
  Add,
  Sub,
  Mul,
  Div,
}

impl From<&Token> for Op {
  fn from(tok: &Token) -> Self {
    match tok {
      Token::Plus => Self::Add,
      Token::Minus => Self::Sub,
      Token::Asterisk => Self::Mul,
      Token::Slash => Self::Div,
      _ => panic!("This is not a valid operator!")
    }
  }
}