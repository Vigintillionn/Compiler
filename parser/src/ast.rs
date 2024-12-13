use crate::types::Type;

#[derive(Debug)]
pub enum Expr {
  Literal(i64, Option<Type>),
  Variable(String, Option<Type>),
  BinaryOp(Box<Expr>, String, Box<Expr>, Option<Type>),
  Assign(String, Box<Expr>, Option<Type>),
}

impl Expr {
  pub fn get_type(&self) -> Option<Type> {
    match self {
      Expr::Literal(_, ty) => ty.clone(),
      Expr::Variable(_, ty) => ty.clone(),
      Expr::BinaryOp(_, _, _, ty) => ty.clone(),
      Expr::Assign(_, _, ty) => ty.clone(),
    }
  }
}

#[derive(Debug)]
pub enum Stmt {
  Expr(Expr),
  Let(String, Expr, Option<Type>),            // Variable declaration
  Block(Vec<Stmt>),                   // Scope / Block
  While(Box<Expr>, Box<Stmt>),        // While loop
}