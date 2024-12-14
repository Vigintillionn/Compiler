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

  pub fn print(&self, indent: usize) {
    let indent_str = " ".repeat(indent * 2);
    match self {
      Expr::Literal(val, _) => println!("{}INTEGER({})", indent_str, val),
      Expr::Variable(name, _) => println!("{}IDENTIFIER({})", indent_str, name),
      Expr::BinaryOp(left, op, right, _) => {
        println!("{}BinOp({})", indent_str, op);
        left.print(indent + 1);
        right.print(indent + 1);
      },
      Expr::Assign(name, expr, _) => {
        println!("{}ASSIGN({})", indent_str, name);
        expr.print(indent + 1);
      }
    }
  }
}

#[derive(Debug)]
pub enum Stmt {
  Program(Vec<Stmt>),                 // Program
  Expr(Expr),
  Let(String, Expr, Option<Type>),    // Variable declaration
  Block(Vec<Stmt>),                   // Scope / Block
  While(Expr, Box<Stmt>),        // While loop
  If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}

impl Stmt {
  pub fn print(&self, indent: usize) {
    let indent_str = " ".repeat(indent * 2);
    match self {
      Stmt::Program(stmts) | Stmt::Block(stmts) => {
        for stmt in stmts {
          stmt.print(indent);
        }
      },
      Stmt::Expr(expr) => expr.print(indent),
      Stmt::Let(name, expr, _) => {
        println!("{}LetStmt", indent_str);
        println!("{}  |-- IDENTIFIER({})", indent_str, name);
        expr.print(indent + 1);
      },
      Stmt::While(cond, body) => {
        print!("{}While ", indent_str);
        cond.print(0);
        body.print(indent + 1);
      },
      Stmt::If(cond, then, else_do) => {
        println!("{}IfStmt ", indent_str);
        cond.print(0);
        then.print(indent+1);
        // if let Some(otherwise) = *else_do {
        //   println!("  {}ELSE", indent_str);
        //   otherwise.print(indent+1);
        // }
      } 
    }
  }
}