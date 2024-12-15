use crate::types::Type;

#[derive(Debug)]
pub enum Literal {
  Integer(i64),
  Float(f64),
  String(String),
  Boolean(bool),
}

impl Literal {
  pub fn get_type(&self) -> Type {
    match self {
      Literal::Integer(_) => Type::Int,
      Literal::Float(_) => Type::Int, // TODO: Change this to Float
      Literal::String(_) => Type::String,
      Literal::Boolean(_) => Type::Bool,
    }
  }

  pub fn print(&self, indent_str: &str) {
    match self {
      Literal::Integer(val) => println!("{}INTEGER({})", indent_str, val),
      Literal::Float(val) => println!("{}FLOAT({})", indent_str, val),
      Literal::String(val) => println!("{}STRING({})", indent_str, val),
      Literal::Boolean(val) => println!("{}BOOLEAN({})", indent_str, val),
    }
  }
}

#[derive(Debug)]
pub enum Expr {
  Variable(String, Option<Type>),
  BinaryOp(Box<Expr>, String, Box<Expr>, Option<Type>),
  Assign(String, Box<Expr>, Option<Type>),
  Literal(Literal, Option<Type>),
}

impl Expr {
  pub fn get_type(&self) -> Option<Type> {
    match self {
      Expr::Literal(_, ty) => ty.clone(),
      Expr::Variable(_, ty) => ty.clone(),
      Expr::BinaryOp(lhs, _, rhs, ty) => {
        if let Some(ty) = ty {
          return Some(ty.clone());
        }

        let left_ty = lhs.get_type()?;
        let right_ty = rhs.get_type()?;

        if left_ty == right_ty {
          return Some(left_ty);
        }

        None
      },
      Expr::Assign(_, _, ty) => ty.clone(),
    }
  }

  pub fn print(&self, indent: usize) {
    let indent_str = " ".repeat(indent * 2);
    match self {
      Expr::Literal(val, _) => val.print(&indent_str),
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