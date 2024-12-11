#[derive(Debug)]
pub enum Expr {
  Literal(i64),
  Variable(String),
  BinaryOp(Box<Expr>, String, Box<Expr>),
}

#[derive(Debug)]
pub enum Stmt {
  Expr(Expr),
  Let(String, Expr),            // Variable declaration
  Block(Vec<Stmt>),             // Scope / Block
  While(Box<Expr>, Box<Stmt>),  // While loop
}