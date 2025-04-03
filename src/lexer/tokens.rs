mod keywords {
  use super::Token::*;
  pub const KEYWORDS: phf::Map<&'static str, super::Token> = phf::phf_map! {
    "proc" => Proc,
    "var" => Var,
    "if" => If,
    "else" => Else,
    "ret" => Ret,
    "true" => BoolLiteral(true),
    "false" => BoolLiteral(false),
    "int" => Int,  
    "bool" => Bool,
    "str" => Str,
    "for" => For,
    "while" => While,
    "loop" => Loop,
    "break" => Break,
    "continue" => Continue,
  };
}

pub use keywords::KEYWORDS;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
  Identifier(String),
  
  /* Literals */
  StringLiteral(String),
  IntLiteral(i64),
  FloatLiteral(f64),
  BoolLiteral(bool),

  /* Keywords */
  While,
  For,
  Loop,
  If,
  Else,
  Var,
  Ret,
  Proc,
  Break,
  Continue,

  /* Types */
  Int,
  Float,
  Str,
  Bool,

  /* Operators */
  Plus,         // +
  Minus,        // -
  Asterisk,     // *
  Slash,        // /
  Assign,       // =
  Bang,         // !
  Eq,           // ==
  NotEq,        // !=
  LThan,        // <
  GThan,        // >
  LThanEq,      // <=
  GThanEq,      // >=

  /* Other */
  Semi,         // ;
  Colon,        // :
  OpenParen,    // (
  CloseParen,   // )
  OpenBrace,    // {
  CloseBrace,   // }
  Comma,        // ,
  Arrow,        // ->
  And,          // &&
  Or,           // ||
}

pub enum Assoc {
  Left,
  Right,
}

pub struct OpInfo {
  pub prec: u8,
  pub assoc: Assoc,
  pub is_unary: bool,
}

impl Token {
  pub fn op_info(&self) -> Option<OpInfo> {
    match self {
      Token::Plus | Token::Minus => Some(OpInfo { prec: 2, assoc: Assoc::Left, is_unary: false }),
      Token::Asterisk | Token::Slash => Some(OpInfo { prec: 3, assoc: Assoc::Left, is_unary: false }),
      Token::Eq | Token::NotEq | Token::LThan | Token::GThan | Token::LThanEq | Token::GThanEq => Some(OpInfo { prec: 1, assoc: Assoc::Left, is_unary: false }),
      Token::And | Token::Or => Some(OpInfo { prec: 0, assoc: Assoc::Left, is_unary: false }),
      Token::Bang => Some(OpInfo { prec: 4, assoc: Assoc::Right, is_unary: true }),
      _ => None,
    }
  }
}