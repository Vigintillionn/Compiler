use logos::{Logos, Lexer};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
  #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
  Identifier(String),

  #[regex(r"[0-9]+", parse_number)]
  Number(i64),

  #[token("fn")]
  Function,

  #[token("return")]
  Return,

  #[token("let")]
  Let,

  #[token("if")]
  If,

  #[token("else")]
  Else,

  #[token("while")]
  While,

  #[token("uint")]
  Uint,

  #[token("int")]
  Int,

  #[token("string")]
  StringType,

  #[token("->")]
  Arrow,

  #[token("(")]
  LParen,

  #[token(")")]
  RParen,

  #[token("{")]
  LBrace,

  #[token("}")]
  RBrace,

  #[token(":")]
  Colon,

  #[token(";")]
  Semicolon,

  #[token(",")]
  Comma,

  #[token("=")]
  Assign,

  #[token("==")]
  Equals,

  #[token("!=")]
  NEquals,

  #[token("<=")]
  LTEqual,

  #[token(">=")]
  GTEqual,

  #[token("<")]
  LArrow,

  #[token(">")]
  RArrow,

  #[token("+")]
  Plus,

  #[token("-")]
  Minus,

  #[token("*")]
  Star,

  #[token("/")]
  Slash,
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
  Token::lexer(input)
    .map(|res| res.map_err(|_| "Error while lexing".to_string()))
    .collect()
}

pub fn operator_precedence(op: &Token) -> Option<u8> {
  match op {
    Token::Equals | Token::NEquals | Token::LArrow | Token::RArrow |
    Token::LTEqual | Token::GTEqual => Some(1),
    Token::Plus | Token::Minus => Some(2),
    Token::Star | Token::Slash => Some(3),
    _ => None,
  }
}

pub fn is_left_associative(op: &Token) -> bool {
  match op {
    Token::Plus | Token::Minus | Token::Equals => true,
    _ => false,
  }
}

pub fn operator_to_string(op: &Token) -> String {
  match op {
    Token::Plus => "+".to_string(),
    Token::Minus => "-".to_string(),
    Token::Star => "*".to_string(),
    Token::Slash => "/".to_string(),
    Token::Equals => "==".to_string(),
    Token::NEquals => "!=".to_string(),
    Token::LTEqual => "<=".to_string(),
    Token::LArrow => "<".to_string(),
    Token::GTEqual => ">=".to_string(),
    Token::RArrow => ">".to_string(),
    _ => "".to_string(),
  }
}

fn parse_number(lex: &Lexer<Token>) -> Result<i64, ()> {
  lex.slice().parse::<i64>().map_err(|_| ())
}

#[cfg(test)]
mod tests {
  // TODO: write tests
}
