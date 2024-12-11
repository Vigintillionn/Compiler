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

  #[token("let")]
  Let,

  #[token("(")]
  LParen,

  #[token(")")]
  RParen,

  #[token("{")]
  LBrace,

  #[token("}")]
  RBrace,

  #[token(";")]
  Semicolon,

  #[token("=")]
  Assign,

  #[token("==")]
  Equals,

  #[token("+")]
  Plus,

  #[token("-")]
  Minus,
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
  Token::lexer(input)
    .map(|res| res.map_err(|_| "Error while lexing".to_string()))
    .collect()
}

fn parse_number(lex: &Lexer<Token>) -> Result<i64, ()> {
  lex.slice().parse::<i64>().map_err(|_| ())
}

#[cfg(test)]
mod tests {
  // TODO: write tests
}
