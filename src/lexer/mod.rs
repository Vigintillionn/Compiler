pub mod tokens;
use tokens::{Token, KEYWORDS};

use crate::{LexerResult, LexerError};

struct Lexer<'a> {
  src: &'a str,
  pos: usize,
}

impl<'a> Lexer<'a> {
  pub fn new(src: &'a str) -> Self {
    Lexer { src, pos: 0 }
  }

  fn take_while<F>(&mut self, mut pred: F) -> (&'a str, usize)
    where F: FnMut(char) -> bool
  {
    let end = self.src
      .chars()
      .position(|c| !pred(c))
      .unwrap_or_else(|| self.src.len());
    (&self.src[..end], end)
  }

  fn skip_whitespace(&mut self) -> bool {
    let (_, bytes) = self.take_while(|c| c.is_whitespace());
    self.chomp(bytes);

    bytes > 0
  }

  fn skip_comments(&mut self) -> bool {
    let pairs = [
      ("//", "\n"),
      ("/*", "*/")
    ];

    let mut skipped = false;

    for &(start, end) in pairs.iter() {
      if self.src.starts_with(start) {
        self.skip_until(end);
        skipped = true;
      }
    }

    skipped
  }

  fn skip_until(&mut self, pat: &str) {
    while !self.src.is_empty() && !self.src.starts_with(pat) {
      let next = self.src.chars().next().expect("");
      self.chomp(next.len_utf8());
    }

    if !self.src.is_empty() {
      self.chomp(pat.len());
    }
  }

  fn skip(&mut self) {
    loop {
      let ws = self.skip_whitespace();
      let comments = self.skip_comments();

      if !ws && !comments {
        break;
      }
    }
  }

  fn tokenize_cmp(&mut self) -> LexerResult<(Token, usize)> {
    let cmp = self.src.chars().next().unwrap();
    let tokens = [
      ('=', Token::Assign, Token::Eq),
      ('!', Token::Bang, Token::NotEq),
      ('<', Token::LThan, Token::LThanEq),
      ('>', Token::GThan, Token::GThanEq)
    ];

    for (pat, tok1, tok2) in tokens.iter() {
      if cmp == *pat {
        if self.src.chars().nth(1) == Some('=') {
          return Ok((tok2.clone(), 2));
        } else {
          return Ok((tok1.clone(), 1));
        }
      }
    }

    Err(LexerError::InvalidCharacter(cmp, self.pos))
  }

  fn tokenize_single_token(&mut self) -> LexerResult<(Token, usize)> {
    let next = self.src.chars().next().ok_or(LexerError::UnexpectedEOF)?;

    let (tok, bytes) = match next {
      '(' => (Token::OpenParen, 1),
      ')' => (Token::CloseParen, 1),
      '{' => (Token::OpenBrace, 1),
      '}' => (Token::CloseBrace, 1),
      ';' => (Token::Semi, 1),
      ':' => (Token::Colon, 1),
      ',' => (Token::Comma, 1),
      '+' => (Token::Plus, 1),
      '-' => {
        if self.src.chars().nth(1) == Some('>') {
          (Token::Arrow, 2)
        } else {
          (Token::Minus, 1)
        }
      },
      '*' => (Token::Asterisk, 1),
      '/' => (Token::Slash, 1),
      '=' | '!' | '<' | '>' => self.tokenize_cmp()?,
      '&' => {
        if self.src.chars().nth(1) == Some('&') {
          (Token::And, 2)
        } else {
          return Err(LexerError::InvalidCharacter(next, self.pos));
        }
      },
      '|' => {
        if self.src.chars().nth(1) == Some('|') {
          (Token::Or, 2)
        } else {
          return Err(LexerError::InvalidCharacter(next, self.pos));
        }
      },
      '0'..='9' => self.tokenize_number()?,
      c if c.is_alphanumeric() || c == '_' => self.tokenize_ident()?,
      _ => return Err(LexerError::InvalidCharacter(next, self.pos))
    };

    Ok((tok, bytes))
  }

  fn tokenize_number(&mut self) -> LexerResult<(Token, usize)> {
    let (num, bytes) = self.take_while(|c| c.is_digit(10) || c == '.');
    if num.contains('.') {
      let num = num.parse::<f64>()
        .map_err(|_| LexerError::InvalidNumber(num.to_string(), self.pos))?;
      Ok((Token::FloatLiteral(num), bytes))
    } else {
      let num = num.parse::<i64>()
        .map_err(|_| LexerError::InvalidNumber(num.to_string(), self.pos))?;
      Ok((Token::IntLiteral(num), bytes))
    }
  }

  fn tokenize_ident(&mut self) -> LexerResult<(Token, usize)> {
    let (ident, bytes) = self.take_while(|c| c.is_alphanumeric() || c == '_');

    let tok = KEYWORDS
      .get(ident)
      .cloned()
      .unwrap_or(Token::Identifier(ident.to_string()));

    Ok((tok, bytes))
  }

  fn chomp(&mut self, bytes: usize) {
    self.src = &self.src[bytes..];
    self.pos += bytes;
  }
}

impl Iterator for Lexer<'_> {
  type Item = LexerResult<Token>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip();
    if self.src.is_empty() {
      return None;
    }

    let (tok, bytes) = match self.tokenize_single_token() {
      Ok((tok, bytes)) => (tok, bytes),
      Err(e) => return Some(Err(e))
    };

    self.chomp(bytes);
    Some(Ok(tok))
  }
}

pub fn tokenize(src: &str) -> LexerResult<Vec<Token>> {
  // let lexer = Lexer::new(src);
  // let mut tokens = Vec::new();

  // for token in lexer {
  //   tokens.push(token?);
  // }

  let tokens = Lexer::new(src)
    .collect::<Result<Vec<Token>, LexerError>>()?;

  Ok(tokens)
}





/*

3. Lexical Analysis: Building a Lexer

  What is Lexical Analysis?
    Define tokens and explain the purpose of a lexer.
  Defining Tokens
    Create token definitions for identifiers, keywords, operators, literals, etc.
  Writing the Lexer
    Implement a lexer to scan source code and output a stream of tokens.
  Error Handling in the Lexer
    Handle unknown symbols or malformed input gracefully.
  Testing the Lexer
    Provide examples of source code and the corresponding tokens.

*/