use std::fmt;

use super::{ReportableError, Location};

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone)]
pub enum LexerError {
  InvalidCharacter(Location, char),
  UnterminatedString(Location),
  InvalidNumber(Location, String),
  UnexpectedEOF(Location),
  Other(Location, String),
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LexerError::InvalidCharacter(location, found) => {
        write!(f, "Invalid character '{}' at line {}:{}", found, location.line, location.col)
      }
      LexerError::UnterminatedString(location) => {
        write!(f, "Unterminated string at line {}:{}", location.line, location.col)
      }
      LexerError::InvalidNumber(location, number) => {
        write!(f, "Invalid number '{}' at line {}:{}", number, location.line, location.col)
      }
      LexerError::UnexpectedEOF(location) => {
        write!(f, "Unexpected EOF at line {}:{}", location.line, location.col)
      }
      LexerError::Other(location, msg) => {
        write!(f, "Error at line {}:{}: {}", location.line, location.col, msg)
      }
    }
  }
}

impl ReportableError for &LexerError {
  fn get_col_pos(&self) -> (usize, usize) {
    match self {
      LexerError::InvalidCharacter(location, _) => (location.col, location.pos),
      LexerError::UnterminatedString(location) => (location.col, location.pos),
      LexerError::InvalidNumber(location, _) => (location.col, location.pos),
      LexerError::UnexpectedEOF(location) => (location.col, location.pos),
      LexerError::Other(location, _) => (location.col, location.pos),
    }
  }

  fn len(&self) -> usize {
    match self {
      LexerError::InvalidCharacter(_, _) => 1,
      LexerError::UnterminatedString(_) => 1,
      LexerError::InvalidNumber(_, _) => 1,
      LexerError::UnexpectedEOF(_) => 1,
      LexerError::Other(_, msg) => msg.len(),
    }
  }
}