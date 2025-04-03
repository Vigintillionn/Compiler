pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
  InvalidCharacter(char, usize, usize, usize), // (char, index)
  UnterminatedString(usize, usize, usize),     // index
  InvalidNumber(String, usize, usize, usize),          // index
  UnexpectedEOF(usize, usize),
  Other(String),
}

impl std::fmt::Display for LexerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LexerError::InvalidCharacter(c, line, col, src) => {
        write!(f, "Invalid character '{}' at line {}, column {}: {}", c, line, col, src)
      }
      LexerError::UnterminatedString(line, col, src) => {
        write!(f, "Unterminated string at line {}, column {}: {}", line, col, src)
      }
      LexerError::InvalidNumber(num, line, col, src) => {
        write!(f, "Invalid number '{}' at line {}, column {}: {}", num, line, col, src)
      }
      LexerError::UnexpectedEOF(line, col) => {
        write!(f, "Unexpected EOF at line {}, column {}", line, col)
      }
      LexerError::Other(msg) => {
        write!(f, "{}", msg)
      }
    }
  }
}

impl std::error::Error for LexerError {}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {}