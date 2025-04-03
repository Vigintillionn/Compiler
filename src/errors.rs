pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
  InvalidCharacter(char, usize, usize, usize), // (char, index)
  UnterminatedString(usize, usize, usize),     // index
  InvalidNumber(String, usize, usize, usize),          // index
  UnexpectedEOF(usize, usize),
  Other(String),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {}