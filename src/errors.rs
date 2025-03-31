pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
  InvalidCharacter(char, usize), // (char, index)
  UnterminatedString(usize),     // index
  InvalidNumber(String, usize),          // index
  UnexpectedEOF,
  Other(String),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {}