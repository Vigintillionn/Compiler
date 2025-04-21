use std::fmt;

use super::{ReportableError, Span};

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidCharacter(char, Span),
    InvalidNumber(String, Span),
    UnterminatedString(Span),
    UnexpectedEOF(Span),
    Other(String, Span),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::InvalidCharacter(c, _) => {
                write!(f, "Invalid character '{}'", c)
            }
            LexerError::InvalidNumber(num, _) => {
                write!(f, "Invalid number '{}'", num)
            }
            LexerError::UnexpectedEOF(_) => {
                write!(f, "Unexpected end of file")
            }
            LexerError::UnterminatedString(_) => {
                write!(f, "Unterminated string literal")
            }
            LexerError::Other(msg, _) => {
                write!(f, "Lexer error: {}", msg)
            }
        }
    }
}

impl ReportableError for LexerError {
    fn get_span(&self) -> &Span {
        match self {
            LexerError::InvalidCharacter(_, span) => span,
            LexerError::InvalidNumber(_, span) => span,
            LexerError::UnexpectedEOF(span) => span,
            LexerError::UnterminatedString(span) => span,
            LexerError::Other(_, span) => span,
        }
    }
}
