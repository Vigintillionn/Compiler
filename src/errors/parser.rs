use std::fmt;

use crate::{lexer::tokens::TokenKind, Token};

use super::{Location, ReportableError};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken(usize, usize, usize),   // (line, column, index)
    InvalidExpression(usize, usize, usize), // (line, column, index)
    InvalidStatement(usize, usize, usize),  // (line, column, index)
    MissingToken(Location, TokenKind),      // (line, column, index)
    MismatchedParantheses(usize, usize, usize), // (start, end)
    ExpectedIdentifier(Token),
    Other(String), // TODO: improve the locations here
}

impl ParserError {
    fn pos(&self) -> usize {
        match self {
            ParserError::UnexpectedToken(_, _, pos) => *pos,
            ParserError::InvalidExpression(_, _, pos) => *pos,
            ParserError::InvalidStatement(_, _, pos) => *pos,
            ParserError::MissingToken(location, _) => location.pos,
            ParserError::ExpectedIdentifier(token) => token.pos,
            ParserError::MismatchedParantheses(start, end, _) => std::cmp::max(*start, *end),
            ParserError::Other(_) => 0,
        }
    }

    fn rank(&self) -> usize {
        match self {
            ParserError::MissingToken(_, _) => 3,
            ParserError::InvalidExpression(_, _, _) => 2,
            ParserError::InvalidStatement(_, _, _) => 2,
            ParserError::ExpectedIdentifier(_) => 2,
            ParserError::MismatchedParantheses(_, _, _) => 2,
            ParserError::UnexpectedToken(_, _, _) => 1,
            ParserError::Other(_) => 0,
        }
    }
}

impl PartialEq for ParserError {
    fn eq(&self, other: &Self) -> bool {
        self.pos() == other.pos() && self.rank() == other.rank()
    }
}

impl Eq for ParserError {}

impl Ord for ParserError {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.pos(), self.rank()).cmp(&(other.pos(), other.rank()))
    }
}

impl PartialOrd for ParserError {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl ParserError {
    pub fn or(self, other: ParserError) -> ParserError {
        std::cmp::max(self, other)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(line, col, _) => {
                write!(f, "Unexpected token at line {}, column {}", line, col)
            }
            ParserError::InvalidExpression(line, col, _) => {
                write!(f, "Invalid expression at line {}, column {}", line, col)
            }
            ParserError::InvalidStatement(line, col, _) => {
                write!(f, "Invalid statement at line {}, column {}", line, col)
            }
            ParserError::MissingToken(location, token_kind) => {
                write!(
                    f,
                    "Missing token {:?} at line {}, column {}",
                    token_kind, location.line, location.col
                )
            }
            ParserError::ExpectedIdentifier(token) => {
                write!(f, "Expected identifier but found {:?}", token)
            }
            ParserError::MismatchedParantheses(start, end, _) => {
                write!(f, "Mismatched parentheses at {} and {}", start, end)
            }
            ParserError::Other(msg) => write!(f, "{}", msg),
        }
    }
}

impl ReportableError for &ParserError {
    fn get_col_pos(&self) -> (usize, usize) {
        (0, 0)
    }

    fn len(&self) -> usize {
        0
    }
}
