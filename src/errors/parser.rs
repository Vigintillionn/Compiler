use std::fmt;

use crate::lexer::tokens::TokenKind;

use super::{ReportableError, Span};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub enum ParserError {
    InvalidExpression(Span),
    MissingToken(TokenKind, Span),
    MismatchedParantheses(Span),
    ExpectedIdentifier(Span),
    Other(String, Span), // TODO: improve the locations here
}

impl ParserError {
    fn pos(&self) -> (usize, usize) {
        match self {
            ParserError::InvalidExpression(span) => (span.start, span.end),
            ParserError::MissingToken(_, span) => (span.start, span.end),
            ParserError::MismatchedParantheses(span) => (span.start, span.end),
            ParserError::ExpectedIdentifier(span) => (span.start, span.end),
            ParserError::Other(_, span) => (span.start, span.end),
        }
    }

    fn rank(&self) -> usize {
        match self {
            ParserError::MissingToken(_, _) => 3,
            ParserError::InvalidExpression(_) => 2,
            ParserError::ExpectedIdentifier(_) => 2,
            ParserError::MismatchedParantheses(_) => 2,
            ParserError::Other(_, _) => 0,
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
        if self.rank() == other.rank() {
            return self.pos().cmp(&other.pos());
        }
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
            ParserError::InvalidExpression(_) => {
                write!(f, "Invalid expression")
            }
            ParserError::MissingToken(_, token) => {
                write!(f, "Missing token: {:?}", token)
            }
            ParserError::MismatchedParantheses(token) => {
                write!(f, "Mismatched parantheses: {:?}", token)
            }
            ParserError::ExpectedIdentifier(token) => {
                write!(f, "Expected identifier: {:?}", token)
            }
            _ => todo!(),
        }
    }
}

impl ReportableError for &ParserError {
    fn get_span(&self) -> &Span {
        match self {
            ParserError::InvalidExpression(span) => span,
            _ => todo!(),
        }
    }
}
