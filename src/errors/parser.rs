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
    fn rank(&self) -> usize {
        match self {
            // Less important
            ParserError::Other(_, _) => 0,
            // Semicolons, commas, etc
            ParserError::MissingToken(_, _) => 1,
            // Syntax level issues
            ParserError::InvalidExpression(_)
            | ParserError::ExpectedIdentifier(_)
            | ParserError::MismatchedParantheses(_) => 2,
        }
    }
}

impl PartialEq for ParserError {
    fn eq(&self, other: &Self) -> bool {
        self.get_span() == other.get_span() && self.rank() == other.rank()
    }
}

impl Eq for ParserError {}

impl Ord for ParserError {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_span = self.get_span();
        let other_span = other.get_span();

        self_span
            .end
            .cmp(&other_span.end)
            .then_with(|| self.rank().cmp(&other.rank())) // Break ties with rank
            .then_with(|| self_span.start.cmp(&other_span.start)) // Breka ranked ties with starting positions
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
        use ParserError::*;
        match self {
            InvalidExpression(span)
            | ExpectedIdentifier(span)
            | MissingToken(_, span)
            | MismatchedParantheses(span)
            | Other(_, span) => span,
        }
    }
}
