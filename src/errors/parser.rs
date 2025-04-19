use std::fmt;

use crate::lexer::tokens::TokenKind;

use super::{ReportableError, Span};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub enum ParserError {
    InvalidExpression(Span),
    MissingToken(TokenKind, TokenKind, Span),
    MismatchedParantheses(Span),
    ExpectedIdentifier(TokenKind, Span),
    Other(String, Span), // TODO: improve the locations here
}

impl ParserError {
    fn rank(&self) -> usize {
        match self {
            // Less important
            ParserError::Other(_, _) => 0,
            // Semicolons, commas, etc
            ParserError::MissingToken(_, _, _) => 1,
            // Syntax level issues
            ParserError::InvalidExpression(_)
            | ParserError::ExpectedIdentifier(_, _)
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
            ParserError::MissingToken(expected, got, _) => {
                write!(f, "Expected '{}' but got '{}'", expected, got)
            }
            ParserError::MismatchedParantheses(_) => {
                write!(f, "Mismatched parantheses")
            }
            ParserError::ExpectedIdentifier(token, _) => {
                write!(f, "Expected identifier but got '{}'", token)
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
            | ExpectedIdentifier(_, span)
            | MissingToken(_, _, span)
            | MismatchedParantheses(span)
            | Other(_, span) => span,
        }
    }
}
