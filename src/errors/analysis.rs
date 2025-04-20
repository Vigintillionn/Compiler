use std::fmt;

use crate::parser::ast::{Op, Type};

use super::{ReportableError, Span};

pub enum AnalysisError {
    UndefinedVariable(String, Span),
    UndefinedFunction(String, Span),
    TypeMismatch(Type, Type, Span),       // Expected type, actual type
    ReturnTypeMismatch(Type, Type, Span), // Expected type, actual type
    IncompatibleTypesBin(Op, Type, Type, Span), // Left type, right type
    IncompatibleTypesUn(Op, Type, Span),  // Left type, right type
    UnsupportedOperation(Op, Span),       // Left type, right type
    WrongArity(usize, usize, String, Span), // Expected arity, actual arity, function name
    UncallableType(Type, Span),           // Type, span
    Other(String, Span),
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalysisError::UndefinedVariable(name, _) => {
                write!(f, "Undefined variable '{}'", name)
            }
            AnalysisError::UndefinedFunction(name, _) => {
                write!(f, "Undefined function '{}'", name)
            }
            AnalysisError::TypeMismatch(expected, actual, _) => {
                write!(
                    f,
                    "Type mismatch: expected {:?}, found {:?}",
                    expected, actual
                )
            }
            AnalysisError::IncompatibleTypesBin(op, left, right, _) => {
                write!(
                    f,
                    "Incompatible types for operation '{:?}': left type {:?}, right type {:?}",
                    op, left, right
                )
            }
            AnalysisError::IncompatibleTypesUn(op, left, _) => {
                write!(
                    f,
                    "Incompatible types for operation '{:?}': type {:?}",
                    op, left
                )
            }
            AnalysisError::ReturnTypeMismatch(expected, actual, _) => {
                write!(
                    f,
                    "Return type mismatch: expected {:?}, found {:?}",
                    expected, actual
                )
            }
            AnalysisError::UnsupportedOperation(op, _) => {
                write!(f, "Unsupported operation '{:?}'", op)
            }
            AnalysisError::WrongArity(expected, actual, name, _) => {
                write!(
                    f,
                    "Wrong arity for function '{}': expected {} arguments, found {}",
                    name, expected, actual
                )
            }
            AnalysisError::UncallableType(typ, _) => {
                write!(f, "Uncallable type '{:?}'", typ)
            }
            AnalysisError::Other(msg, _) => {
                write!(f, "{}", msg)
            }
        }
    }
}

impl ReportableError for &AnalysisError {
    fn get_span(&self) -> &Span {
        match self {
            AnalysisError::UndefinedVariable(_, span) => span,
            AnalysisError::UndefinedFunction(_, span) => span,
            AnalysisError::TypeMismatch(_, _, span) => span,
            AnalysisError::IncompatibleTypesBin(_, _, _, span) => span,
            AnalysisError::IncompatibleTypesUn(_, _, span) => span,
            AnalysisError::ReturnTypeMismatch(_, _, span) => span,
            AnalysisError::UnsupportedOperation(_, span) => span,
            AnalysisError::WrongArity(_, _, _, span) => span,
            AnalysisError::UncallableType(_, span) => span,
            AnalysisError::Other(_, span) => span,
        }
    }
}
