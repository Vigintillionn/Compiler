use std::fmt;

use crate::parser::ast::{Op, Type};

use super::{ReportableError, Span};

pub enum AnalysisError {
    // Type Checking
    UndefinedVariable(String, Span),
    UndefinedFunction(String, Span),
    TypeMismatch(Type, Type, Span), // Expected type, actual type
    ReturnTypeMismatch(String, Type, Type, Span), // Expected type, actual type
    IncompatibleTypesBin(Op, Type, Type, Span), // Left type, right type
    IncompatibleTypesUn(Op, Type, Span), // Left type, right type
    UnsupportedOperation(Op, Span), // Left type, right type
    WrongArity(usize, usize, String, Span), // Expected arity, actual arity, function name
    UncallableType(Type, Span),     // Type, span
    // Control Flow
    Unreachable(Span),
    FunctionNoReturnAllPaths(String, Span),

    Other(String, Span),
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use AnalysisError::*;
        match self {
            UndefinedVariable(name, _) => {
                write!(f, "Undefined variable '{}'", name)
            }
            UndefinedFunction(name, _) => {
                write!(f, "Undefined function '{}'", name)
            }
            TypeMismatch(expected, actual, _) => {
                write!(
                    f,
                    "Type mismatch: expected '{}', found '{}'",
                    expected, actual
                )
            }
            IncompatibleTypesBin(op, left, right, _) => {
                write!(
                    f,
                    "Incompatible types for binary operator '{}': left type '{}', right type '{}'",
                    op, left, right
                )
            }
            IncompatibleTypesUn(op, left, _) => {
                write!(
                    f,
                    "Incompatible type for unary operator '{}': type '{}'",
                    op, left
                )
            }
            ReturnTypeMismatch(name, expected, actual, _) => {
                write!(
                    f,
                    "Function '{}' returns type '{}' but got '{}'",
                    name, expected, actual
                )
            }
            UnsupportedOperation(op, _) => {
                write!(f, "Unsupported operation '{}'", op)
            }
            WrongArity(expected, actual, name, _) => {
                write!(
                    f,
                    "Wrong arity for function '{}': expected {} arguments, found {}",
                    name, expected, actual
                )
            }
            UncallableType(typ, _) => {
                write!(f, "Uncallable type '{}'", typ)
            }
            Unreachable(_) => {
                write!(f, "Unreachable code")
            }
            FunctionNoReturnAllPaths(name, _) => {
                write!(f, "Function '{}' may not return on all paths", name)
            }

            Other(msg, _) => {
                write!(f, "{}", msg)
            }
        }
    }
}

impl ReportableError for AnalysisError {
    fn get_span(&self) -> &Span {
        use AnalysisError::*;
        match self {
            UndefinedVariable(_, span)
            | UndefinedFunction(_, span)
            | TypeMismatch(_, _, span)
            | IncompatibleTypesBin(_, _, _, span)
            | IncompatibleTypesUn(_, _, span)
            | ReturnTypeMismatch(_, _, _, span)
            | UnsupportedOperation(_, span)
            | WrongArity(_, _, _, span)
            | UncallableType(_, span)
            | Unreachable(span)
            | FunctionNoReturnAllPaths(_, span)
            | Other(_, span) => span,
        }
    }
}
