use std::{cell::RefCell, fmt, rc::Rc};

use crate::parser::ast::{Stmt, Type};

#[derive(Debug, Clone)]
pub enum EvalValue {
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

impl EvalValue {
    pub fn into_value(self) -> Value {
        match self {
            EvalValue::Value(v) | EvalValue::Return(v) => v,
            EvalValue::Break | EvalValue::Continue => panic!("Cannot convert break to value"),
        }
    }
}

#[derive(Clone)]
pub struct NativeFunction(pub fn(Vec<Value>) -> Value);

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFunction")
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Void,
    Pointer(Rc<RefCell<Value>>),
    Function(Vec<(String, Type)>, Type, Stmt),
    NativeFunction(NativeFunction),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(val) => *val,
            Value::Int(val) => val != &0,
            Value::Float(val) => val != &0.0,
            Value::String(val) => !val.is_empty(),
            Value::Void => false,
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs.cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs).unwrap(),
            (Value::String(lhs), Value::String(rhs)) => lhs.cmp(rhs),
            _ => panic!("Cannot compare values"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Boolean(val) => write!(f, "{}", val),
            Value::Void => write!(f, "void"),
            Value::Pointer(val) => write!(f, "Pointer({})", val.borrow()),
            Value::Function(params, ret, _) => write!(
                f,
                "proc({}) -> {:?}",
                params
                    .iter()
                    .map(|(n, t)| format!("{}: {:?}", n, t))
                    .collect::<Vec<String>>()
                    .join(", "),
                ret
            ),
            Value::NativeFunction(_) => write!(f, "<native function>"),
        }
    }
}
