#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  Uint,
  Int,
  String,
  Bool,
  Pointer(Box<Type>),
  FSignature(Vec<Type>, Box<Type>),   // Function signature (args, return type)
  Void,                               // No return type
}