use core::panic;
use std::{collections::HashMap, fmt, rc::Rc};
use crate::parser::ast::{BinaryOp, Expr, ExprKind, LiteralValue, Op, Program, Stmt, Type};

#[derive(Clone)]
struct NativeFunction(Rc<dyn Fn(Vec<Value>) -> Value>);

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
  Pointer(Box<Value>),
  Function(Vec<(String, Type)>, Type, Stmt),
  NativeFunction(NativeFunction)
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

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Value::Int(val) => write!(f, "{}", val),
      Value::Float(val) => write!(f, "{}", val),
      Value::String(val) => write!(f, "{}", val),
      Value::Boolean(val) => write!(f, "{}", val),
      Value::Void => write!(f, "void"),
      Value::Pointer(val) => write!(f, "Pointer({})", *val),
      Value::Function(params, ret, _) => write!(f, "proc({}) -> {:?}", 
        params.iter()
          .map(|(n, t)| format!("{}: {:?}", n, t))
          .collect::<Vec<String>>()
          .join(", "),
        ret
      ),
      Value::NativeFunction(_) => write!(f, "<native function>"),
    }
  }
}

#[derive(Debug)]
pub struct Environment {
  scopes: Vec<HashMap<String, Value>>
}

impl Environment {
  pub fn new() -> Self {
    let mut env = Self {
      scopes: vec![HashMap::new()]
    };

    env.define(
      "print".to_string(),
    Value::NativeFunction(NativeFunction(Rc::new(|args: Vec<Value>| {
        let mut output = String::new();
        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            output.push_str(", ");
          }

          output.push_str(&format!("{}", arg));
        }
        println!("{}", output);
        Value::Void
      })))
    );

    env
  }

  pub fn enter_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn exit_scope(&mut self) -> Result<(), String> {
    if self.scopes.len() == 1 {
      Err("Cannot exit the global scope".to_string())
    } else {
      self.scopes.pop();
      Ok(())
    }
  }

  pub fn define(&mut self, name: String, val: Value) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name, val);
    }
  }

  pub fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
    for scope in self.scopes.iter_mut().rev() {
      if scope.contains_key(name) {
        scope.insert(name.to_string(), value);
        return Ok(());
      }
    }

    Err(format!("Undefined variable: {}", name))
  }

  pub fn get(&self, name: &str) -> Result<Value, String> {
    for scope in self.scopes.iter().rev() {
      if let Some(val) = scope.get(name) {
        return Ok(val.clone());
      }
    }

    Err(format!("Undefined variable: {}", name))
  }
}

pub trait Eval {
  fn eval(self, env: &mut Environment) -> Value;
}

impl Eval for Program {
  fn eval(self, env: &mut Environment) -> Value {
    self.0.into_iter().map(|stmt| stmt.eval(env)).last().unwrap_or(Value::Void)
  }
}

impl Eval for Stmt {
  fn eval(self, env: &mut Environment) -> Value {
    use Stmt::*;
    match self {
      Block(stmts) => {
        env.enter_scope();
        let res = stmts
          .into_iter()
          .map(|stmt| stmt.eval(env))
          .last()
          .unwrap_or(Value::Void); // TODO: handle early return
        env.exit_scope().unwrap();
        res
      },
      Var(name, expr, _) => {
        let val = expr.eval(env);
        env.define(name, val);
        Value::Void
      },
      Assign(name, expr) => {
        let val = expr.eval(env);
        env.assign(&name, val).unwrap_or_else(|err| panic!("{}", err)); // TODO: handle error
        Value::Void
      },
      If(cond, then_block, else_block) => {
        let cond = cond.eval(env);
        if cond.is_truthy() {
          then_block.eval(env)
        } else if let Some(else_block) = else_block {
          else_block.eval(env)
        } else {
          Value::Void
        }
      },
      Function(name, args, ret, body) => {
        env.define(name, Value::Function(args, ret, *body));
        Value::Void
      },
      Expr(expr) => expr.eval(env),
      Ret(expr) => expr.map_or(Value::Void, |expr| expr.eval(env)),
      _ => panic!("{:?} unimplemented", self),
    }
  }
}

impl Eval for Expr {
  fn eval(self, env: &mut Environment) -> Value {
    use ExprKind::*;
    match self.0 {
      Literal(value) => value.eval(env),
      Ident(name) => env.get(&name).unwrap_or_else(|err| panic!("{}", err)), // TODO: handle error
      BinaryOp(bin) => bin.eval(env),
      Call(name, args) => {
        let func = env.get(&name).unwrap_or_else(|err| panic!("{}", err)); // TODO: handle error
        match func {
          Value::Function(params, _, body) => {
            env.enter_scope();
            for (param, arg) in params.iter().zip(args) {
              let val = arg.eval(env);
              env.define(param.0.clone(), val);
            }

            let res = body.eval(env);
            env.exit_scope().unwrap();
            res
          },
          Value::NativeFunction(func) => func.0(
            args.into_iter()
              .map(|arg| arg.eval(env))
              .collect()
          ),
          _ => unimplemented!(),
        }
      }
      _ => unimplemented!(),
    }
  }
}

impl Eval for BinaryOp {
  fn eval(self, env: &mut Environment) -> Value {
    let Self (lhs, op, rhs) = self;
    let lhs = lhs.eval(env);
    let rhs = rhs.eval(env);

    let res = match op {
      Op::Add => match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
        (Value::String(lhs), Value::String(rhs)) => Value::String(format!("{}{}", lhs, rhs)),
        _ => unimplemented!(),
      },
      Op::Sub => match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
        _ => unimplemented!(),
      },
      Op::Mul => match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
        _ => unimplemented!(),
      },
      Op::Div => match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
        _ => unimplemented!(),
      },
    };

    res
  }
}

impl Eval for LiteralValue {
  fn eval(self, _: &mut Environment) -> Value {
    use LiteralValue::*;
    match self {
      Integer(value) => Value::Int(value),
      Float(value) => Value::Float(value),
      String(value) => Value::String(value),
      Boolean(value) => Value::Boolean(value),
    }
  }
}