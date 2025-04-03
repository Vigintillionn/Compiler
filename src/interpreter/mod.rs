use core::panic;
use std::{collections::HashMap, fmt, rc::Rc};
use crate::parser::ast::{BinaryOp, Expr, ExprKind, LiteralValue, Op, Program, Stmt, Type, UnaryOp};

#[derive(Debug, Clone)]
pub enum EvalValue {
  Value(Value),
  Return(Value),
  Break,
  Continue
  // TODO: maybe add break and continue
}

impl EvalValue {
  fn into_value(self) -> Value {
    match self {
      EvalValue::Value(v) | EvalValue::Return(v) => v,
      EvalValue::Break | EvalValue::Continue => panic!("Cannot convert break to value"),
    }
  }
}

#[derive(Clone)]
pub struct NativeFunction(Rc<dyn Fn(Vec<Value>) -> Value>);

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
  fn eval(self, env: &mut Environment) -> EvalValue;
}

impl Eval for Program {
  fn eval(self, env: &mut Environment) -> EvalValue {
    self.0.into_iter().map(|stmt| stmt.eval(env)).last().unwrap_or(EvalValue::Value(Value::Void))
  }
}

impl Eval for Stmt {
  fn eval(self, env: &mut Environment) -> EvalValue {
    use Stmt::*;
    match self {
      Block(stmts) => {
        env.enter_scope();
        let mut result = EvalValue::Value(Value::Void);
        for stmt in stmts {
          result = stmt.eval(env);
          if let EvalValue::Return(_) = result {
            break;
          }
        }
        env.exit_scope().unwrap();
        result
      },
      Var(name, expr, _) => {
        let val = expr.eval(env).into_value();
        env.define(name, val);
        EvalValue::Value(Value::Void)
      },
      Assign(name, expr) => {
        let val = expr.eval(env).into_value();
        env.assign(&name, val).unwrap_or_else(|err| panic!("{}", err)); // TODO: handle error
        EvalValue::Value(Value::Void)
      },
      If(cond, then_block, else_block) => {
        let cond = cond.eval(env).into_value();
        if cond.is_truthy() {
          then_block.eval(env)
        } else if let Some(else_block) = else_block {
          else_block.eval(env)
        } else {
          EvalValue::Value(Value::Void)
        }
      },
      Function(name, args, ret, body) => {
        env.define(name, Value::Function(args, ret, *body));
        EvalValue::Value(Value::Void)
      },
      Expr(expr) => expr.eval(env),
      Ret(expr) =>{
        //  expr.map_or(Value::Void, |expr| expr.eval(env))
        let val = expr.map_or(Value::Void, |expr| {
          match expr.eval(env) {
            EvalValue::Return(v) | EvalValue::Value(v) => v,
            _ => panic!("Invalid return value"),
          }
        });
        EvalValue::Return(val)
      },
      Loop(init, cond, incr, block) => {
        env.enter_scope();
        if let Some(init) = init {
          init.eval(env);
        }

        let mut result = EvalValue::Value(Value::Void);
        'outer: loop {
          let cond = cond.as_ref().map(|c| c.clone().eval(env).into_value());
          if let Some(cond) = cond {
            if !cond.is_truthy() {
              break;
            }
          }
          
          if let Stmt::Block(ref block) = *block {
            for stmt in block.clone() {
              result = stmt.eval(env);
              if let EvalValue::Return(_) = result {
                break 'outer;
              }
              if let EvalValue::Break = result {
                break 'outer;
              }
              if let EvalValue::Continue = result {
                if let Some(incr) = incr.clone() {
                  incr.eval(env);
                }
                continue 'outer;
              }
            }
          }

          if let Some(incr) = incr.clone() {
            incr.eval(env);
          }
        }

        println!("Loop result: {:?}", result);

        result
      },
      Break => EvalValue::Break,
      Continue => EvalValue::Continue,
      _ => panic!("{:?} unimplemented", self),
    }
  }
}

impl Eval for Expr {
  fn eval(self, env: &mut Environment) -> EvalValue {
    use ExprKind::*;
    match self.0 {
      Literal(value) => value.eval(env),
      Ident(name) => EvalValue::Value(env.get(&name).unwrap_or_else(|err| panic!("{}", err))), // TODO: handle error
      BinaryOp(bin) => bin.eval(env),
      UnaryOp(un) => un.eval(env),
      Call(name, args) => {
        let func = env.get(&name).unwrap_or_else(|err| panic!("{}", err)); // TODO: handle error
        match func {
          Value::Function(params, _, body) => {
            env.enter_scope();
            for (param, arg) in params.iter().zip(args) {
              let val = arg.eval(env).into_value();
              env.define(param.0.clone(), val);
            }

            let res = body.eval(env);
            env.exit_scope().unwrap();
            EvalValue::Value(res.into_value())
          },
          Value::NativeFunction(func) => EvalValue::Value(func.0(
            args.into_iter()
              .map(|arg| arg.eval(env).into_value())
              .collect())
          ),
          _ => unimplemented!(), // TODO: return error with uncallable type
        }
      }
    }
  }
}

impl Eval for BinaryOp {
  fn eval(self, env: &mut Environment) -> EvalValue {
    let Self (lhs, op, rhs) = self;
    let lhs = lhs.eval(env).into_value();
    let rhs = rhs.eval(env).into_value();

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
      Op::Eq => Value::Boolean(lhs == rhs),
      Op::Neq => Value::Boolean(lhs != rhs),
      Op::Gt => Value::Boolean(lhs > rhs),
      Op::Gte => Value::Boolean(lhs >= rhs),
      Op::Lt => Value::Boolean(lhs < rhs),
      Op::Lte => Value::Boolean(lhs <= rhs),

      Op::And => Value::Boolean(lhs.is_truthy() && rhs.is_truthy()),
      Op::Or => Value::Boolean(lhs.is_truthy() || rhs.is_truthy()),

      _ => panic!("This is not a valid binary operator!"),
    };

    EvalValue::Value(res)
  }
}

impl Eval for UnaryOp {
  fn eval(self, env: &mut Environment) -> EvalValue {
    let Self(op, expr) = self;
    let val = expr.eval(env).into_value();

    let res = match op {
      Op::Not => Value::Boolean(!val.is_truthy()),
      _ => unimplemented!(),
    };

    EvalValue::Value(res)
  }
}

impl Eval for LiteralValue {
  fn eval(self, _: &mut Environment) -> EvalValue {
    use LiteralValue::*;
    let v = match self {
      Integer(value) => Value::Int(value),
      Float(value) => Value::Float(value),
      String(value) => Value::String(value),
      Boolean(value) => Value::Boolean(value),
    };

    EvalValue::Value(v)
  }
}