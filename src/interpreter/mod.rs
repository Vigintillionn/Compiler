use nativefunctions::print;

use crate::{
    environment::Environment,
    parser::ast::{BinaryOp, Expr, ExprKind, LiteralValue, Op, Program, Stmt, UnaryOp},
};
use core::panic;
use std::{cell::RefCell, rc::Rc};
use values::{EvalValue, NativeFunction, Value};

mod nativefunctions;
mod values;

type EvalEnvironment = Environment<Rc<RefCell<Value>>>;

trait Eval {
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue;
}

impl Eval for Program {
    #[allow(clippy::double_ended_iterator_last)]
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue {
        self.0
            .into_iter()
            .map(|stmt| stmt.eval(env))
            .last()
            .unwrap_or(EvalValue::Value(Value::Void))
    }
}

impl Eval for Stmt {
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue {
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
            }
            Var(name, expr, _) => {
                let val = expr.eval(env).into_value();
                env.define(name, Rc::new(RefCell::new(val)));
                EvalValue::Value(Value::Void)
            }
            Assign(lhs, rhs) => {
                let val = rhs.eval(env).into_value();

                match &lhs.0 {
                    ExprKind::Ident(ref name) => {
                        env.assign(&name, Rc::new(RefCell::new(val.clone())))
                            .unwrap_or_else(|err| panic!("{}", err));
                        EvalValue::Value(Value::Void)
                    }
                    ExprKind::UnaryOp(UnaryOp(Op::Deref, ref expr)) => {
                        let pointer_val = expr.clone().eval(env).into_value(); // TODO: make this cleaner, preferably without cloning
                        match pointer_val {
                            Value::Pointer(ref ptr) => {
                                *ptr.borrow_mut() = val.clone();
                                EvalValue::Value(Value::Void)
                            }
                            _ => panic!("Dereference assignment on non-pointer value"),
                        }
                    }
                    _ => panic!("Can't assign to this"),
                }
            }
            If(cond, then_block, else_block) => {
                let cond = cond.eval(env).into_value();
                if cond.is_truthy() {
                    then_block.eval(env)
                } else if let Some(else_block) = else_block {
                    else_block.eval(env)
                } else {
                    EvalValue::Value(Value::Void)
                }
            }
            Function(name, args, ret, body) => {
                env.define(
                    name,
                    Rc::new(RefCell::new(Value::Function(args, ret, *body))),
                );
                EvalValue::Value(Value::Void)
            }
            Expr(expr) => expr.eval(env),
            Ret(expr) => {
                //  expr.map_or(Value::Void, |expr| expr.eval(env))
                let val = expr.map_or(Value::Void, |expr| match expr.eval(env) {
                    EvalValue::Return(v) | EvalValue::Value(v) => v,
                    _ => panic!("Invalid return value"),
                });
                EvalValue::Return(val)
            }
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
                            if matches!(result, EvalValue::Return(_) | EvalValue::Break) {
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

                result
            }
            Break => EvalValue::Break,
            Continue => EvalValue::Continue,
        }
    }
}

impl Eval for Expr {
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue {
        use ExprKind::*;
        match self.0 {
            Literal(value) => value.eval(env),
            Ident(name) => EvalValue::Value(
                env.get(&name)
                    .ok_or_else(|| panic!("Something went wrong"))
                    .unwrap()
                    .borrow_mut()
                    .to_owned(),
            ), // TODO: handle error
            BinaryOp(bin) => bin.eval(env),
            UnaryOp(un) => un.eval(env),
            Call(name, args) => {
                let func = env
                    .get(&name)
                    .ok_or_else(|| panic!("Something went wrong"))
                    .unwrap()
                    .borrow_mut()
                    .to_owned(); // TODO: handle error
                match func {
                    Value::Function(params, _, body) => {
                        env.enter_scope();
                        for (param, arg) in params.iter().zip(args) {
                            let val = arg.eval(env).into_value();
                            env.define(param.0.clone(), Rc::new(RefCell::new(val)));
                        }

                        let res = body.eval(env);
                        env.exit_scope().unwrap();
                        EvalValue::Value(res.into_value())
                    }
                    Value::NativeFunction(func) => EvalValue::Value(func.0(
                        args.into_iter()
                            .map(|arg| arg.eval(env).into_value())
                            .collect(),
                    )),
                    _ => unimplemented!(), // TODO: return error with uncallable type
                }
            }
        }
    }
}

impl Eval for BinaryOp {
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue {
        let Self(lhs, op, rhs) = self;
        let lhs = lhs.eval(env).into_value();
        let rhs = rhs.eval(env).into_value();

        let res = match op {
            Op::Add => match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
                (Value::String(lhs), Value::String(rhs)) => {
                    Value::String(format!("{}{}", lhs, rhs))
                }
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
    fn eval(self, env: &mut EvalEnvironment) -> EvalValue {
        let Self(op, expr) = self;

        let res = match op {
            Op::Not => {
                let val = expr.eval(env).into_value();
                Value::Boolean(!val.is_truthy())
            }
            Op::AddressOf => match &expr.0 {
                ExprKind::Ident(name) => {
                    let pointer = env
                        .get(&name)
                        .unwrap_or_else(|| panic!("Undefined variable"));
                    Value::Pointer(pointer)
                }
                _ => panic!("Address-of operator can only be applied to variables"),
            },
            Op::Deref => {
                let val = expr.eval(env).into_value();
                match val {
                    Value::Pointer(ptr) => ptr.borrow().to_owned(),
                    _ => panic!("Deref operator applied to a non-pointer value"),
                }
            }
            _ => unimplemented!(),
        };

        EvalValue::Value(res)
    }
}

impl Eval for LiteralValue {
    fn eval(self, _: &mut EvalEnvironment) -> EvalValue {
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

pub fn eval_program(program: Program) -> Value {
    let mut env = EvalEnvironment::new();
    env.define(
        "print".to_string(),
        Rc::new(RefCell::new(Value::NativeFunction(NativeFunction(print)))),
    );
    program.eval(&mut env).into_value()
}
