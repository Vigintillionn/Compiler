use crate::{
    environment::Environment,
    parser::ast::{self, Expr, ExprKind, LiteralValue, Op, Program, Stmt, Type, UnaryOp},
};

pub type TypeEnv = Environment<Type>;

pub trait TypeCheck {
    fn type_check(&self, env: &mut TypeEnv, expected: &Option<Type>) -> Result<Type, String>;
}

impl TypeCheck for Expr {
    fn type_check(&self, env: &mut TypeEnv, _: &Option<Type>) -> Result<Type, String> {
        use ExprKind::*;
        match &self.0 {
            Literal(val) => match val {
                LiteralValue::Integer(_) => Ok(Type::Int),
                LiteralValue::Float(_) => Ok(Type::Float),
                LiteralValue::String(_) => Ok(Type::String),
                LiteralValue::Boolean(_) => Ok(Type::Boolean),
            },
            Ident(ref id) => {
                if let Some(ty) = env.get(id) {
                    *self.1.borrow_mut() = Some(ty.clone());
                    Ok(ty.clone())
                } else {
                    Err(format!("Undefined variable: {}", id))
                }
            }
            BinaryOp(ast::BinaryOp(left, op, right)) => {
                use Op::*;
                let left = left.type_check(env, &None)?;
                let right = right.type_check(env, &None)?;
                let res = match op {
                    Add => {
                        if left == right && matches!(left, Type::Int | Type::Float | Type::String) {
                            left
                        } else {
                            return Err(format!("Type mismatch: {:?} and {:?}", left, right));
                        }
                    }
                    Sub | Mul | Div => {
                        if left == right && matches!(left, Type::Int | Type::Float) {
                            left
                        } else {
                            return Err(format!("Type mismatch: {:?} and {:?}", left, right));
                        }
                    }
                    Eq | Neq => {
                        if left == right {
                            Type::Boolean
                        } else {
                            return Err(format!("Type mismatch: {:?} and {:?}", left, right));
                        }
                    }
                    Lt | Lte | Gt | Gte => {
                        if left == right && matches!(left, Type::Int | Type::Float) {
                            Type::Boolean
                        } else {
                            return Err(format!("Type mismatch: {:?} and {:?}", left, right));
                        }
                    }
                    And | Or => {
                        if left == Type::Boolean && right == Type::Boolean {
                            Type::Boolean
                        } else {
                            return Err(format!("Type mismatch: {:?} and {:?}", left, right));
                        }
                    }
                    _ => {
                        return Err(format!("Unsupported operator: {:?}", op));
                    }
                };
                *self.1.borrow_mut() = Some(res.clone());
                Ok(res)
            }
            UnaryOp(ast::UnaryOp(op, expr)) => {
                use Op::*;
                let expr_type = expr.type_check(env, &None)?;
                let res = match op {
                    Not => {
                        if expr_type == Type::Boolean {
                            Type::Boolean
                        } else {
                            return Err(format!("Type mismatch: {:?}", expr_type));
                        }
                    }
                    AddressOf => Type::Pointer(Box::new(expr_type)),
                    Deref => Type::Int, // TODO: Fix
                    _ => {
                        return Err(format!("Unsupported operator: {:?}", op));
                    }
                };
                *self.1.borrow_mut() = Some(res.clone());
                Ok(res)
            }
            Call(name, args) => {
                let func_type = match env.get(name) {
                    Some(ty) => ty,
                    None => return Err(format!("Undefined function: {}", name)),
                };
                match func_type.clone() {
                    Type::Function(args_types, ret_type) => {
                        if args.len() != args_types.len() {
                            return Err(format!(
                                "Function {} expects {} arguments, found {}",
                                name,
                                args_types.len(),
                                args.len()
                            ));
                        }

                        // for (arg, expected_type) in args.iter().zip(args_types.iter()) {
                        //     let arg_type = arg.type_check(env, &None)?;
                        //     if &arg_type != expected_type {
                        //         return Err(format!(
                        //             "Type mismatch: expected {:?}, found {:?}",
                        //             expected_type, arg_type
                        //         ));
                        //     }
                        // }
                        *self.1.borrow_mut() = Some(*ret_type.clone());
                        Ok(*ret_type.clone())
                    }
                    _ => {
                        return Err(format!("{} is not a function", name));
                    }
                }
            }
        }
    }
}

impl TypeCheck for Stmt {
    fn type_check(&self, env: &mut TypeEnv, expected: &Option<Type>) -> Result<Type, String> {
        use Stmt::*;
        match self {
            Block(stmts) => {
                env.enter_scope();
                for stmt in stmts {
                    stmt.type_check(env, expected)?;
                }
                env.exit_scope();
                Ok(Type::Void)
            }
            Var(name, expr, ty) => {
                let expr_type = expr.type_check(env, expected)?;
                {
                    if let Some(ty) = ty.borrow().as_ref() {
                        if ty != &expr_type {
                            return Err(format!(
                                "Type mismatch: expected {:?}, found {:?}",
                                ty, expr_type
                            ));
                        }
                    }
                }
                if ty.borrow().is_none() {
                    *ty.borrow_mut() = Some(expr_type.clone());
                }
                env.define(name.clone(), expr_type.clone());
                Ok(Type::Void)
            }
            Assign(lhs, rhs) => {
                let rhs_type = rhs.type_check(env, expected)?;
                let name = match &lhs.0 {
                    ExprKind::Ident(name) => name,
                    ExprKind::UnaryOp(UnaryOp(Op::Deref, expr)) => match &(**expr).0 {
                        ExprKind::Ident(name) => name,
                        _ => panic!("Can't dereference this."),
                    },
                    _ => panic!("Can't assign to this."),
                };

                // TODO: Fix for assigning to pointers
                // if let Some(ty) = env.get(name) {
                //     if ty != rhs_type {
                //         return Err(format!(
                //             "Type mismatch: expected {:?}, found {:?}",
                //             ty, rhs_type
                //         ));
                //     }
                // } else {
                //     return Err(format!("Undefined variable: {}", name));
                // }
                Ok(Type::Void)
            }
            If(cond, then_stmt, else_stmt) => {
                let cond_type = cond.type_check(env, expected)?;
                if cond_type != Type::Boolean {
                    return Err(format!("Condition must be boolean, found {:?}", cond_type));
                }
                then_stmt.type_check(env, expected)?;
                if let Some(else_stmt) = else_stmt {
                    else_stmt.type_check(env, expected)?;
                }
                Ok(Type::Void)
            }
            Expr(expr) => {
                let ty = expr.type_check(env, expected)?;
                *expr.1.borrow_mut() = Some(ty.clone());
                Ok(ty)
            }
            Loop(init, cond, incr, body) => {
                env.enter_scope();
                if let Some(init) = init {
                    init.type_check(env, expected)?;
                }
                if let Some(cond) = cond {
                    let cond_type = cond.type_check(env, expected)?;
                    if cond_type != Type::Boolean {
                        return Err(format!("Condition must be boolean, found {:?}", cond_type));
                    }
                }
                if let Some(incr) = incr {
                    incr.type_check(env, expected)?;
                }
                body.type_check(env, expected)?;
                env.exit_scope();
                Ok(Type::Void)
            }
            Ret(expr) => {
                let ret_type = if let Some(expr) = expr {
                    expr.type_check(env, expected)?
                } else {
                    Type::Void
                };

                if let Some(expected) = expected {
                    if ret_type != *expected {
                        return Err(format!(
                            "Return type mismatch: expected {:?}, found {:?}",
                            expected, ret_type
                        ));
                    }
                }

                Ok(ret_type)
            }
            Function(name, args, ret_type, body) => {
                let arg_types = args.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
                let func_type = Type::Function(arg_types.clone(), Box::new(ret_type.clone()));

                env.define(name.clone(), func_type.clone());
                env.enter_scope();
                for (arg_name, arg_type) in args {
                    env.define(arg_name.clone(), arg_type.clone());
                }

                body.type_check(env, &Some(ret_type.clone()))?;
                env.exit_scope();

                Ok(Type::Void)
            }
            Continue | Break => Ok(Type::Void),
        }
    }
}

pub fn type_check_program(program: &Program) -> Result<(), String> {
    let mut env = TypeEnv::new();
    env.define(
        "print".to_string(),
        Type::Function(vec![Type::Int], Box::new(Type::Void)),
    );
    for stmt in &program.0 {
        stmt.type_check(&mut env, &None)?;
    }
    Ok(())
}
