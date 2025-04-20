use crate::{
    environment::Environment,
    errors::analysis::AnalysisError,
    parser::ast::{self, Expr, ExprKind, LiteralValue, Op, Stmt, StmtKind, Type, UnaryOp},
};

pub type TypeEnv = Environment<Type>;

pub struct Expected {
    func: String,
    ty: Type,
}

pub trait TypeCheck {
    fn type_check(
        &self,
        env: &mut TypeEnv,
        expected: &Option<Expected>,
    ) -> Result<Type, AnalysisError>;
}

impl TypeCheck for Expr {
    fn type_check(&self, env: &mut TypeEnv, _: &Option<Expected>) -> Result<Type, AnalysisError> {
        use ExprKind::*;
        match &self.node.0 {
            Literal(val) => match val {
                LiteralValue::Integer(_) => Ok(Type::Int),
                LiteralValue::Float(_) => Ok(Type::Float),
                LiteralValue::String(_) => Ok(Type::String),
                LiteralValue::Boolean(_) => Ok(Type::Boolean),
            },
            Ident(ref id) => {
                if let Some(ty) = env.get(id) {
                    *self.node.1.borrow_mut() = Some(ty.clone());
                    Ok(ty.clone())
                } else {
                    Err(AnalysisError::UndefinedVariable(id.clone(), self.span))
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
                            return Err(AnalysisError::IncompatibleTypesBin(
                                op.clone(),
                                left,
                                right,
                                self.span,
                            ));
                        }
                    }
                    Sub | Mul | Div => {
                        if left == right && matches!(left, Type::Int | Type::Float) {
                            left
                        } else {
                            return Err(AnalysisError::IncompatibleTypesBin(
                                op.clone(),
                                left,
                                right,
                                self.span,
                            ));
                        }
                    }
                    Eq | Neq => {
                        if left == right {
                            Type::Boolean
                        } else {
                            return Err(AnalysisError::IncompatibleTypesBin(
                                op.clone(),
                                left,
                                right,
                                self.span,
                            ));
                        }
                    }
                    Lt | Lte | Gt | Gte => {
                        if left == right && matches!(left, Type::Int | Type::Float) {
                            Type::Boolean
                        } else {
                            return Err(AnalysisError::IncompatibleTypesBin(
                                op.clone(),
                                left,
                                right,
                                self.span,
                            ));
                        }
                    }
                    And | Or => {
                        if left == Type::Boolean && right == Type::Boolean {
                            Type::Boolean
                        } else {
                            return Err(AnalysisError::IncompatibleTypesBin(
                                op.clone(),
                                left,
                                right,
                                self.span,
                            ));
                        }
                    }
                    _ => {
                        return Err(AnalysisError::UnsupportedOperation(op.clone(), self.span));
                    }
                };
                *self.node.1.borrow_mut() = Some(res.clone());
                Ok(res)
            }
            UnaryOp(ast::UnaryOp(op, expr)) => {
                use Op::*;
                let expr_type = expr.type_check(env, &None)?;
                let res = match op {
                    Negate => {
                        if matches!(expr_type, Type::Int | Type::Float) {
                            expr_type
                        } else {
                            return Err(AnalysisError::IncompatibleTypesUn(
                                op.clone(),
                                expr_type,
                                self.span,
                            )); // TODO: better error
                        }
                    }
                    Not => {
                        if expr_type == Type::Boolean {
                            Type::Boolean
                        } else {
                            return Err(AnalysisError::IncompatibleTypesUn(
                                op.clone(),
                                expr_type,
                                self.span,
                            ));
                        }
                    }
                    AddressOf => Type::Pointer(Box::new(expr_type)),
                    Deref => {
                        if let Type::Pointer(ty) = expr_type {
                            *ty
                        } else {
                            return Err(AnalysisError::IncompatibleTypesUn(
                                op.clone(),
                                expr_type,
                                self.span,
                            ));
                        }
                    }
                    _ => {
                        return Err(AnalysisError::UnsupportedOperation(op.clone(), self.span));
                    }
                };
                *self.node.1.borrow_mut() = Some(res.clone());
                Ok(res)
            }
            Call(name, args) => {
                let func_type = match env.get(name) {
                    Some(ty) => ty,
                    None => return Err(AnalysisError::UndefinedFunction(name.clone(), self.span)),
                };
                match func_type.clone() {
                    Type::Function(args_types, ret_type, variadic) => {
                        if args.len() != args_types.len() && !variadic {
                            return Err(AnalysisError::WrongArity(
                                args_types.len(),
                                args.len(),
                                name.clone(),
                                self.span,
                            ));
                        }

                        for (arg, expected_type) in args.iter().zip(args_types.iter()) {
                            let arg_type = arg.type_check(env, &None)?;
                            if &arg_type != expected_type && expected_type != &Type::Any {
                                return Err(AnalysisError::TypeMismatch(
                                    expected_type.clone(),
                                    arg_type,
                                    self.span,
                                ));
                            }
                        }

                        // If the function is variadic, check if the passed arguments, past the required types are of the same type as the last specified type
                        if variadic && args.len() > args_types.len() {
                            let last_type = args_types.last().unwrap();
                            for arg in &args[args_types.len()..] {
                                let arg_type = arg.type_check(env, &None)?;
                                if &arg_type != last_type && last_type != &Type::Any {
                                    return Err(AnalysisError::TypeMismatch(
                                        last_type.clone(),
                                        arg_type,
                                        self.span,
                                    ));
                                }
                            }
                        }

                        *self.node.1.borrow_mut() = Some(*ret_type.clone());
                        Ok(*ret_type.clone())
                    }
                    _ => {
                        return Err(AnalysisError::UncallableType(func_type, self.span));
                    }
                }
            }
        }
    }
}

impl TypeCheck for Stmt {
    fn type_check(
        &self,
        env: &mut TypeEnv,
        expected: &Option<Expected>,
    ) -> Result<Type, AnalysisError> {
        use StmtKind::*;
        match &self.node {
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
                            return Err(AnalysisError::TypeMismatch(
                                ty.clone(),
                                expr_type,
                                self.span,
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
                let name = match &lhs.node.0 {
                    ExprKind::Ident(name) => name,
                    ExprKind::UnaryOp(UnaryOp(Op::Deref, expr)) => match &(**expr).node.0 {
                        ExprKind::Ident(name) => name,
                        _ => panic!("Can't dereference this."),
                    },
                    _ => panic!("Can't assign to this."),
                };

                if let Some(ty) = env.get(name) {
                    let ty = if let Type::Pointer(ty) = ty { *ty } else { ty };

                    if ty != rhs_type {
                        return Err(AnalysisError::TypeMismatch(ty, rhs_type, self.span));
                    }
                } else {
                    return Err(AnalysisError::UndefinedFunction(name.clone(), self.span));
                }
                Ok(Type::Void)
            }
            If(cond, then_stmt, else_stmt) => {
                let cond_type = cond.type_check(env, expected)?;
                if cond_type != Type::Boolean {
                    return Err(AnalysisError::TypeMismatch(
                        Type::Boolean,
                        cond_type,
                        self.span,
                    ));
                }
                then_stmt.type_check(env, expected)?;
                if let Some(else_stmt) = else_stmt {
                    else_stmt.type_check(env, expected)?;
                }
                Ok(Type::Void)
            }
            Expr(expr) => {
                let ty = expr.type_check(env, expected)?;
                *expr.node.1.borrow_mut() = Some(ty.clone());
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
                        return Err(AnalysisError::TypeMismatch(
                            Type::Boolean,
                            cond_type,
                            self.span,
                        ));
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
                    if ret_type != expected.ty {
                        return Err(AnalysisError::ReturnTypeMismatch(
                            expected.func.clone(),
                            expected.ty.clone(),
                            ret_type,
                            self.span,
                        ));
                    }
                }

                Ok(ret_type)
            }
            Function(name, args, ret_type, body) => {
                let arg_types = args.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
                let func_type =
                    Type::Function(arg_types.clone(), Box::new(ret_type.clone()), false);

                env.define(name.clone(), func_type.clone());
                env.enter_scope();
                for (arg_name, arg_type) in args {
                    env.define(arg_name.clone(), arg_type.clone());
                }

                body.type_check(
                    env,
                    &Some(Expected {
                        func: name.clone(),
                        ty: ret_type.clone(),
                    }),
                )?;
                env.exit_scope();

                Ok(Type::Void)
            }
            Continue | Break => Ok(Type::Void),
        }
    }
}
