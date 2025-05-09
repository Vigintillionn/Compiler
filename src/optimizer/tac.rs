use std::{collections::HashMap, vec};

use crate::{
    errors::Spanned,
    parser::ast::{BinaryOp, ExprInner, ExprKind, LiteralValue, Op, Stmt, StmtKind, Type, UnaryOp},
};

#[derive(Debug, Clone, Copy)]
pub enum TACOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Not,
    Negate,
    Deref,
    AddressOf,
}

impl From<Op> for TACOp {
    fn from(value: Op) -> Self {
        match value {
            Op::Add => TACOp::Add,
            Op::Sub => TACOp::Sub,
            Op::Mul => TACOp::Mul,
            Op::Div => TACOp::Div,
            Op::Eq => TACOp::Eq,
            Op::Neq => TACOp::Neq,
            Op::Lt => TACOp::Lt,
            Op::Lte => TACOp::Lte,
            Op::Gt => TACOp::Gt,
            Op::Gte => TACOp::Gte,
            Op::And => TACOp::And,
            Op::Or => TACOp::Or,
            Op::Not => TACOp::Not,
            Op::Negate => TACOp::Negate,
            Op::Deref => TACOp::Deref,
            Op::AddressOf => TACOp::AddressOf,
        }
    }
}

impl From<&Op> for TACOp {
    fn from(value: &Op) -> Self {
        (*value).into()
    }
}

#[derive(Debug, Clone)]
pub enum TACOperand {
    ConstInt(i64),
    ConstFloat(f64),
    ConstBool(bool),
    ConstString(String),
    Var(String),
    Temp(String),
}

#[derive(Debug, Clone)]
pub enum TACInstr {
    Label(String),
    Assign {
        dst: String,
        src: TACOperand,
    }, // a = b
    Store {
        ptr: TACOperand,
        src: TACOperand,
        offset: usize,
    },
    Load {
        dst: String,
        base: TACOperand,
        offset: usize,
    },
    BinOp {
        op: TACOp,
        dst: String,
        lhs: TACOperand,
        rhs: TACOperand,
    }, // a = b op c
    UnOp {
        op: TACOp,
        dst: String,
        expr: TACOperand,
    }, // a = op b
    Goto(String), // goto label
    IfGoto {
        cond: TACOperand,
        label: String,
    }, // if a goto label
    Call {
        dst: Option<String>,
        name: String,
        args: Vec<TACOperand>,
    }, // call function with args
    Return(Option<TACOperand>), // return value
}

#[derive(Debug)]
pub struct TAC {
    pub instrs: Vec<TACInstr>,
    temp_count: usize,
    label_count: usize,
    break_stack: Vec<String>,
    continue_stack: Vec<String>,
}

impl TAC {
    pub fn new() -> Self {
        Self {
            instrs: Vec::new(),
            temp_count: 0,
            label_count: 0,
            break_stack: Vec::new(),
            continue_stack: Vec::new(),
        }
    }

    pub fn new_temp(&mut self) -> String {
        let temp = format!("t{}", self.temp_count);
        self.temp_count += 1;
        temp
    }

    pub fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_count);
        self.label_count += 1;
        label
    }
}

pub trait ToTAC<T> {
    fn to_tac(&self, tac: &mut TAC) -> Result<T, String>;
}

impl<T, U> ToTAC<U> for Spanned<T>
where
    T: ToTAC<U>,
{
    fn to_tac(&self, tac: &mut TAC) -> Result<U, String> {
        self.node.to_tac(tac)
    }
}

impl ToTAC<()> for Stmt {
    fn to_tac(&self, tac: &mut TAC) -> Result<(), String> {
        use StmtKind::*;
        match &self.node {
            Block(stmts) => {
                for stmt in stmts {
                    stmt.to_tac(tac)?;
                }
                Ok(())
            }
            If(cond, then_branch, else_branch) => {
                let cond_op = cond.to_tac(tac)?;
                let then_label = tac.new_label();
                let end_label = tac.new_label();

                let on_false = if else_branch.is_some() {
                    tac.new_label()
                } else {
                    end_label.clone()
                };

                // If false jump to else/end
                tac.instrs.push(TACInstr::IfGoto {
                    cond: cond_op,
                    label: on_false.clone(),
                });
                // Then
                tac.instrs.push(TACInstr::Label(then_label));
                then_branch.to_tac(tac)?;
                // Else
                if else_branch.is_some() {
                    tac.instrs.push(TACInstr::Goto(end_label.clone()));
                    tac.instrs.push(TACInstr::Label(on_false));
                    else_branch.as_ref().unwrap().to_tac(tac)?; // unwrap is safe here
                }
                tac.instrs.push(TACInstr::Label(end_label));

                Ok(())
            }
            Var(name, expr, _) => {
                let expr_op = expr.to_tac(tac)?;
                tac.instrs.push(TACInstr::Assign {
                    dst: name.clone(),
                    src: expr_op,
                });
                Ok(())
            }
            Assign(lhs, rhs) => {
                let r = rhs.to_tac(tac)?;

                match &lhs.node.0 {
                    ExprKind::Ident(name) => {
                        tac.instrs.push(TACInstr::Assign {
                            dst: name.clone(),
                            src: r,
                        });
                        Ok(())
                    }
                    ExprKind::UnaryOp(UnaryOp(Op::Deref, boxed_ptr)) => {
                        let ptr = boxed_ptr.to_tac(tac)?;
                        tac.instrs.push(TACInstr::Store {
                            ptr,
                            src: r,
                            offset: 0,
                        });
                        Ok(())
                    }
                    _ => {
                        return Err(format!("Invalid LHS in assignment: {:?}", lhs));
                    }
                }
            }
            Loop(init, cond, incr, body) => {
                if let Some(init) = init {
                    init.to_tac(tac)?;
                }
                let start_label = tac.new_label();
                let end_label = tac.new_label();

                tac.continue_stack.push(start_label.clone());
                tac.break_stack.push(end_label.clone());

                tac.instrs.push(TACInstr::Label(start_label.clone()));
                if let Some(cond) = cond {
                    let cond_op = cond.to_tac(tac)?;
                    tac.instrs.push(TACInstr::IfGoto {
                        cond: cond_op,
                        label: end_label.clone(),
                    });
                }
                body.to_tac(tac)?;
                if let Some(incr) = incr {
                    incr.to_tac(tac)?;
                }
                tac.instrs.push(TACInstr::Goto(start_label));
                tac.instrs.push(TACInstr::Label(end_label));

                tac.break_stack.pop();
                tac.continue_stack.pop();

                Ok(())
            }
            Ret(expr) => {
                let val = if let Some(e) = expr {
                    Some(e.to_tac(tac)?)
                } else {
                    None
                };
                tac.instrs.push(TACInstr::Return(val));
                Ok(())
            }
            Function(name, params, _, body) => {
                tac.instrs.push(TACInstr::Label(format!("proc_{}", name)));

                for (i, (param_name, _)) in params.iter().enumerate() {
                    let arg_temp = format!("arg{}", i);
                    tac.instrs.push(TACInstr::Assign {
                        dst: param_name.clone(),
                        src: TACOperand::Var(arg_temp),
                    });
                }

                body.to_tac(tac)?;

                // TODO: maybe map params to TACInstr::Assign?

                if !matches!(tac.instrs.last(), Some(TACInstr::Return(_))) {
                    tac.instrs.push(TACInstr::Return(None)); // implicit return
                }
                tac.instrs.push(TACInstr::Label(name.clone() + "_end"));
                Ok(())
            }
            Expr(expr) => {
                expr.to_tac(tac)?;
                Ok(())
            }
            Break => {
                if let Some(label) = tac.break_stack.last() {
                    tac.instrs.push(TACInstr::Goto(label.clone()));
                } else {
                    return Err("Break statement outside of loop".to_string());
                }
                Ok(())
            }
            Continue => {
                if let Some(label) = tac.continue_stack.last() {
                    tac.instrs.push(TACInstr::Goto(label.clone()));
                } else {
                    return Err("Continue statement outside of loop".to_string());
                }
                Ok(())
            }
        }
    }
}

impl ToTAC<TACOperand> for ExprInner {
    fn to_tac(&self, tac: &mut TAC) -> Result<TACOperand, String> {
        match &self.0 {
            ExprKind::Literal(lit) => match lit {
                LiteralValue::Integer(i) => Ok(TACOperand::ConstInt(*i)),
                LiteralValue::Float(f) => Ok(TACOperand::ConstFloat(*f)),
                LiteralValue::Boolean(b) => Ok(TACOperand::ConstBool(*b)),
                LiteralValue::String(s) => Ok(TACOperand::ConstString(s.clone())),
            },
            ExprKind::Ident(name) => Ok(TACOperand::Var(name.clone())),
            ExprKind::BinaryOp(BinaryOp(lhs, op, rhs)) => {
                let l = lhs.to_tac(tac)?;
                let r = rhs.to_tac(tac)?;
                let dst = tac.new_temp();
                tac.instrs.push(TACInstr::BinOp {
                    op: op.into(),
                    dst: dst.clone(),
                    lhs: l,
                    rhs: r,
                });
                Ok(TACOperand::Temp(dst))
            }
            ExprKind::UnaryOp(UnaryOp(op, expr)) => {
                let expr = expr.to_tac(tac)?;
                let dst = tac.new_temp();
                tac.instrs.push(TACInstr::UnOp {
                    op: op.into(),
                    dst: dst.clone(),
                    expr,
                });
                Ok(TACOperand::Temp(dst))
            }
            ExprKind::Call(fname, args) => {
                let ret_type = &self.1;
                let mut arg_ops = Vec::new();
                for arg in args {
                    arg_ops.push(arg.to_tac(tac)?);
                }

                // If return type is void, we don't need a destination
                let d = tac.new_temp();
                let dst = if let Some(ret) = ret_type.clone().into_inner() {
                    match ret {
                        Type::Void => None,
                        _ => Some(d.clone()),
                    }
                } else {
                    None
                };

                tac.instrs.push(TACInstr::Call {
                    dst,
                    name: fname.clone(),
                    args: arg_ops,
                });
                Ok(TACOperand::Temp(d))
            }
        }
    }
}

pub trait UsesDefs {
    fn uses(&self) -> Vec<String>;
    fn defs(&self) -> Vec<String>;
}

impl UsesDefs for TACInstr {
    fn uses(&self) -> Vec<String> {
        match self {
            TACInstr::Assign { src, .. } => src.vars(),
            TACInstr::Store { ptr, src, .. } => vec![ptr.clone(), src.clone()]
                .into_iter()
                .flat_map(|op| op.vars())
                .collect(),
            TACInstr::Load { base, .. } => base.vars(),
            TACInstr::BinOp { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()]
                .into_iter()
                .flat_map(|op| op.vars())
                .collect(),
            TACInstr::UnOp { expr, .. } => expr.vars(),
            TACInstr::IfGoto { cond, .. } => cond.vars(),
            TACInstr::Call { args, .. } => args.iter().flat_map(|arg| arg.vars()).collect(),
            TACInstr::Return(Some(expr)) => expr.vars(),
            _ => vec![],
        }
    }

    fn defs(&self) -> Vec<String> {
        match self {
            TACInstr::Assign { dst, .. } => vec![dst.clone()],
            TACInstr::Store { ptr, .. } => vec![ptr.clone()]
                .into_iter()
                .flat_map(|op| op.vars())
                .collect(),
            TACInstr::Load { dst, .. } => vec![dst.clone()],
            TACInstr::BinOp { dst, .. } => vec![dst.clone()],
            TACInstr::UnOp { dst, .. } => vec![dst.clone()],
            TACInstr::Call { dst: Some(d), .. } => vec![d.clone()],
            _ => vec![],
        }
    }
}

impl TACInstr {
    fn rename_vars(&mut self, map: &HashMap<String, String>) {
        match self {
            TACInstr::Assign { dst, src } => {
                *dst = map.get(dst).unwrap_or(dst).clone();
                src.rename_vars(map);
            }
            TACInstr::BinOp { dst, lhs, rhs, .. } => {
                *dst = map.get(dst).unwrap_or(dst).clone();
                lhs.rename_vars(map);
                rhs.rename_vars(map);
            }
            TACInstr::UnOp { dst, expr, .. } => {
                *dst = map.get(dst).unwrap_or(dst).clone();
                expr.rename_vars(map);
            }
            TACInstr::Store { ptr, src, .. } => {
                ptr.rename_vars(map);
                src.rename_vars(map);
            }
            TACInstr::IfGoto { cond, label } => {
                cond.rename_vars(map);
                *label = map.get(label).unwrap_or(label).clone();
            }
            TACInstr::Call { dst, name, args } => {
                if let Some(d) = dst {
                    *d = map.get(d).unwrap_or(d).clone();
                }
                *name = map.get(name).unwrap_or(name).clone();
                for arg in args {
                    arg.rename_vars(map);
                }
            }
            TACInstr::Return(Some(expr)) => {
                expr.rename_vars(map);
            }
            _ => {}
        }
    }
}

impl TACOperand {
    fn vars(&self) -> Vec<String> {
        match self {
            TACOperand::Var(name) | TACOperand::Temp(name) => vec![name.clone()],
            _ => vec![],
        }
    }

    fn rename_vars(&mut self, map: &HashMap<String, String>) {
        if let TACOperand::Var(v) = self {
            *v = map.get(v).unwrap_or(v).clone();
        }
        if let TACOperand::Temp(v) = self {
            *v = map.get(v).unwrap_or(v).clone();
        }
    }
}

impl TAC {
    pub fn uniquify_variables(&mut self) {
        let mut counters = HashMap::new();
        let mut mapping = HashMap::new();

        for instr in &mut self.instrs {
            for var in instr.defs().into_iter().chain(instr.uses()) {
                if !mapping.contains_key(&var) {
                    let cnt = counters.entry(var.clone()).or_insert(0);
                    let new_name = format!("{}_{}", var.clone(), cnt);
                    *cnt += 1;
                    mapping.insert(var, new_name.clone());
                }
            }
            instr.rename_vars(&mapping);
        }
    }
}
