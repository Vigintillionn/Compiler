use std::collections::HashMap;

use super::tac::{TACInstr, TACOp, TACOperand, TAC};

pub fn constant_fold_and_prop(tac: &mut TAC) {
    let mut const_env: HashMap<String, TACOperand> = HashMap::new();
    for instr in &mut tac.instrs {
        match instr {
            TACInstr::Assign { dst, src } => {
                if matches!(
                    src,
                    TACOperand::ConstInt(_)
                        | TACOperand::ConstFloat(_)
                        | TACOperand::ConstString(_)
                        | TACOperand::ConstBool(_)
                ) {
                    const_env.insert(dst.clone(), src.clone());
                } else if let TACOperand::Temp(t) = src {
                    if let Some(val) = const_env.get(t) {
                        *src = val.clone();
                        const_env.insert(dst.clone(), val.clone());
                    } else {
                        const_env.remove(dst);
                    }
                } else {
                    const_env.remove(dst);
                }
            }
            TACInstr::BinOp { op, dst, lhs, rhs } => {
                let fold = |operand: &TACOperand| match operand {
                    TACOperand::Var(v) | TACOperand::Temp(v) => {
                        const_env.get(v).cloned().unwrap_or_else(|| operand.clone())
                    }
                    _ => operand.clone(),
                };

                // Propagate constants to the operands
                let new_lhs = fold(lhs);
                let new_rhs = fold(rhs);

                // Try constant folding for ints, floats, bools and comparisons
                let folded: Option<TACOperand> = match (&new_lhs, &new_rhs, op.clone()) {
                    // Integer arithmetic
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Add) => {
                        Some(TACOperand::ConstInt(lv + rv))
                    }
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Sub) => {
                        Some(TACOperand::ConstInt(lv - rv))
                    }
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Mul) => {
                        Some(TACOperand::ConstInt(lv * rv))
                    }
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Div) => {
                        Some(TACOperand::ConstInt(lv / rv))
                    }

                    // Float arithmetic
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Add) => {
                        Some(TACOperand::ConstFloat(lv + rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Sub) => {
                        Some(TACOperand::ConstFloat(lv - rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Mul) => {
                        Some(TACOperand::ConstFloat(lv * rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Div) => {
                        Some(TACOperand::ConstFloat(lv / rv))
                    }

                    // Boolean logic
                    (TACOperand::ConstBool(lv), TACOperand::ConstBool(rv), TACOp::And) => {
                        Some(TACOperand::ConstBool(*lv && *rv))
                    }
                    (TACOperand::ConstBool(lv), TACOperand::ConstBool(rv), TACOp::Or) => {
                        Some(TACOperand::ConstBool(*lv || *rv))
                    }

                    // Equality and inequality
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Eq) => {
                        Some(TACOperand::ConstBool(lv == rv))
                    }
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Neq) => {
                        Some(TACOperand::ConstBool(lv != rv))
                    }

                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Eq) => {
                        Some(TACOperand::ConstBool(lv == rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Neq) => {
                        Some(TACOperand::ConstBool(lv != rv))
                    }

                    (TACOperand::ConstBool(lv), TACOperand::ConstBool(rv), TACOp::Eq) => {
                        Some(TACOperand::ConstBool(*lv == *rv))
                    }
                    (TACOperand::ConstBool(lv), TACOperand::ConstBool(rv), TACOp::Neq) => {
                        Some(TACOperand::ConstBool(*lv != *rv))
                    }

                    // Greather than and less than
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Gt) => {
                        Some(TACOperand::ConstBool(lv > rv))
                    }
                    (TACOperand::ConstInt(lv), TACOperand::ConstInt(rv), TACOp::Lt) => {
                        Some(TACOperand::ConstBool(lv < rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Gt) => {
                        Some(TACOperand::ConstBool(lv > rv))
                    }
                    (TACOperand::ConstFloat(lv), TACOperand::ConstFloat(rv), TACOp::Lt) => {
                        Some(TACOperand::ConstBool(lv < rv))
                    }

                    _ => None,
                };

                if let Some(c) = folded {
                    // Record and replace with simple assignment
                    const_env.insert(dst.clone(), c.clone());
                    *instr = TACInstr::Assign {
                        dst: dst.clone(),
                        src: c,
                    };
                } else {
                    // No folding: update operands and clear any old constant for dst
                    const_env.remove(dst);
                    *instr = TACInstr::BinOp {
                        op: *op,
                        dst: dst.clone(),
                        lhs: new_lhs,
                        rhs: new_rhs,
                    };
                }
            }
            TACInstr::UnOp { op, dst, expr } => {
                // Propagate constants to the operand
                let new_expr = match expr {
                    TACOperand::Var(v) | TACOperand::Temp(v) => {
                        const_env.get(v).cloned().unwrap_or_else(|| expr.clone())
                    }
                    _ => expr.clone(),
                };

                // Try constant folding for ints, floats, bools and comparisons
                let folded: Option<TACOperand> = match (&new_expr, op.clone()) {
                    // Integer arithmetic
                    (TACOperand::ConstInt(v), TACOp::Negate) => Some(TACOperand::ConstInt(-v)),
                    // Float arithmetic
                    (TACOperand::ConstFloat(v), TACOp::Negate) => Some(TACOperand::ConstFloat(-v)),
                    // Boolean logic
                    (TACOperand::ConstBool(v), TACOp::Not) => Some(TACOperand::ConstBool(!*v)),
                    _ => None,
                };

                if let Some(c) = folded {
                    // Record and replace with simple assignment
                    const_env.insert(dst.clone(), c.clone());
                    *instr = TACInstr::Assign {
                        dst: dst.clone(),
                        src: c,
                    };
                } else {
                    // No folding: update operand and clear any old constant for dst
                    const_env.remove(dst);
                    *instr = TACInstr::UnOp {
                        op: *op,
                        dst: dst.clone(),
                        expr: new_expr,
                    };
                }
            }
            _ => {} // no-op
        }
    }
}
