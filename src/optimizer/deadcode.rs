use std::collections::HashSet;

use super::tac::{TACInstr, TACOperand, TAC};

pub fn dead_code_elimination(tac: &mut TAC) {
    // Compute liveness
    let mut live: HashSet<String> = HashSet::new();
    let mut result: Vec<TACInstr> = Vec::new();

    // Walk the instructions in reverse order
    for instr in tac.instrs.iter().rev() {
        match instr {
            // Check if the destination variable is live
            TACInstr::Assign { dst, src } => {
                if live.contains(dst) {
                    // Keep the instruction if the destination is live
                    result.push(instr.clone());
                    // Update liveness: dst is redefined -> kill, src is used -> add
                    live.remove(dst);
                    src.add_uses(&mut live);
                }
            }
            TACInstr::BinOp { dst, lhs, rhs, .. } => {
                if live.contains(dst) {
                    result.push(instr.clone());
                    live.remove(dst);
                    lhs.add_uses(&mut live);
                    rhs.add_uses(&mut live);
                }
            }
            TACInstr::UnOp { dst, expr, .. } => {
                if live.contains(dst) {
                    result.push(instr.clone());
                    live.remove(dst);
                    expr.add_uses(&mut live);
                }
            }
            // Control and side-effect instructions: always keep and add uses
            TACInstr::Label(_)
            | TACInstr::Goto(_)
            | TACInstr::Store { .. }
            | TACInstr::Load { .. }
            | TACInstr::IfGoto { .. }
            | TACInstr::Call { .. }
            | TACInstr::Return(_) => {
                // Keep and record uses
                result.push(instr.clone());
                match instr {
                    TACInstr::IfGoto { cond, .. } => cond.add_uses(&mut live),
                    TACInstr::Store { ptr, src, .. } => {
                        ptr.add_uses(&mut live);
                        src.add_uses(&mut live);
                    }
                    TACInstr::Load { dst, base, .. } => {
                        base.add_uses(&mut live); // TODO: fix
                    }
                    TACInstr::Call { args, .. } => {
                        for arg in args {
                            arg.add_uses(&mut live);
                        }
                    }
                    TACInstr::Return(Some(expr)) => expr.add_uses(&mut live),
                    _ => {}
                }
            }
        }
    }

    // Reverse the result to maintain original order
    result.reverse();
    tac.instrs = result;
}

impl TACOperand {
    fn add_uses(&self, live: &mut HashSet<String>) {
        // Add any variables used in this operand to the live set
        match self {
            TACOperand::Var(name) | TACOperand::Temp(name) => {
                live.insert(name.clone());
            }
            _ => {} // Do nothing for other operand types
        }
    }
}
