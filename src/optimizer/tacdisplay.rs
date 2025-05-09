use std::fmt;

use super::tac::{TACInstr, TACOp, TACOperand, TAC};

impl fmt::Display for TACOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TACOp::Add => "+",
            TACOp::Sub => "-",
            TACOp::Mul => "*",
            TACOp::Div => "/",
            TACOp::Eq => "==",
            TACOp::Neq => "!=",
            TACOp::Lt => "<",
            TACOp::Lte => "<=",
            TACOp::Gt => ">",
            TACOp::Gte => ">=",
            TACOp::And => "&&",
            TACOp::Or => "||",
            TACOp::Not => "!",
            TACOp::Negate => "-",
            TACOp::Deref => "*",
            TACOp::AddressOf => "&",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for TACOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TACOperand::ConstInt(i) => write!(f, "{}", i),
            TACOperand::ConstFloat(fl) => write!(f, "{}", fl),
            TACOperand::ConstBool(b) => write!(f, "{}", b),
            TACOperand::ConstString(s) => write!(f, "\"{}\"", s),
            TACOperand::Var(name) => write!(f, "{}", name),
            TACOperand::Temp(t) => write!(f, "{}", t),
        }
    }
}

impl fmt::Display for TACInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TACInstr::Label(lbl) => write!(f, "{}:", lbl),

            TACInstr::Assign { dst, src } => write!(f, "{} = {}", dst, src),

            TACInstr::BinOp { op, dst, lhs, rhs } => write!(f, "{} = {} {} {}", dst, lhs, op, rhs),

            TACInstr::UnOp { op, dst, expr } =>
            // unary prefix operators, e.g. t1 = - t2 or t1 = ! t2
            {
                write!(f, "{} = {} {}", dst, op, expr)
            }

            TACInstr::Goto(lbl) => write!(f, "goto {}", lbl),

            TACInstr::IfGoto { cond, label } => write!(f, "if {} == 0 goto {}", cond, label),

            TACInstr::Call {
                dst: None,
                name,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "call {}({})", name, args_str)
            }
            TACInstr::Call {
                dst: Some(dst),
                name,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = call {}({})", dst, name, args_str)
            }

            TACInstr::Return(None) => write!(f, "return"),
            TACInstr::Return(Some(rv)) => write!(f, "return {}", rv),

            TACInstr::Store { ptr, src, .. } => write!(f, "*{} = {}", ptr, src),
            TACInstr::Load { dst, base, offset } => write!(f, "{} = *{} + {}", dst, base, offset),
        }
    }
}

impl fmt::Display for TAC {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instr in &self.instrs {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}
