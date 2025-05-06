use constants::constant_fold_and_prop;
use tac::{ToTAC, TAC};

use crate::program::CfCheckedProgram;

mod constants;
pub mod tac;
pub mod tacdisplay;

pub fn optimize_program(program: CfCheckedProgram) -> Result<TAC, String> {
    let mut tac = TAC::new();
    for stmt in &program.stmts {
        stmt.to_tac(&mut tac)?;
    }
    // Perform constant folding and propagation
    constant_fold_and_prop(&mut tac);
    Ok(tac)
}
