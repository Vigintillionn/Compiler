use crate::{
    errors::analysis::AnalysisError,
    parser::ast::Type,
    program::{CfCheckedProgram, TypeCheckedProgram, UncheckedProgram},
};
use cf_check::FlowCheck;
use type_check::{TypeCheck, TypeEnv};

mod cf_check;
mod type_check;

pub fn type_check_program(
    program: UncheckedProgram,
) -> Result<TypeCheckedProgram, Vec<AnalysisError>> {
    let mut errors = Vec::new();

    let mut env = TypeEnv::new();
    env.define(
        "print".to_string(),
        Type::Function(vec![Type::Any], Box::new(Type::Void), true),
    );

    for stmt in &program.stmts {
        match stmt.type_check(&mut env, &None) {
            Ok(_) => {}
            Err(err) => errors.push(err),
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(program.into_type_checked())
    }
}

pub fn cf_check_program(program: TypeCheckedProgram) -> Result<CfCheckedProgram, String> {
    program.flow_check()?;
    Ok(program.into_cf_checked())
}
