use std::marker::PhantomData;

use crate::parser::ast::Block;

#[derive(Debug)]
pub enum Unchecked {}
#[derive(Debug)]
pub enum TypeChecked {}
#[derive(Debug)]
pub enum CfChecked {}

#[derive(Debug)]
pub struct Program<Ph> {
    pub stmts: Block,
    _phase: PhantomData<Ph>,
}

pub type UncheckedProgram = Program<Unchecked>;
pub type TypeCheckedProgram = Program<TypeChecked>;
pub type CfCheckedProgram = Program<CfChecked>;

impl UncheckedProgram {
    pub fn new(stmts: Block) -> Self {
        Self {
            stmts,
            _phase: PhantomData,
        }
    }

    pub fn into_type_checked(self) -> TypeCheckedProgram {
        TypeCheckedProgram {
            stmts: self.stmts,
            _phase: PhantomData,
        }
    }
}

impl TypeCheckedProgram {
    pub fn into_cf_checked(self) -> CfCheckedProgram {
        CfCheckedProgram {
            stmts: self.stmts,
            _phase: PhantomData,
        }
    }
}
