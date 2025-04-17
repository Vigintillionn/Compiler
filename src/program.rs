use std::marker::PhantomData;

use crate::parser::ast::Block;

#[derive(Debug)]
pub struct Unchecked;
#[derive(Debug)]
pub struct TypeChecked;
#[derive(Debug)]
pub struct CfChecked;

#[derive(Debug)]
pub struct UncheckedProgram {
    pub stmts: Block,
    _phase: Unchecked,
}

#[derive(Debug)]
pub struct TypeCheckedProgram {
    pub stmts: Block,
    _phase: TypeChecked,
}

#[derive(Debug)]
pub struct CfCheckedProgram {
    pub stmts: Block,
    _phase: CfChecked,
}

impl UncheckedProgram {
    pub fn new(stmts: Block) -> Self {
        Self {
            stmts,
            _phase: Unchecked,
        }
    }

    pub fn into_type_checked(self) -> TypeCheckedProgram {
        TypeCheckedProgram {
            stmts: self.stmts,
            _phase: TypeChecked,
        }
    }
}

impl TypeCheckedProgram {
    pub fn into_cf_checked(self) -> CfCheckedProgram {
        CfCheckedProgram {
            stmts: self.stmts,
            _phase: CfChecked,
        }
    }
}
