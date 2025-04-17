use crate::parser::ast::Block;

#[derive(Debug)]
pub struct Unchecked;
#[derive(Debug)]
pub struct TypeChecked;
#[derive(Debug)]
pub struct CfChecked;

#[derive(Debug)]
pub struct Program<Ph> {
    pub stmts: Block,
    _phase: Ph,
}

pub type UncheckedProgram = Program<Unchecked>;
pub type TypeCheckedProgram = Program<TypeChecked>;
pub type CfCheckedProgram = Program<CfChecked>;

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
