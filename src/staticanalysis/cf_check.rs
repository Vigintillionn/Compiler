use crate::{
    parser::ast::{Stmt, StmtKind, Type},
    program::TypeCheckedProgram,
};

#[derive(Debug, Clone, Copy)]
pub struct FlowInfo {
    pub reachable: bool,
    pub always_returns: bool,
}

impl Default for FlowInfo {
    fn default() -> Self {
        FlowInfo {
            reachable: true,
            always_returns: false,
        }
    }
}

pub trait FlowCheck {
    fn flow_check(&self) -> Result<FlowInfo, String>;
}

impl FlowCheck for TypeCheckedProgram {
    fn flow_check(&self) -> Result<FlowInfo, String> {
        let mut info = FlowInfo::default();

        for stmt in &self.stmts {
            if !info.reachable {
                return Err(format!("Unreachable code in program {:?}", stmt));
            }

            let next = stmt.flow_check()?;
            if next.always_returns {
                info = next;
                break;
            }
            info = next;
        }

        Ok(info)
    }
}

impl FlowCheck for Stmt {
    fn flow_check(&self) -> Result<FlowInfo, String> {
        use StmtKind::*;
        match &self.node {
            Block(stmts) => {
                let mut info = FlowInfo::default();

                for stmt in stmts {
                    if !info.reachable {
                        return Err(format!("Unreachable code in program {:?}", stmt));
                    }
                    let next = stmt.flow_check()?;
                    if next.always_returns {
                        info = next;
                        break;
                    }
                    info = next;
                }
                Ok(info)
            }
            If(_, then_s, else_s) => {
                let then_info = then_s.flow_check()?;
                let else_info = if let Some(es) = else_s {
                    es.flow_check()?
                } else {
                    FlowInfo::default()
                };

                Ok(FlowInfo {
                    reachable: then_info.reachable || else_info.reachable,
                    always_returns: then_info.always_returns && else_info.always_returns,
                })
            }
            Loop(_, _, _, body) => {
                let _ = body.flow_check()?;
                Ok(FlowInfo {
                    reachable: true,
                    always_returns: false,
                })
            }
            Function(name, _, ret, body) => {
                let body_info = body.flow_check()?;
                if ret != &Type::Void && !body_info.always_returns {
                    return Err(format!("Function `{}` may not return on all paths", name));
                }
                Ok(FlowInfo {
                    reachable: true,
                    always_returns: false,
                })
            }
            Continue | Break => Ok(FlowInfo {
                reachable: false,
                always_returns: false,
            }),
            Ret(_) => Ok(FlowInfo {
                reachable: false,
                always_returns: true,
            }),
            _ => Ok(FlowInfo::default()),
        }
    }
}
