use crate::{
    errors::analysis::AnalysisError,
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

pub trait FlowCheck<E> {
    fn flow_check(&self) -> Result<FlowInfo, E>;
}

impl FlowCheck<Vec<AnalysisError>> for TypeCheckedProgram {
    fn flow_check(&self) -> Result<FlowInfo, Vec<AnalysisError>> {
        let mut errors = Vec::new();
        let mut info = FlowInfo::default();

        for stmt in &self.stmts {
            if !info.reachable {
                errors.push(AnalysisError::Unreachable(stmt.span));
                continue;
            }

            match stmt.flow_check() {
                Ok(next) => info = next,
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(info)
        }
    }
}

impl FlowCheck<AnalysisError> for Stmt {
    fn flow_check(&self) -> Result<FlowInfo, AnalysisError> {
        use StmtKind::*;
        match &self.node {
            Block(stmts) => {
                let mut info = FlowInfo::default();

                for stmt in stmts {
                    if !info.reachable {
                        return Err(AnalysisError::Unreachable(stmt.span));
                    }
                    info = stmt.flow_check()?;
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
                    return Err(AnalysisError::FunctionNoReturnAllPaths(
                        name.clone(),
                        self.span,
                    ));
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
