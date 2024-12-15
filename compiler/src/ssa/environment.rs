use std::collections::HashMap;
use parser::{ast::Stmt, types::Type};

#[derive(Debug, Clone)]
pub struct Environment {
  variables: HashMap<String, Type>,
  parent: Option<Box<Environment>>,
}

impl Environment {
  pub fn new() -> Self {
    Environment {
      variables: HashMap::new(),
      parent: None,
    }
  }

  pub fn with_parent(parent: Environment) -> Self {
    Environment {
      variables: HashMap::new(),
      parent: Some(Box::new(parent)),
    }
  }

  pub fn insert(&mut self, name: String, ty: Type) {
    self.variables.insert(name, ty);
  }

  pub fn get(&self, name: &str) -> Option<&Type> {
    if let Some(ty) = self.variables.get(name) {
      return Some(ty);
    }

    if let Some(parent) = &self.parent {
      return parent.get(name);
    }

    None
  }
  
  pub fn new_scope(&self) -> Environment {
    Environment::with_parent(self.clone())
  }

  pub fn get_parent(&self) -> Option<&Environment> {
    self.parent.as_ref().map(|p| p.as_ref())
  }

  pub fn generate_from_stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Let(name, expr, ty ) => {
        // If the type is specified, insert it into the environment, also check if the type matches the expression
        // Otherwise, infer the type from the expression

        if self.get(name).is_some() {
          panic!("Variable {} already declared", name);
        }

        let expr_type = expr.get_type().unwrap_or(Type::Bool);
        if let Some(ty) = ty {
          if !are_equivalent_types(&expr_type, ty) {
            panic!("Type mismatch: {:?} and {:?}", ty, expr_type);
          }
        }

        self.insert(name.clone(), expr_type);
      },
      Stmt::Program(stmts) => {
        for stmt in stmts {
          self.generate_from_stmt(stmt);
        }
      }
      Stmt::Block(stmts) => {
        let mut new_env = self.new_scope();
        for stmt in stmts {
          new_env.generate_from_stmt(stmt);
        }
      },
      _ => {}
    }
  }

  pub fn print(&self) {
    for (name, ty) in &self.variables {
      println!("{}: {:?}", name, ty);
    }
  }
}

fn are_equivalent_types(type1: &Type, type2: &Type) -> bool {
  // int and uint are equivalent

  match (type1, type2) {
    (Type::Int, Type::Uint) | (Type::Uint, Type::Int) => true,
    (Type::Pointer(t1), Type::Pointer(t2)) => are_equivalent_types(t1, t2),
    (Type::FSignature(params1, return1), Type::FSignature(params2, return2)) => {
      if params1.len() != params2.len() {
        return false;
      }

      for (p1, p2) in params1.iter().zip(params2.iter()) {
        if !are_equivalent_types(p1, p2) {
          return false;
        }
      }

      are_equivalent_types(return1, return2)
    },
    _ => type1 == type2,
  }
}