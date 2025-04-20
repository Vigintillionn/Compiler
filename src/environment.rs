use std::collections::HashMap;

pub struct Environment<T> {
    scopes: Vec<HashMap<String, T>>,
}

impl<T> Environment<T> {
    pub fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) -> Result<(), String> {
        if self.scopes.len() > 1 {
            self.scopes.pop();
            Ok(())
        } else {
            Err("Cannot exit global scope".to_string())
        }
    }

    pub fn define(&mut self, name: String, value: T) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        } else {
            panic!("No scope to define in");
        }
    }

    pub fn assign(&mut self, name: &str, value: T) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }

        Err(format!("Undefined variable: '{}'", name))
    }
}

impl<T: Clone> Environment<T> {
    pub fn get(&self, name: &str) -> Option<T> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }

        None
    }
}
