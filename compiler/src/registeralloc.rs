use core::alloc;
use std::collections::{HashMap, HashSet};
use parser::ast::{Expr, Stmt};

const REGISTERS: [&str; 8] = ["t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7"];

pub struct InferenceGraph {
  adj_list: HashMap<String, HashSet<String>>,
}

impl InferenceGraph {
  pub fn new() -> Self {
    InferenceGraph {
      adj_list: HashMap::new(),
    }
  }

  pub fn add_edge(&mut self, var1: String, var2: String) {
    if !self.adj_list.contains_key(&var1) {
      self.adj_list.insert(var1.clone(), HashSet::new());
    }
    if !self.adj_list.contains_key(&var2) {
      self.adj_list.insert(var2.clone(), HashSet::new());
    }

    self.adj_list.get_mut(&var1).unwrap().insert(var2.clone());
    self.adj_list.get_mut(&var2).unwrap().insert(var1.clone());
  }

  pub fn get_neighbors(&self, var: &str) -> &HashSet<String> {
    self.adj_list.get(var).ok_or(&HashSet::<String>::new()).unwrap()
  }

  pub fn from_ast(stmts: &Stmt) -> Self {
    let mut graph = InferenceGraph::new();
    let mut live_vars = HashSet::new();
    graph.visit_stmt(stmts, &mut live_vars);
    graph.print_colored();
    
    graph
  }

  // pub fn get_allocations(&self) -> HashMap<String, String> {
  //   let mut allocations = HashMap::new();
  //   let colors = self.color();

  //   self.print_colored(); 

  //   for (var, color) in self.adj_list.keys().zip(colors.iter()) {
  //     allocations.insert(var.clone(), color.clone());
  //   }

  //   allocations
  // }

  pub fn get_allocations(&self) -> HashMap<String, String> {
    let mut allocations = HashMap::new();
    let mut stack: Vec<String> = self.adj_list.keys().cloned().collect();
    let mut colors: HashMap<String, String> = HashMap::new();

    while let Some(var) = stack.pop() {
      let neighbors = self.get_neighbors(&var);

      let used_colors: HashSet<String> = neighbors
        .iter()
        .filter_map(|neighbor| colors.get(neighbor))
        .cloned()
        .collect();

      let color = REGISTERS
        .iter()
        .find(|&&reg| !used_colors.contains(reg))
        .expect("Ran out of registers")
        .to_string();

      colors.insert(var.clone(), color);
    }

    for (var, color) in colors {
      allocations.insert(var, color);
    }

    allocations
  }

  fn visit_stmt(&mut self, node: &Stmt, live_vars: &mut HashSet<String>) {
    match node {
      Stmt::Let(name, expr, _) => {
        self.visit_expr(expr, live_vars);

        for live_var in live_vars.iter() {
          self.add_edge(name.clone(), live_var.clone());
        }

        live_vars.insert(name.clone());
      }
      Stmt::Block(stmts) => {
        let mut block_live_vars = live_vars.clone();

        for stmt in stmts {
          self.visit_stmt(stmt, &mut block_live_vars);
        }

        *live_vars = live_vars.union(&block_live_vars).cloned().collect();
      }
      _ => {}
    }
  }

  fn visit_expr(&mut self, node: &Expr, live_vars: &mut HashSet<String>) {
    match node {
      Expr::Variable(name, _) => {
        live_vars.insert(name.clone());
      }
      Expr::BinaryOp(lhs, _, rhs, _) => {
        self.visit_expr(lhs, live_vars);
        self.visit_expr(rhs, live_vars);
      }
      _ => {}
    }
  }

  pub fn print(&self) {
    for (var, neighbors) in &self.adj_list {
      println!("{} -> {:?}", var, neighbors);
    }
  }

  pub fn print_colored(&self) {
    let allocations = self.get_allocations();
    for (var, color) in allocations {
      println!("{} -> {}", var, color);
    }
  }
}