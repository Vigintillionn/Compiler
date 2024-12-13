use std::collections::HashMap;
use parser::{ast::{Expr, Stmt}, types::Type};

use crate::{instruction::{Instruction, InstructionSet}, registeralloc::InferenceGraph};

pub struct Compiler {
  variables: HashMap<String, i32>,  // Maps variables to stack offsets
  stack_offset: i32,                // Current stack offset
  instructions: InstructionSet,     // The set of instructions built by the compiler
  register_allocations: HashMap<String, String>, // Maps variables to registers
  register_spills: HashMap<String, i32>, // Maps spilled variables to stack offsets
}

impl Compiler {
  pub fn new() -> Self {
    Compiler {
      variables: HashMap::new(),
      stack_offset: 0,
      instructions: InstructionSet::new(),
      register_allocations: HashMap::new(),
      register_spills: HashMap::new(),
    }
  }

  pub fn compile(&mut self, stmts: &Stmt, file_name: &str) {
    self.register_allocations = InferenceGraph::from_ast(stmts).get_allocations();
    self.compile_stmt(stmts);
    self.instructions.export(file_name).unwrap();
  }

  fn compile_stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Expr(expr) => {
        let mut instructions = InstructionSet::default();
        self.compile_expr(expr, &mut instructions);
        self.instructions.extend(instructions);
      },
      Stmt::Let(name, expr, ty) => {
        let mut instructions = InstructionSet::default();
        let expr_type = self.compile_expr(expr, &mut instructions);

        if let Some(ty) = ty {
          if *ty != expr_type {
            panic!("Type mismatch: {:?} and {:?}", ty, expr_type);
          }
        }

        self.stack_offset -= 4; // Allocate space on the stack
        self.variables.insert(name.clone(), self.stack_offset);

        instructions.add(Instruction::from(format!("sw a0, {}(sp)", self.stack_offset)));
        self.instructions.extend(instructions);
      },
      Stmt::Block(stmts) => {
        for stmt in stmts {
          self.compile_stmt(stmt);
        }
      },
      Stmt::While(expr, block) => {
        let mut instructions = InstructionSet::default();
        let label = format!("L{}", self.instructions.len());
        instructions.add(Instruction::new_label(&label));
        self.compile_expr(expr, &mut instructions);
        instructions.add(Instruction::from("beqz a0, L1".to_string()));
        self.instructions.extend(instructions);

        self.compile_stmt(block);
        self.instructions.add(Instruction::from(format!("j {}", label)));
      },
      _ => unimplemented!(),
    }
  }

  fn compile_expr(&mut self, expr: &Expr, instructions: &mut InstructionSet) -> Type {
    match expr {
      Expr::Literal(value, ty) => {
        let inferred = ty.as_ref().expect("Type not inferred");
        match inferred {
          Type::Uint | Type::Int => instructions.add(Instruction::from(format!("li a0, {}", value))),
          _ => unimplemented!(),
        };

        inferred.clone()
      },
      Expr::Variable(name, ty) => {
        if let Some(offset) = self.variables.get(name) {
          instructions.add(Instruction::from(format!("lw a0, {}(sp)", offset)));

          ty.as_ref().expect("Type not inferred").clone()
        } else {
          panic!("Variable {} not found", name); // TODO: use a Result instead
        }
      },
      Expr::Assign(name, expr, ty) => {
        let mut instructions = InstructionSet::default();
        let expr_type = self.compile_expr(expr, &mut instructions);

        if let Some(ty) = ty {
          if *ty != expr_type {
            panic!("Type mismatch: {:?} and {:?}", ty, expr_type);
          }
        }

        instructions.add(Instruction::from("sw a0, 0(sp)".to_string()));
        instructions.add(Instruction::from(format!("sw a0, {}(sp)", self.variables.get(name).unwrap())));

        self.instructions.extend(instructions);

        expr_type
      },
      Expr::BinaryOp(left, op, right, ty) => {
        let left_type = self.compile_expr(left, instructions);
        instructions.add(Instruction::from("addi sp, sp, -4".to_string()));
        instructions.add(Instruction::from("sw a0, 0(sp)".to_string()));

        let right_type = self.compile_expr(right, instructions);
        instructions.add(Instruction::from("lw t0, 0(sp)".to_string()));
        instructions.add(Instruction::from("addi sp, sp, 4".to_string()));

        if left_type != right_type {
          panic!("Type mismatch: {:?} and {:?}", left_type, right_type);
        }

        match op.as_str() {
          "+" => instructions.add(Instruction::from("add a0, t0, a0".to_string())),
          "-" => instructions.add(Instruction::from("sub a0, t0, a0".to_string())),
          "*" => instructions.add(Instruction::from("mul a0, t0, a0".to_string())),
          "/" => instructions.add(Instruction::from("div a0, t0, a0".to_string())),
          _ => unimplemented!(),
        };

        ty.as_ref().expect("Type not inferred").clone()
      }
    }
  }

  fn get_register(&mut self, var: &str) -> String {
    if let Some(reg) = self.register_allocations.get(var) {
      return reg.clone()
    }

    if !self.variables.contains_key(var) {
      self.stack_offset -= 4;
      self.register_spills.insert(var.to_string(), self.stack_offset);
    }

    format!("{}(sp)", self.variables.get(var).unwrap())
  }

  fn generate_variable_instr(&mut self, var: &str, is_load: bool) -> String {
    let location = self.get_register(var);

    if is_load {
      format!("lw a0, {}", location)
    } else {
      format!("sw a0, {}", location)
    }
  }
}