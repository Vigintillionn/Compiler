use std::collections::HashMap;
use parser::{ast::{Expr, Stmt}, types::Type};

use crate::{instruction::{Instruction, InstructionSet}, registeralloc::InferenceGraph};

pub struct Compiler {
  variables: HashMap<String, i32>,  // Maps variables to stack offsets
  stack_offset: i32,                // Current stack offset
  instructions: InstructionSet,     // The set of instructions built by the compiler
}

impl Compiler {
  pub fn new() -> Self {
    Compiler {
      variables: HashMap::new(),
      stack_offset: 0,
      instructions: InstructionSet::new(),
    }
  }

  pub fn compile(&mut self, stmts: &Stmt, file_name: &str) {
    // self.register_allocations = InferenceGraph::from_ast(stmts).get_allocations();
    self.compile_stmt(stmts);
    self.instructions.export(file_name).unwrap();
  }

  fn compile_stmt(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::Block(stmts) | Stmt::Program(stmts) => {
        for stmt in stmts {
          self.compile_stmt(stmt);
        }
      },
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
      Stmt::While(expr, block) => {
        let mut instructions = InstructionSet::default();
        let start_label = format!("L{}", self.instructions.len());
        let end_label = format!("L{}", self.instructions.len() + 1);
        instructions.add(Instruction::new_label(&start_label));
        self.compile_expr(expr, &mut instructions);
        instructions.add(Instruction::from(format!("beqz a0, {}", end_label)));
        self.instructions.extend(instructions);

        self.compile_stmt(block);
        self.instructions.add(Instruction::from(format!("j {}", start_label)));
        self.instructions.add(Instruction::new_label(&end_label));
      },
      Stmt::If(cond, then, else_then) => {
        let mut instructions = InstructionSet::default();
        self.compile_expr(cond, &mut instructions);

        let else_label = format!("L{}", self.instructions.len());
        let end_label = format!("L{}", self.instructions.len() + 1);
        instructions.add(Instruction::from(format!("beqz a0, {}", if else_then.is_some() { &else_label } else { &end_label })));

        self.instructions.extend(instructions);
        self.compile_stmt(then);

        if let Some(else_then) = else_then {
          self.instructions.add(Instruction::from(format!("j {}", end_label)));
          self.instructions.add(Instruction::new_label(&else_label));
          self.compile_stmt(else_then);
        }

        self.instructions.add(Instruction::new_label(&end_label));
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
          "<" => instructions.add(Instruction::from("slt a0, a0, t0".to_string())),
          "<=" => {
            instructions.add(Instruction::from("slt a0, a0, t0".to_string()));
            instructions.add(Instruction::from("xori a0, a0, 1".to_string()));
          },
          ">" => instructions.add(Instruction::from("slt a0, t0, a0".to_string())),
          ">=" => {
            instructions.add(Instruction::from("slt a0, t0, a0".to_string()));
            instructions.add(Instruction::from("xori a0, a0, 1".to_string()));
          },
          "==" => {
            instructions.add(Instruction::from("sub a0, t0, a0".to_string()));
            instructions.add(Instruction::from("seqz a0, a0".to_string()));
          },
          "!=" => {
            instructions.add(Instruction::from("sub a0, t0, a0".to_string()));
            instructions.add(Instruction::from("snez a0, a0".to_string()));
          }
          _ => unimplemented!(),
        };

        ty.as_ref().expect("Type not inferred").clone()
      }
    }
  }
}