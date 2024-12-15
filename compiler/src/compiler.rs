use std::collections::HashMap;
use parser::ast::{Expr, Literal, Stmt};
use crate::{instruction::{Instruction, InstructionSet}, ssa::Environment};

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

    let mut environment = Environment::new();
    environment.generate_from_stmt(stmts);
    environment.print();

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
      Stmt::Let(name, expr, _) => {
        let mut instructions = InstructionSet::default();
        self.compile_expr(expr, &mut instructions);

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

  fn compile_expr(&mut self, expr: &Expr, instructions: &mut InstructionSet) {
    match expr {
      Expr::Literal(value, _) => {
        match value {
          Literal::Integer(value) => {
            instructions.add(Instruction::from(format!("li a0, {}", value)));
          },
          Literal::Boolean(value) => {
            instructions.add(Instruction::from(format!("li a0, {}", if *value { 1 } else { 0 })));
          },
          _ => unimplemented!(),
        }
      },
      Expr::Variable(name, _) => {
        if let Some(offset) = self.variables.get(name) {
          instructions.add(Instruction::from(format!("lw a0, {}(sp)", offset)));
        } else {
          panic!("Variable {} not found", name); // TODO: use a Result instead
        }
      },
      Expr::Assign(name, expr, _) => {
        let mut instructions = InstructionSet::default();
        self.compile_expr(expr, &mut instructions);

        instructions.add(Instruction::from("sw a0, 0(sp)".to_string()));
        instructions.add(Instruction::from(format!("sw a0, {}(sp)", self.variables.get(name).unwrap())));

        self.instructions.extend(instructions);
      },
      Expr::BinaryOp(left, op, right, _) => {
        self.compile_expr(left, instructions);
        instructions.add(Instruction::from("addi sp, sp, -4".to_string()));
        instructions.add(Instruction::from("sw a0, 0(sp)".to_string()));

        self.compile_expr(right, instructions);
        instructions.add(Instruction::from("lw t0, 0(sp)".to_string()));
        instructions.add(Instruction::from("addi sp, sp, 4".to_string()));

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
      }
    }
  }
}