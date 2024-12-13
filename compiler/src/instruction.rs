use std::{fs::File, io::{self, Write}};

pub enum Instruction {
  Label(String),
  Operation { opcode: String, operands: Vec<String> }
}

impl Instruction {
  pub fn new(opcode: String, operands: Vec<String>) -> Self {
    Instruction::Operation {
      opcode,
      operands
    }
  }

  pub fn new_label(label: &str) -> Self {
    Instruction::Label(label.to_string())
  }

  pub fn from(str: String) -> Self {
    let (op, rest) = str.split_once(" ").unwrap();
    let operands: Vec<String> = rest.split(", ").map(|o| o.trim().to_string()).collect();
    Self::new(op.to_string(), operands)
  }

  pub fn build(&self) -> String {
    match self {
      Instruction::Label(label) => format!("{}:", label),
      Instruction::Operation { opcode, operands } => format!("{} {}", opcode, operands.join(", "))
    }
  }
}

pub struct InstructionSet {
  instructions: Vec<Instruction>
}

impl InstructionSet {
  pub fn new() -> Self {
    InstructionSet {
      instructions: vec![
        Instruction::Label("main".to_string())
      ]
    }
  }

  pub fn add(&mut self, instr: Instruction) {
    self.instructions.push(instr);
  }

  pub fn extend(&mut self, set: InstructionSet) {
    self.instructions.extend(set.instructions);
  }

  pub fn from(instr: Instruction) -> Self {
    InstructionSet {
      instructions: vec![instr]
    }  
  }

  pub fn export(&mut self, file_name: &str) -> io::Result<()> {
    let mut file = File::create(file_name)?;
    writeln!(file, ".text")?;
    writeln!(file, ".globl main")?;

    for instr in &self.instructions {
      writeln!(file, "{}", instr.build())?;
    }

    Ok(())
  }

  pub fn default() -> Self {
    InstructionSet {
      instructions: Vec::new()
    }
  }
}