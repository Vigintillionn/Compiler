mod compiler;
mod instruction;
mod ssa;
use compiler::Compiler;
use parser::ast::Stmt;

pub fn compile(stmt: &Stmt) {
  let mut compiler = Compiler::new();

  compiler.compile(stmt, "out/output.asm");
}

#[cfg(test)]
mod tests {

}
