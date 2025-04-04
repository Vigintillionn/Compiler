use interpreter::Eval;
pub use lexer::{tokens::Token, tokenize};
pub use errors::{LexerError, LexerResult};

mod lexer;
mod parser;
mod interpreter;
mod errors;
mod sourcelines;

fn main() -> Result<(), String> {
  let code: &str = "proc fib(n: int) -> int {
      var a: int = 0;
      for (var i = 0; i < n; i = i + 1) {
      if (i == 4) {
        break;
      }
        a = a + i;
      }
      ret a;
    }

    print(fib(10));
  ";

  let tokens = tokenize(code);
  let source_lines = sourcelines::SourceLines::new(code);

  if let Ok(tokens) = tokens {
    let ast = parser::parse(tokens.as_slice()).unwrap();
    println!("{:?}", ast);
  
    ast.eval(&mut interpreter::Environment::new());
  } else {
    let errors = tokens.unwrap_err();
    for err in &errors {
      match err {
        LexerError::InvalidCharacter(found, line, col, pos) => {
          let line_idx = source_lines.find_line(*pos);
          let (line_start, line_end) = source_lines.line_range(line_idx);
          let line_src = &code[line_start..line_end];

          eprintln!("Error at line {}:{}: invalid character '{}'", line_idx + 1, col, found);
          eprintln!("{:4} | {}", line_idx + 1, line_src.trim_end());
          eprintln!("     | {:>width$}^", "", width = col);
        },
        _ => todo!()
      }
    }
    eprintln!("Found {} errors", errors.len());
  }
  




  Ok(())
}
