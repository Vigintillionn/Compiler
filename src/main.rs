use interpreter::Eval;
pub use lexer::{tokens::Token, tokenize};
pub use errors::{LexerError, LexerResult};

mod lexer;
mod parser;
mod interpreter;
mod errors;

fn main() -> Result<(), LexerError> {
  let code = "
    proc fib(n: int) -> int {
      if (n <= 1) {
        ret n;
      }

      ret fib(n - 1) + fib(n - 2);
    }

    print(fib(10));
  ";

  let tokens = tokenize(code)?;
  println!("{:?}", tokens);

  let ast = parser::parse(tokens.as_slice()).map_err(|e| {
    eprintln!("{}", e);
    LexerError::UnexpectedEOF
  })?;
  println!("{:?}", ast);

  ast.eval(&mut interpreter::Environment::new());

  Ok(())
}
