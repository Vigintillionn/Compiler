pub use lexer::{tokens::Token, tokenize};
pub use errors::{LexerError, LexerResult};

mod lexer;
mod parser;
mod errors;

fn main() -> Result<(), LexerError> {
  let code = "
  proc main(a: int, b: int) -> int {
    var a = 10;
    ret;
  }
  ";

  let tokens = tokenize(code)?;
  println!("{:?}", tokens);

  let ast = parser::parse(tokens.as_slice());
  println!("{:?}", ast);

  Ok(())
}
