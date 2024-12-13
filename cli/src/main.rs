use lexer::lex;
use parser;

fn main() {
  let str = "{
    let x = 42 + 1 * 3;
    x = x + 1;
  }";
  match lex(str) {
    Ok(tokens) => {
      println!("{:?}", tokens);
      let (_, stmt) = parser::parse(&tokens).unwrap();
      println!("{:?}", stmt);
      compiler::compile(&stmt);
    },
    Err(e) => {
      eprintln!("{}", e);
    }
  }
}
