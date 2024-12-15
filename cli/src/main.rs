use lexer::lex;
use parser;

fn main() {
  let str = "
    let x = 2;
    let y: uint = 1 + x + 2;
    let z = true;
  ";
  match lex(str) {
    Ok(tokens) => {
      println!("{:?}", tokens);
      let stmt = parser::parse(&tokens).unwrap();
      stmt.print(0);
      compiler::compile(&stmt);
    },
    Err(e) => {
      eprintln!("{}", e);
    }
  }
}
