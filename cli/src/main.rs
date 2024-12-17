use lexer::lex;
use parser;

fn main() {
  let str = "
    for (let x = 1; x < 10; x = x + 1) {
    x = x + 1;}
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
