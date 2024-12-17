use lexer::lex;
use parser;

fn main() {
  let str = "
    for (;;) {
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
