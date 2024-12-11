use lexer::lex;
use parser;

fn main() {
  let str = "let x = 42 + 1 * 3;";
  
  match lex(str) {
    Ok(tokens) => {
      let (_, stmt) = parser::parse(&tokens).unwrap();
      println!("{:?}", stmt);
    },
    Err(e) => {
      eprintln!("{}", e);
    }
  }
}
