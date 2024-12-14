use lexer::lex;
use parser;

fn main() {
  let str = "
    let x = 2; 
    if x <= 3 { 
      while 1 { 
        let x = 3; 
      } 
    } 
    let y = 3;
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
