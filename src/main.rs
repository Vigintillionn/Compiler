use errors::report_error;
use interpreter::eval_program;
pub use lexer::{tokenize, tokens::Token};
pub use parser::parse;
use staticanalysis::type_check_program;

mod environment;
mod errors;
mod interpreter;
mod lexer;
mod parser;
mod sourcelines;
mod staticanalysis;

fn main() -> Result<(), String> {
    let code = "
        proc a(b: &int) -> int {
            var c = 10;
            var d = 20;
            *b = c + d;
            ret *b + d;
        }

        var x: int = 0;
        var y: int = a(&x);
        print(x, y);
    ";

    let tokens = tokenize(code);
    let source_lines = sourcelines::SourceLines::new(code);

    if let Ok(tokens) = tokens {
        let ast = parse(tokens.as_slice());
        println!("{:#?}", ast);
        if let Ok(ast) = ast {
            type_check_program(&ast)?;
            println!("{:#?}", ast);
            eval_program(ast);
        } else {
            let errors = ast.unwrap_err();
            errors
                .iter()
                .for_each(|e| report_error(e, code, &source_lines));
            // for err in &errors {
            //   match err {
            //     ParserError::ExpectedIdentifier(tok) => {
            //       // println!("Expected identifier at line {}:{}: {}", line, col, pos);
            //       let pos = tok.pos;
            //       let col = tok.col;
            //       let line_idx = source_lines.find_line(pos);
            //       let (line_start, line_end) = source_lines.line_range(line_idx);
            //       let line_src = &code[line_start..line_end];
            //       let len = tok.kind.len();

            //       eprintln!("\x1b[93mError\x1b[0m at line {}:{}: expected identifier", line_idx + 1, col);
            //       eprintln!("{:4} | {}", line_idx + 1, line_src.trim_end());
            //       eprintln!("     | {:>width$}{}", "", "^".repeat(len), width = col-1);
            //     },
            //     _ => unimplemented!()
            //   }
            // }
        }
    } else {
        let errors = tokens.unwrap_err();
        errors
            .iter()
            .for_each(|e| report_error(e, code, &source_lines));
        eprintln!("Found {} errors", errors.len());
    }

    Ok(())
}
