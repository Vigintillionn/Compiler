use errors::report_error;
use interpreter::eval_program;
pub use lexer::{tokenize, tokens::Token};
pub use parser::parse;
use staticanalysis::{cf_check_program, type_check_program};

mod environment;
mod errors;
mod interpreter;
mod lexer;
mod parser;
pub mod program;
mod sourcelines;
mod staticanalysis;

fn main() -> Result<(), String> {
    let code = "
        proc a(b: &int) -> int {
            if(true) {
                *b = 2;
                ret 1;
            } else {

                var c = 10;
                var d = 20;
                *b = c + d;
                
            }
            ret *b;
        }

        var x: int = 0;
        var y: int = a(&x);
        print(x, y);
    ";

    let tokens = tokenize(code);
    let source_lines = sourcelines::SourceLines::new(code);

    let Ok(tokens) = tokens else {
        let errors = tokens.unwrap_err();
        errors
            .iter()
            .for_each(|e| report_error(e, code, &source_lines));
        eprintln!("Found {} errors", errors.len());

        return Err("Failed to tokenize".to_string());
    };

    let ast = parse(&tokens);

    let Ok(ast) = ast else {
        let errors = ast.unwrap_err();
        errors
            .iter()
            .for_each(|e| report_error(e, code, &source_lines));
        eprintln!("Found {} errors", errors.len());

        return Err("Failed to parse".to_string());
    };

    let ast = type_check_program(ast)?;
    let ast = cf_check_program(ast)?;

    eval_program(ast);

    Ok(())
}
