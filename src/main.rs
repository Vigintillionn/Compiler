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

    type_check_program(&ast)?;
    eval_program(ast);

    Ok(())
}
