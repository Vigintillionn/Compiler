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
mod sourcemap;
mod staticanalysis;

fn main() -> Result<(), String> {
    let code = "
        if (a) {
        
        }
    ";

    let tokens = tokenize(code);
    let source_map = sourcemap::SourceMap::new(code);

    let Ok(tokens) = tokens else {
        let errors = tokens.unwrap_err();
        errors.iter().for_each(|e| report_error(e, &source_map));
        eprintln!("Found {} errors", errors.len());

        return Err("Failed to tokenize".to_string());
    };

    let ast = parse(&tokens);

    let Ok(ast) = ast else {
        let errors = ast.unwrap_err();
        errors.iter().for_each(|e| report_error(e, &source_map));
        eprintln!("Found {} errors", errors.len());

        return Err("Failed to parse".to_string());
    };

    let ast = type_check_program(ast)?;
    let ast = cf_check_program(ast)?;

    eval_program(ast);

    Ok(())
}
