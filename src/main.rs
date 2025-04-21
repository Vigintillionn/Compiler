use errors::ReportableError;
use interpreter::eval_program;
pub use lexer::{tokenize, tokens::Token};
pub use parser::parse;
use sourcemap::SourceMap;
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
        proc test(x: int) -> int {
            if (x > 10) {
                ret 1;
            }
            var a = 10;
            ret 20;
        }

        ret 20;
        print(test(10));
    ";

    let tokens = tokenize(code);
    let source_map = SourceMap::new(code);

    let tokens = handle_errors(&source_map, tokens, |t| t)?;
    let ast = handle_errors(&source_map, tokens.as_slice(), |t| parse(t))?;
    let ast = handle_errors(&source_map, ast, |t| type_check_program(t))?;
    let ast = handle_errors(&source_map, ast, |t| cf_check_program(t))?;

    eval_program(ast);

    Ok(())
}

fn handle_errors<F, E, I, O>(src: &SourceMap, input: I, f: F) -> Result<O, String>
where
    O: std::fmt::Debug,
    E: ReportableError,
    F: Fn(I) -> Result<O, Vec<E>>,
{
    let out = f(input);
    let Ok(out) = out else {
        let errors = out.unwrap_err();
        let len = errors.len();
        for e in errors {
            e.report(src);
        }
        eprintln!("Found {} errors", len);

        return Err("Failed to compile".to_string());
    };

    Ok(out)
}
