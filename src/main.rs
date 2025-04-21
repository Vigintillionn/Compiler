use interpreter::eval_program;
use lexer::{tokenize, tokens::Token};
use parser::parse;
use pipeline::{handle_errors, Pipeline};
use sourcemap::SourceMap;
use staticanalysis::{cf_check_program, type_check_program};

mod environment;
mod errors;
mod interpreter;
mod lexer;
mod parser;
mod pipeline;
mod program;
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

    let source_map = SourceMap::new(code);
    let tokens = handle_errors(code, &source_map, tokenize)?;

    let ast = tokens
        .as_slice()
        .with_ctx(&source_map, parse)
        .with_ctx(&source_map, type_check_program)
        .with_ctx(&source_map, cf_check_program)?;

    eval_program(ast);

    Ok(())
}
