use std::fmt;

use crate::{sourcelines::SourceLines, Token};

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

impl From<&Token> for Location {
    fn from(token: &Token) -> Self {
        Location {
            line: token.line,
            col: token.col,
            pos: token.pos,
        }
    }
}

pub trait ReportableError: fmt::Display {
    fn get_col_pos(&self) -> (usize, usize);
    fn len(&self) -> usize;
}

pub fn report_error<E: ReportableError>(err: E, code: &str, source_lines: &SourceLines) {
    let (col, pos) = err.get_col_pos();
    let line_idx = source_lines.find_line(pos);
    let (line_start, line_end) = source_lines.line_range(line_idx);
    let line_src = &code[line_start..line_end];

    eprintln!(
        "\x1b[93mError\x1b[0m at line {}:{}: {}",
        line_idx + 1,
        col,
        err
    );
    eprintln!("     |");
    eprintln!("{:4} | {}", line_idx + 1, line_src.trim_end());
    eprintln!(
        "     | {:>width$}{}",
        "",
        "^".repeat(err.len()),
        width = col - 1
    );
}
