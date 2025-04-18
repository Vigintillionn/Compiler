use std::fmt;

use crate::sourcemap::SourceMap;

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        // assert!(
        //     start <= end,
        //     "Start index must be less than or equal to end index"
        // );
        Span { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub trait ReportableError: fmt::Display {
    fn get_span(&self) -> &Span;
    fn len(&self) -> usize {
        self.get_span().len()
    }
}

pub fn report_error<E: ReportableError>(err: E, sourcemap: &SourceMap) {
    let (line_idx, col) = sourcemap.span_to_line_col(err.get_span().start);
    let line_src = sourcemap.line_text(line_idx).unwrap_or_default();

    eprintln!("\x1b[93mError\x1b[0m at line {}:{}: {}", line_idx, col, err);
    eprintln!("     |");
    eprintln!("{:4} | {}", line_idx, line_src.trim_end());
    eprintln!(
        "     | {:>width$}{}",
        "",
        "^".repeat(err.len()),
        width = col - 1
    );
}

pub enum CompilerError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
}

impl Into<CompilerError> for lexer::LexerError {
    fn into(self) -> CompilerError {
        CompilerError::LexerError(self)
    }
}

impl Into<CompilerError> for parser::ParserError {
    fn into(self) -> CompilerError {
        CompilerError::ParserError(self)
    }
}
