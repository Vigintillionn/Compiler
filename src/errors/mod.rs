use std::{fmt, ops::Range};

use crate::sourcemap::SourceMap;

pub mod analysis;
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

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
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
    fn report(&self, sourcemap: &SourceMap) {
        let (line_idx, col) = sourcemap.span_to_line_col(self.get_span().start);
        let line_src = sourcemap.line_text(line_idx).unwrap_or_default();

        eprintln!(
            "\x1b[93mError\x1b[0m at line {}:{}: {}",
            line_idx, col, self
        );
        eprintln!("     |");
        eprintln!("{:4} | {}", line_idx, line_src.trim_end());
        eprintln!(
            "     | {:>width$}{}",
            "",
            "^".repeat(self.len()),
            width = col - 1
        );
    }
}
