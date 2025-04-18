pub struct SourceMap<'a> {
    pub src: &'a str,
    pub line_starts: Vec<usize>,
}

impl<'a> SourceMap<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in src.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        Self { src, line_starts }
    }

    pub fn span_to_line_col(&self, pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&pos) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        };
        let col = pos - self.line_starts[line];
        (line + 1, col + 1)
    }

    pub fn line_text(&self, line: usize) -> Option<&'a str> {
        let start = *self.line_starts.get(line - 1)?;
        let end = *self.line_starts.get(line).unwrap_or(&self.src.len());
        Some(&self.src[start..end])
    }
}
