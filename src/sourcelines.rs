pub struct SourceLines {
  line_starts: Vec<usize>,
}

impl SourceLines {
  pub fn new(src: &str) -> Self {
    let mut line_starts = vec![0];
    let mut pos = 0;

    for c in src.chars() {
      if c == '\n' {
        line_starts.push(pos + 1);
      }
      pos += c.len_utf8();
    }

    SourceLines { line_starts }
  }

  pub fn find_line(&self, pos: usize) -> usize {
    match self.line_starts.binary_search(&pos) {
      Ok(line) => line,
      Err(line) => {
        if line == 0 {
          0
        } else {
          line - 1
        }
      }
    }
  }

  pub fn line_range(&self, line: usize) -> (usize, usize) {
    let start = self.line_starts[line];
    let end = self.line_starts.get(line + 1)
      .copied()
      .unwrap_or_else(|| start + self.line_starts[line..].iter().sum::<usize>());
    (start, end)
  }
}