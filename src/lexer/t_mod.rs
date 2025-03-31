fn take_while<F>(src: &str, mut f: F) -> Result<(&str, usize), String>
where 
  F: FnMut(char) -> bool
{
  let mut chars = src.chars();
  let mut len = 0;
  while let Some(c) = chars.next() {
    if !f(c) {
      break;
    }
    len += c.len_utf8();
  }

  if len == 0 {
    Err("No characters matched".to_string())
  } else {
    Ok((&src[..len], len))
  }
}

fn tokenize_ident(src: &str) -> Result<(Token, usize), String> {
  src.chars().next().ok_or("No characters left")?;

  let (ident, bytes) = take_while(src, |c| c.is_alphanumeric() || c == '_')?;

  let token = match ident {
    /* Keywords */
    "while" => Token::While,
    "for" => Token::For,
    "loop" => Token::Loop,
    "if" => Token::If,
    "else" => Token::Else,
    "var" => Token::Var,
    "ret" => Token::Ret,
    "proc" => Token::Proc,

    /* Types */
    "int" => Token::Int,
    "float" => Token::Float,
    "str" => Token::Str,
    "bool" => Token::Bool,

    "true" => Token::True,
    "false" => Token::False,

    _ => Token::Identifier(ident.to_string())
  };

  Ok((token, bytes))
}

fn tokenize_number(src: &str) -> Result<(Token, usize), String> {
  src.chars().next().ok_or("No characters left")?;

  let (num, bytes) = take_while(src, |c| c.is_digit(10) || c == '.')?;
  let num = num.parse().map_err(|e| format!("Failed to parse number: {}", e))?;

  Ok((Token::Number(num), bytes))
}

fn skip_whitespace(stc: &str) -> usize {
  match take_while(stc, |c| c.is_whitespace()) {
    Ok((_, len)) => len,
    Err(_) => 0
  }
}

fn skip_comments(src: &str) -> usize {
  if src.starts_with("//") {
    let remainder = skip_until(src, "\n");
    return src.len() - remainder.len();
  }

  0
}

fn skip_until<'a>(mut src: &'a str, pat: &str) -> &'a str {
  while !src.is_empty() && !src.starts_with(pat) {
    let next = src.chars().next().expect("").len_utf8();
    src = &src[next..];
  }

  &src[pat.len()..]
}

fn tokenize_single_token(src: &str) -> Result<(Token, usize), String> {
  let next = src.chars().next().ok_or("No characters left")?;

  let (tok, bytes) = match next {
    '+' => (Token::Plus, 1),
    '-' => (Token::Minus, 1),
    '*' => (Token::Asterix, 1),
    '/' => (Token::Slash, 1),
    '=' => (Token::Eq, 1),
    '!' => (Token::Excl, 1),
    '<' => (Token::LThen, 1),
    '>' => (Token::GThen, 1),
    '0'..='9' => tokenize_number(src)?,
    c @ '_' | c if c.is_alphanumeric() => tokenize_ident(src)?,
    _ => return Err(format!("Unexpected character: {}", next))
  };

  Ok((tok, bytes))
}

struct Lexer<'a> {
  curr: usize,
  src: &'a str
}

impl<'a> Lexer<'a> {
  fn new(src: &'a str) -> Self {
    Lexer { curr: 0, src }
  }

  fn next_token(&mut self) -> Result<Option<Token>, String> {
    self.skip();

    if self.src.is_empty() {
      Ok(None)
    } else {
      let (tok, bytes) = tokenize_single_token(&self.src)?;
      self.consume(bytes);
      Ok(Some(tok))
    }
  }

  fn skip(&mut self) {
    let mut remaining = self.src;

    loop {
      let white_space = skip_whitespace(remaining);
      remaining = &remaining[white_space..];
      let comments = skip_comments(remaining);
      remaining = &remaining[comments..];

      if white_space + comments == 0 {
        break;
      }
    }

    self.consume(self.src.len() - remaining.len());
    self.src = remaining;
  }

  fn consume(&mut self, bytes: usize) {
    self.src = &self.src[bytes..];
    self.curr += bytes;
  }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, String> {
  let mut tokenizer = Lexer::new(src);
  let mut tokens = Vec::new();

  while let Some(tok) = tokenizer.next_token()? {
    tokens.push(tok);
  }

  Ok(tokens)
}