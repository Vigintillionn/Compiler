pub mod tokens;
use tokens::{Token, TokenKind, KEYWORDS};

use crate::{
    errors::lexer::{LexerError, LexerResult},
    errors::Location,
};

struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn get_loc(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
            pos: self.pos,
        }
    }

    fn take_while<F>(&mut self, mut pred: F) -> (&'a str, usize)
    where
        F: FnMut(char) -> bool,
    {
        let end = self
            .src
            .chars()
            .position(|c| !pred(c))
            .unwrap_or(self.src.len());
        (&self.src[..end], end)
    }

    fn skip_whitespace(&mut self) -> bool {
        let (_, bytes) = self.take_while(|c| c.is_whitespace());
        self.chomp(bytes);

        bytes > 0
    }

    fn skip_comments(&mut self) -> bool {
        let pairs = [("//", "\n"), ("/*", "*/")];

        let mut skipped = false;

        for &(start, end) in pairs.iter() {
            if self.src.starts_with(start) {
                self.skip_until(end);
                skipped = true;
            }
        }

        skipped
    }

    fn skip_until(&mut self, pat: &str) {
        while !self.src.is_empty() && !self.src.starts_with(pat) {
            let next = self.src.chars().next().expect("");
            self.chomp(next.len_utf8());
        }

        if !self.src.is_empty() {
            self.chomp(pat.len());
        }
    }

    fn skip(&mut self) {
        loop {
            let ws = self.skip_whitespace();
            let comments = self.skip_comments();

            if !ws && !comments {
                break;
            }
        }
    }

    fn tokenize_cmp(&mut self) -> LexerResult<(TokenKind, usize)> {
        let cmp = self.src.chars().next().unwrap();
        let tokens = [
            ('=', TokenKind::Assign, TokenKind::Eq),
            ('!', TokenKind::Bang, TokenKind::NotEq),
            ('<', TokenKind::LThan, TokenKind::LThanEq),
            ('>', TokenKind::GThan, TokenKind::GThanEq),
        ];

        for (pat, tok1, tok2) in tokens.iter() {
            if cmp == *pat {
                if self.src.chars().nth(1) == Some('=') {
                    return Ok((tok2.clone(), 2));
                } else {
                    return Ok((tok1.clone(), 1));
                }
            }
        }

        Err(LexerError::InvalidCharacter(self.get_loc(), cmp))
    }

    fn tokenize_single_token(&mut self) -> LexerResult<(Token, usize)> {
        let next = self
            .src
            .chars()
            .next()
            .ok_or(LexerError::UnexpectedEOF(self.get_loc()))?;

        let (tok, bytes) = match next {
            '(' => (TokenKind::OpenParen, 1),
            ')' => (TokenKind::CloseParen, 1),
            '{' => (TokenKind::OpenBrace, 1),
            '}' => (TokenKind::CloseBrace, 1),
            ';' => (TokenKind::Semi, 1),
            ':' => (TokenKind::Colon, 1),
            ',' => (TokenKind::Comma, 1),
            '+' => (TokenKind::Plus, 1),
            '-' => {
                if self.src.chars().nth(1) == Some('>') {
                    (TokenKind::Arrow, 2)
                } else {
                    (TokenKind::Minus, 1)
                }
            }
            '*' => (TokenKind::Asterisk, 1),
            '/' => (TokenKind::Slash, 1),
            '=' | '!' | '<' | '>' => self.tokenize_cmp()?,
            '&' => {
                if self.src.chars().nth(1) == Some('&') {
                    (TokenKind::And, 2)
                } else {
                    return Err(LexerError::InvalidCharacter(self.get_loc(), next));
                }
            }
            '|' => {
                if self.src.chars().nth(1) == Some('|') {
                    (TokenKind::Or, 2)
                } else {
                    return Err(LexerError::InvalidCharacter(self.get_loc(), next));
                }
            }
            '0'..='9' => self.tokenize_number()?,
            c if c.is_alphanumeric() || c == '_' => self.tokenize_ident()?,
            _ => return Err(LexerError::InvalidCharacter(self.get_loc(), next)),
        };

        Ok((
            Token {
                kind: tok,
                line: self.line,
                col: self.col,
                pos: self.pos,
            },
            bytes,
        ))
    }

    fn tokenize_number(&mut self) -> LexerResult<(TokenKind, usize)> {
        let (num, bytes) = self.take_while(|c| c.is_ascii_digit() || c == '.');
        if num.contains('.') {
            let num = num
                .parse::<f64>()
                .map_err(|_| LexerError::InvalidNumber(self.get_loc(), num.to_string()))?;
            Ok((TokenKind::FloatLiteral(num), bytes))
        } else {
            let num = num
                .parse::<i64>()
                .map_err(|_| LexerError::InvalidNumber(self.get_loc(), num.to_string()))?;
            Ok((TokenKind::IntLiteral(num), bytes))
        }
    }

    fn tokenize_ident(&mut self) -> LexerResult<(TokenKind, usize)> {
        let (ident, bytes) = self.take_while(|c| c.is_alphanumeric() || c == '_');

        let tok = KEYWORDS
            .get(ident)
            .cloned()
            .unwrap_or(TokenKind::Identifier(ident.to_string()));

        Ok((tok, bytes))
    }

    fn chomp(&mut self, bytes: usize) {
        let chunk = &self.src[..bytes];
        for c in chunk.chars() {
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += c.len_utf8();
            }
        }

        self.src = &self.src[bytes..];
        self.pos += bytes;
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexerResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip();
        if self.src.is_empty() {
            return None;
        }

        let (tok, bytes) = match self.tokenize_single_token() {
            Ok((tok, bytes)) => (tok, bytes),
            Err(e) => {
                self.chomp(1);
                return Some(Err(e));
            }
        };

        self.chomp(bytes);
        Some(Ok(tok))
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, Vec<LexerError>> {
    let lexer = Lexer::new(src);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    for token in lexer {
        match token {
            Ok(tok) => tokens.push(tok),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}
