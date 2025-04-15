mod keywords {
    use super::TokenKind::*;
    pub const KEYWORDS: phf::Map<&'static str, super::TokenKind> = phf::phf_map! {
      "proc" => Proc,
      "var" => Var,
      "if" => If,
      "else" => Else,
      "ret" => Ret,
      "true" => BoolLiteral(true),
      "false" => BoolLiteral(false),
      "int" => Int,
      "bool" => Bool,
      "str" => Str,
      "for" => For,
      "while" => While,
      "loop" => Loop,
      "break" => Break,
      "continue" => Continue,
    };
}

pub use keywords::KEYWORDS;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),

    /* Literals */
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),

    /* Keywords */
    While,
    For,
    Loop,
    If,
    Else,
    Var,
    Ret,
    Proc,
    Break,
    Continue,

    /* Types */
    Int,
    Float,
    Str,
    Bool,

    /* Operators */
    Plus,      // +
    Minus,     // -
    Asterisk,  // *
    Slash,     // /
    Assign,    // =
    Bang,      // !
    Eq,        // ==
    NotEq,     // !=
    LThan,     // <
    GThan,     // >
    LThanEq,   // <=
    GThanEq,   // >=
    Ampersand, // &

    /* Other */
    Semi,       // ;
    Colon,      // :
    OpenParen,  // (
    CloseParen, // )
    OpenBrace,  // {
    CloseBrace, // }
    Comma,      // ,
    Arrow,      // ->
    And,        // &&
    Or,         // ||
}

#[derive(Debug, Clone)]
pub enum Assoc {
    Left,
    Right,
}

pub struct OpInfo {
    pub prec: u8,
    pub assoc: Assoc,
    pub is_unary: bool,
}

impl TokenKind {
    pub fn op_info(&self) -> Option<OpInfo> {
        use TokenKind::*;
        match self {
            OpenParen | CloseParen => Some(OpInfo {
                prec: 0,
                assoc: Assoc::Left,
                is_unary: true,
            }),
            Plus | Minus => Some(OpInfo {
                prec: 3,
                assoc: Assoc::Left,
                is_unary: false,
            }),
            Asterisk | Slash => Some(OpInfo {
                prec: 4,
                assoc: Assoc::Left,
                is_unary: false,
            }),
            Eq | NotEq | LThan | GThan | LThanEq | GThanEq => Some(OpInfo {
                prec: 2,
                assoc: Assoc::Left,
                is_unary: false,
            }),
            And | Or => Some(OpInfo {
                prec: 1,
                assoc: Assoc::Left,
                is_unary: false,
            }),
            Bang => Some(OpInfo {
                prec: 15,
                assoc: Assoc::Right,
                is_unary: true,
            }),
            Ampersand => Some(OpInfo {
                prec: 15,
                assoc: Assoc::Right,
                is_unary: true,
            }),
            _ => None,
        }
    }

    pub fn len(&self) -> usize {
        use TokenKind::*;
        match self {
            Plus | Minus | Asterisk | Slash | Assign | Bang | Eq | LThan | GThan | Semi | Colon
            | OpenParen | CloseParen | OpenBrace | CloseBrace | Comma | Ampersand => 1,
            Arrow | And | Or | If | NotEq | LThanEq | GThanEq => 2,
            For | Var | Ret | Int | Str => 3,
            Loop | Else | Proc | Bool => 4,
            While | Break | Float => 5,
            Continue => 8,
            Identifier(s) | StringLiteral(s) => s.len(),
            IntLiteral(s) => s.to_string().len(),
            FloatLiteral(s) => s.to_string().len(),
            BoolLiteral(s) => s.to_string().len(),
        }
    }
}
