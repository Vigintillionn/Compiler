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

use std::fmt;

pub use keywords::KEYWORDS;

use crate::errors::Span;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier(s) => write!(f, "{}", s),
            TokenKind::StringLiteral(s) => write!(f, "{}", s),
            TokenKind::IntLiteral(s) => write!(f, "{}", s),
            TokenKind::FloatLiteral(s) => write!(f, "{}", s),
            TokenKind::BoolLiteral(s) => write!(f, "{}", s),

            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Loop => write!(f, "loop"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::Ret => write!(f, "ret"),
            TokenKind::Proc => write!(f, "proc"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Int => write!(f, "int"),
            TokenKind::Float => write!(f, "float"),
            TokenKind::Str => write!(f, "str"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Assign => write!(f, "="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::NotEq => write!(f, "!="),
            TokenKind::LThan => write!(f, "<"),
            TokenKind::GThan => write!(f, ">"),
            TokenKind::LThanEq => write!(f, "<="),
            TokenKind::GThanEq => write!(f, ">="),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
        }
    }
}
