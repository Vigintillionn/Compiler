use crate::{
    errors::Spanned,
    lexer::tokens::{Assoc, TokenKind},
    Token,
};
use core::fmt;
use std::cell::RefCell;

pub type Block = Vec<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Boolean,
    Function(Vec<Type>, Box<Type>, bool), // bool indicates if it has variadic arguments
    Void,
    Pointer(Box<Type>),
    Any,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            String => write!(f, "string"),
            Boolean => write!(f, "bool"),
            Function(args, ret, _) => {
                write!(f, "proc(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", ret)
            }
            Void => write!(f, "void"),
            Pointer(inner) => write!(f, "&{}", inner),
            Any => write!(f, "any"),
        }
    }
}

pub type Stmt = Spanned<StmtKind>;

#[derive(Debug, Clone)]
pub enum StmtKind {
    Block(Block),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Var(String, Expr, RefCell<Option<Type>>),
    Assign(Expr, Expr),
    Loop(
        Option<Box<Stmt>>,
        Option<Expr>,
        Option<Box<Stmt>>,
        Box<Stmt>,
    ),
    Ret(Option<Expr>),
    Function(String, Vec<(String, Type)>, Type, Box<Stmt>),
    Expr(Expr),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

pub type Expr = Spanned<ExprInner>;

#[derive(Debug, Clone)]
pub struct ExprInner(pub ExprKind, pub RefCell<Option<Type>>);

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(LiteralValue),
    Ident(String),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct BinaryOp(pub Box<Expr>, pub Op, pub Box<Expr>);
#[derive(Debug, Clone)]
pub struct UnaryOp(pub Op, pub Box<Expr>);

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    And,
    Or,
    Not,

    Negate,
    Deref,
    AddressOf,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Op::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Eq => write!(f, "=="),
            Neq => write!(f, "!="),
            Lt => write!(f, "<"),
            Lte => write!(f, "<="),
            Gt => write!(f, ">"),
            Gte => write!(f, ">="),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Not => write!(f, "!"),
            Negate => write!(f, "-"),
            Deref => write!(f, "*"),
            AddressOf => write!(f, "&"),
        }
    }
}

impl From<&DisambiguatedOp> for Op {
    fn from(op: &DisambiguatedOp) -> Self {
        use TokenKind::*;
        match op.token.kind {
            Plus => Self::Add,
            Minus => {
                if op.is_unary {
                    Self::Negate
                } else {
                    Self::Sub
                }
            }
            Asterisk => {
                if op.is_unary {
                    Self::Deref
                } else {
                    Self::Mul
                }
            }
            Slash => Self::Div,
            Eq => Self::Eq,
            NotEq => Self::Neq,
            LThan => Self::Lt,
            LThanEq => Self::Lte,
            GThan => Self::Gt,
            GThanEq => Self::Gte,
            And => Self::And,
            Or => Self::Or,
            Bang => Self::Not,
            Ampersand => Self::AddressOf,
            _ => panic!("This is not a valid operator! {:?}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DisambiguatedOp {
    pub is_unary: bool,
    pub prec: u8,
    pub assoc: Assoc,
    pub token: Token,
}

impl From<&Token> for DisambiguatedOp {
    fn from(token: &Token) -> Self {
        if matches!(
            token,
            Token {
                kind: TokenKind::Identifier(_),
                ..
            }
        ) {
            DisambiguatedOp {
                is_unary: true,
                prec: 50,
                token: token.clone(),
                assoc: Assoc::Left,
            }
        } else {
            let op_info = token.kind.op_info().unwrap();
            DisambiguatedOp {
                is_unary: op_info.is_unary,
                prec: op_info.prec,
                token: token.clone(),
                assoc: op_info.assoc,
            }
        }
    }
}

pub fn disambiguate(token: &Token, prev: Option<TokenKind>) -> DisambiguatedOp {
    if matches!(token.kind, TokenKind::Asterisk | TokenKind::Minus) {
        let is_unary = match prev {
            None => true,                           // If it's the first token, treat it as unary
            Some(TokenKind::OpenParen) => true, // If it's after an open parenthesis, treat it as unary
            Some(kind) => kind.op_info().is_some(), // If the previous token is an operator, treat it as unary
        };

        let prec = if is_unary {
            15
        } else {
            token.kind.op_info().unwrap().prec
        };

        DisambiguatedOp {
            is_unary,
            prec,
            assoc: Assoc::Right,
            token: token.clone(),
        }
    } else {
        let prec = token.kind.op_info().unwrap().prec;
        let assoc = token.kind.op_info().unwrap().assoc;
        let is_unary = token.kind.op_info().unwrap().is_unary;
        DisambiguatedOp {
            is_unary,
            prec,
            assoc,
            token: token.clone(),
        }
    }
}
