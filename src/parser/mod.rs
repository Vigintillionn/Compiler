use crate::{
    errors::parser::{ParserError, ParserResult},
    lexer::tokens::TokenKind,
    Token,
};
use ast::{BinaryOp, Expr, ExprKind, LiteralValue, Program, Stmt, Type, UnaryOp};
use framework::{
    alt, delimited, many, map, opt, preceded, seperated_list, terminated, Parser as ParserTrait,
};
use std::cell::RefCell;

pub mod ast;
mod framework;

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    fn parse_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (stmt, tokens) = alt((
            Self::parse_var_stmt,
            Self::parse_block,
            Self::parse_if_stmt,
            Self::parse_loop_stmt,
            Self::parse_while_stmt,
            Self::parse_for_loop,
            Self::parse_assign_stmt,
            Self::parse_ret_stmt,
            Self::parse_func_decl,
            map(terminated(TokenKind::Break, TokenKind::Semi), |_| {
                Stmt::Break
            }),
            map(terminated(TokenKind::Continue, TokenKind::Semi), |_| {
                Stmt::Continue
            }),
            map(terminated(Self::parse_expr, TokenKind::Semi), |expr| {
                Stmt::Expr(expr)
            }),
        ))(tokens)?;

        Ok((stmt, tokens))
    }

    fn parse_var_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::Var.parse(tokens)?;
        let (name, tokens) = Self::parse_ident(tokens)?;

        let (colon, tokens) = opt(TokenKind::Colon).parse(tokens)?;
        let (ty, tokens) = if colon.is_some() {
            Self::parse_type(tokens).map(|(ty, tokens)| (Some(ty), tokens))?
        } else {
            (None, tokens)
        };

        let (_, tokens) = TokenKind::Assign.parse(tokens)?;
        let (expr, tokens) = Self::parse_expr(tokens)?;
        let (_, tokens) = TokenKind::Semi.parse(tokens)?;

        Ok((Stmt::Var(name, expr, RefCell::new(ty)), tokens))
    }

    fn parse_assign_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (name, tokens) = Self::parse_ident(tokens)?;
        let (_, tokens) = TokenKind::Assign.parse(tokens)?;
        let (expr, tokens) = Self::parse_expr(tokens)?;
        let (_, tokens) = TokenKind::Semi.parse(tokens)?;

        Ok((Stmt::Assign(name, expr), tokens))
    }

    fn parse_if_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::If.parse(tokens)?;
        let (cond, tokens) = delimited(
            TokenKind::OpenParen,
            Self::parse_expr,
            TokenKind::CloseParen,
        )(tokens)?;
        let (then_block, tokens) = Self::parse_block(tokens)?;
        let (has_else, tokens) = opt(TokenKind::Else)(tokens)?;
        let (else_block, tokens) = if has_else.is_some() {
            Self::parse_block(tokens).map(|(else_block, tokens)| (Some(else_block), tokens))?
        } else {
            (None, tokens)
        };

        Ok((
            Stmt::If(cond, Box::new(then_block), else_block.map(Box::new)),
            tokens,
        ))
    }

    fn parse_loop_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::Loop.parse(tokens)?;
        let (block, tokens) = Self::parse_block(tokens)?;

        Ok((Stmt::Loop(None, None, None, Box::new(block)), tokens))
    }

    fn parse_while_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::While.parse(tokens)?;
        let (cond, tokens) = delimited(
            TokenKind::OpenParen,
            Self::parse_expr,
            TokenKind::CloseParen,
        )(tokens)?;
        let (block, tokens) = Self::parse_block(tokens)?;

        Ok((Stmt::Loop(None, Some(cond), None, Box::new(block)), tokens))
    }

    fn parse_for_loop(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::For.parse(tokens)?;
        let (_, tokens) = TokenKind::OpenParen.parse(tokens)?;
        let (init, tokens) = opt(alt((Self::parse_var_stmt, Self::parse_assign_stmt)))(tokens)?;

        let tokens = if init.is_none() {
            TokenKind::Semi.parse(tokens)?.1
        } else {
            tokens
        };

        let (cond, tokens) = opt(Self::parse_expr)(tokens)?;
        let (_, tokens) = TokenKind::Semi.parse(tokens)?;

        let (name, tokens) = opt(Self::parse_ident)(tokens)?;
        let (update, tokens) = if name.is_some() {
            let (_, tokens) = TokenKind::Assign.parse(tokens)?;
            let (update, tokens) = Self::parse_expr(tokens)?;

            (
                Some(Stmt::Assign(name.expect("Name is not some"), update)),
                tokens,
            )
        } else {
            (None, tokens)
        };

        let (_, tokens) = TokenKind::CloseParen.parse(tokens)?;
        let (block, tokens) = Self::parse_block(tokens)?;

        Ok((
            Stmt::Loop(
                init.map(Box::new),
                cond,
                update.map(Box::new),
                Box::new(block),
            ),
            tokens,
        ))
    }

    fn parse_ret_stmt(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (_, tokens) = TokenKind::Ret.parse(tokens)?;
        let (expr, tokens) = opt(Self::parse_expr)(tokens)?;
        let (_, tokens) = TokenKind::Semi.parse(tokens)?;

        Ok((Stmt::Ret(expr), tokens))
    }

    fn parse_func_decl(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let parse_arg = |tokens: &'a [Token]| {
            let (name, tokens) = Self::parse_ident(tokens)?;
            let (_, tokens) = TokenKind::Colon.parse(tokens)?;
            let (ty, tokens) = Self::parse_type(tokens)?;

            Ok(((name, ty), tokens))
        };

        let (_, tokens) = TokenKind::Proc.parse(tokens)?;
        let (name, tokens) = Self::parse_ident(tokens)?;
        let (_, tokens) = TokenKind::OpenParen.parse(tokens)?;

        let (args, tokens) = opt(seperated_list(parse_arg, TokenKind::Comma))(tokens)?;

        let (_, tokens) = TokenKind::CloseParen.parse(tokens)?;
        let (type_dec, tokens) = opt(TokenKind::Arrow)(tokens)?;

        let (ret_ty, tokens) = if type_dec.is_some() {
            Self::parse_type(tokens)?
        } else {
            (Type::Void, tokens)
        };

        let (block, tokens) = Self::parse_block(tokens)?;

        Ok((
            Stmt::Function(name, args.unwrap_or(vec![]), ret_ty, Box::new(block)),
            tokens,
        ))
    }

    fn parse_ident(tokens: &'a [Token]) -> ParserResult<(String, &'a [Token])> {
        if let Some((token, remaining)) = tokens.split_first() {
            match &token.kind {
                TokenKind::Identifier(name) => Ok((name.clone(), remaining)),
                _ => Err(ParserError::ExpectedIdentifier(token.clone())),
            }
        } else {
            // Err(ParserError::ExpectedIdentifier(0, 0, 0))
            Err(ParserError::Other("".to_string()))
        }
    }

    fn parse_block(tokens: &'a [Token]) -> ParserResult<(Stmt, &'a [Token])> {
        let (stmts, tokens) = delimited(
            TokenKind::OpenBrace,
            many(Self::parse_stmt),
            TokenKind::CloseBrace,
        )(tokens)?;

        Ok((Stmt::Block(stmts), tokens))
    }

    fn parse_expr(tokens: &[Token]) -> ParserResult<(Expr, &[Token])> {
        let mut tokens = tokens;
        let mut output: Vec<Expr> = Vec::new();
        let mut op_stack: Vec<&Token> = Vec::new();

        let mut cur_col = 0usize;
        let mut cur_line = 0usize;
        let mut cur_pos = 0usize;

        while let Some(token) = tokens.first() {
            let Token {
                kind,
                line,
                col,
                pos,
            } = token;
            cur_col = *col;
            cur_line = *line;
            cur_pos = *pos;
            match kind {
                TokenKind::Identifier(name) => {
                    if tokens.len() > 1 && tokens[1].kind == TokenKind::OpenParen {
                        tokens = &tokens[1..];
                        // Parse function call arguments
                        let (args, rest) = delimited(
                            TokenKind::OpenParen,
                            seperated_list(Self::parse_expr, TokenKind::Comma),
                            TokenKind::CloseParen,
                        )(tokens)?;
                        output.push(Expr(ExprKind::Call(name.clone(), args), RefCell::new(None)));
                        tokens = rest;
                        continue;
                    } else {
                        output.push(Expr(ExprKind::Ident(name.clone()), RefCell::new(None)))
                    }
                }
                TokenKind::IntLiteral(n) => output.push(Expr(
                    ExprKind::Literal(LiteralValue::Integer(*n)),
                    RefCell::new(Some(Type::Int)),
                )), // TODO: Handle typing
                TokenKind::FloatLiteral(f) => output.push(Expr(
                    ExprKind::Literal(LiteralValue::Float(*f)),
                    RefCell::new(Some(Type::Float)),
                )), // TODO: Handle typing
                TokenKind::BoolLiteral(b) => output.push(Expr(
                    ExprKind::Literal(LiteralValue::Boolean(*b)),
                    RefCell::new(Some(Type::Boolean)),
                )), // TODO: Handle typing
                _ if token.kind.op_info().is_some() => {
                    let op_info = token.kind.op_info().expect("Token op_info is None"); // token.op_info() is not None
                    if op_info.is_unary {
                        op_stack.push(token);
                    } else {
                        // First, apply any pending unary operators
                        while let Some(&top_op) = op_stack.last() {
                            let top_info = top_op
                                .kind
                                .op_info()
                                .expect("Token at top of stack is not an operator");
                            if top_info.is_unary {
                                op_stack.pop();
                                let operand = output.pop().expect("No operand for unary operator");
                                output.push(Expr(
                                    ExprKind::UnaryOp(UnaryOp(top_op.into(), Box::new(operand))),
                                    RefCell::new(None),
                                ));
                            } else {
                                break;
                            }
                        }

                        // Next, apply any pending binary operators
                        while let Some(&top_op) = op_stack.last() {
                            let top_info = top_op
                                .kind
                                .op_info()
                                .expect("Token at top of stack is not an operator");
                            if top_info.prec >= op_info.prec {
                                let rhs = output.pop().unwrap();
                                let lhs = output.pop().unwrap(); // TODO: handle unwrap
                                output.push(Expr(
                                    ExprKind::BinaryOp(BinaryOp(
                                        Box::new(lhs),
                                        top_op.into(),
                                        Box::new(rhs),
                                    )),
                                    RefCell::new(None),
                                ));
                                // op_stack.push(token); TODO: is this correct?
                            } else {
                                break;
                            }
                        }
                        op_stack.push(token);
                    }
                }
                _ => break,
            }
            tokens = &tokens[1..];
        }

        while let Some(op) = op_stack.pop() {
            let expr = if op.kind.op_info().unwrap().is_unary {
                let operand = output.pop().unwrap();
                Expr(
                    ExprKind::UnaryOp(UnaryOp(op.into(), Box::new(operand))),
                    RefCell::new(None),
                )
            } else {
                let rhs = output.pop().unwrap();
                let lhs = output.pop().unwrap();
                Expr(
                    ExprKind::BinaryOp(BinaryOp(Box::new(lhs), op.into(), Box::new(rhs))),
                    RefCell::new(None),
                )
            };

            output.push(expr);
        }

        if output.len() == 1 {
            Ok((output.pop().unwrap(), tokens))
        } else {
            Err(ParserError::InvalidExpression(cur_line, cur_col, cur_pos)) // TODO ERROR
        }
    }

    fn parse_type(tokens: &'a [Token]) -> ParserResult<(Type, &'a [Token])> {
        let parse_f_type = |tokens: &'a [Token]| {
            let (_, tokens) = TokenKind::Proc.parse(tokens)?;
            let (args, tokens) = delimited(
                TokenKind::OpenParen,
                seperated_list(Self::parse_type, TokenKind::Comma),
                TokenKind::CloseParen,
            )(tokens)?;

            let (_, tokens) = TokenKind::Arrow.parse(tokens)?;
            let (ret_ty, tokens) = Self::parse_type(tokens)?;

            Ok((Type::Function(args, Box::new(ret_ty)), tokens))
        };

        let (ty, tokens) = alt((
            map(TokenKind::Int, |_| Type::Int),
            map(TokenKind::Bool, |_| Type::Boolean),
            map(TokenKind::Float, |_| Type::Float),
            map(TokenKind::Str, |_| Type::String),
            map(preceded(TokenKind::Asterisk, Self::parse_type), |ty| {
                Type::Pointer(Box::new(ty))
            }),
            parse_f_type,
        ))(tokens)?;

        Ok((ty, tokens))
    }

    fn recover(&mut self) {
        // Recover from an error by skipping tokens until a semicolon is found
        while !self.tokens.is_empty() {
            if self.tokens[0].kind == TokenKind::Semi {
                self.tokens = &self.tokens[1..];
                return;
            }
            self.tokens = &self.tokens[1..];
        }
    }
}

impl Iterator for Parser<'_> {
    type Item = ParserResult<Stmt>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.is_empty() {
            return None;
        }

        let (stmt, rest) = match Self::parse_stmt(self.tokens) {
            Ok((stmt, rest)) => (stmt, rest),
            Err(e) => {
                self.recover();
                return Some(Err(e));
            }
        };

        self.tokens = rest;
        Some(Ok(stmt))
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, Vec<ParserError>> {
    let parser = Parser::new(tokens);
    let mut stmts = Vec::new();
    let mut errors = Vec::new();

    for stmt in parser {
        match stmt {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => errors.push(e),
        }
    }
    if errors.is_empty() {
        Ok(Program(stmts))
    } else {
        Err(errors)
    }
}
