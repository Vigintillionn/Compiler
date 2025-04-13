use std::cell::RefCell;

use crate::{
    errors::parser::{ParserError, ParserResult},
    lexer::tokens::TokenKind,
    parser::ast::{BinaryOp, ExprKind, LiteralValue, Type},
    Token,
};

use super::ast::{Expr, UnaryOp};

pub fn parse_expr(tokens: &[Token]) -> ParserResult<(Expr, &[Token])> {
    use TokenKind::*;
    let mut tokens = tokens;
    let mut op_stack: Vec<&Token> = Vec::new();
    let mut output: Vec<Expr> = Vec::new();
    let mut arg_stack: Vec<usize> = Vec::new();

    while let Some(token) = tokens.first() {
        match token.kind {
            IntLiteral(n) => output.push(Expr(
                ExprKind::Literal(LiteralValue::Integer(n)),
                RefCell::new(Some(Type::Int)),
            )), // TODO: Handle typing
            FloatLiteral(f) => output.push(Expr(
                ExprKind::Literal(LiteralValue::Float(f)),
                RefCell::new(Some(Type::Float)),
            )), // TODO: Handle typing
            BoolLiteral(b) => output.push(Expr(
                ExprKind::Literal(LiteralValue::Boolean(b)),
                RefCell::new(Some(Type::Boolean)),
            )),
            Identifier(ref id) => {
                if tokens.len() > 1 && tokens[1].kind == TokenKind::OpenParen {
                    // Function call
                    op_stack.push(token);
                    arg_stack.push(0);
                } else {
                    output.push(Expr(ExprKind::Ident(id.clone()), RefCell::new(None)));
                }
            }
            OpenParen => op_stack.push(token),
            CloseParen => {
                while !matches!(
                    op_stack.last(),
                    Some(&Token {
                        kind: OpenParen,
                        ..
                    })
                ) {
                    if let Some(op) = op_stack.pop() {
                        handle_operator(op, &mut output)?;
                    } else {
                        return Err(ParserError::MismatchedParantheses(0, 0, 0));
                    }
                }

                // Pop the open parenthesis
                if !matches!(
                    op_stack.last(),
                    Some(&Token {
                        kind: OpenParen,
                        ..
                    })
                ) {
                    return Err(ParserError::MismatchedParantheses(0, 0, 0));
                }
                op_stack.pop();

                if matches!(
                    op_stack.last(),
                    Some(&Token {
                        kind: Identifier(_),
                        ..
                    })
                ) {
                    let Some(&Token {
                        kind: Identifier(ref name),
                        ..
                    }) = op_stack.pop()
                    else {
                        return Err(ParserError::MismatchedParantheses(0, 0, 0));
                    };
                    let args = output.split_off(
                        output
                            .len()
                            .saturating_sub(arg_stack.pop().unwrap_or(0) + 1),
                    );
                    output.push(Expr(ExprKind::Call(name.clone(), args), RefCell::new(None)));
                }
            }
            Comma => {
                while !matches!(
                    op_stack.last(),
                    Some(&Token {
                        kind: OpenParen,
                        ..
                    })
                ) {
                    let top_op = op_stack.pop().unwrap();
                    handle_operator(top_op, &mut output)?;
                }
                arg_stack.last_mut().map(|args| {
                    *args += 1;
                });
            }
            _ if token.kind.op_info().is_some() => {
                let op_info = token.kind.op_info().unwrap();
                if op_info.is_unary {
                    op_stack.push(token);
                } else {
                    // Is an operator
                    while let Some(&top_op) = op_stack.last() {
                        if let OpenParen | Identifier(_) = top_op.kind {
                            break;
                        }
                        let top_info = top_op
                            .kind
                            .op_info()
                            .expect("Token at top of stack is not an operator");
                        if top_info.is_unary {
                            op_stack.pop();
                            handle_unary_operator(top_op, &mut output)?;
                        } else {
                            break;
                        }
                    }

                    // Next, apply any pending binary operators
                    while let Some(&top_op) = op_stack.last() {
                        // Stop if we reach a parenthesis
                        if let OpenParen | Identifier(_) = top_op.kind {
                            break;
                        }
                        let top_info = top_op
                            .kind
                            .op_info()
                            .expect("Token at top of stack is not an operator");
                        if top_info.prec >= op_info.prec {
                            op_stack.pop();
                            handle_binary_operator(top_op, &mut output)?;
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
        if matches!(op.kind, TokenKind::OpenParen) {
            return Err(ParserError::MismatchedParantheses(0, 0, 0));
        }
        handle_operator(op, &mut output)?;
    }

    if output.len() == 1 {
        Ok((output.pop().unwrap(), tokens))
    } else {
        Err(ParserError::InvalidExpression(0, 0, 0)) // TODO ERROR
    }
}

fn handle_operator(op: &Token, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if op.kind.op_info().unwrap().is_unary {
        handle_unary_operator(op, output)
    } else {
        handle_binary_operator(op, output)
    }
}

fn handle_unary_operator(op: &Token, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if !op.kind.op_info().unwrap().is_unary {
        return Err(ParserError::Other("Expected unary operator".to_string()));
    }
    let operand = output.pop().unwrap();
    let expr = Expr(
        ExprKind::UnaryOp(UnaryOp(op.into(), Box::new(operand))),
        RefCell::new(None),
    );
    output.push(expr);

    Ok(())
}

fn handle_binary_operator(op: &Token, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if op.kind.op_info().unwrap().is_unary {
        return Err(ParserError::Other("Expected binary operator".to_string()));
    }
    let rhs = output.pop().unwrap();
    let lhs = output.pop().unwrap();
    let expr = Expr(
        ExprKind::BinaryOp(BinaryOp(Box::new(lhs), op.into(), Box::new(rhs))),
        RefCell::new(None),
    );
    output.push(expr);

    Ok(())
}
