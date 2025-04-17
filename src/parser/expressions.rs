use std::cell::RefCell;

use crate::{
    errors::parser::{ParserError, ParserResult},
    lexer::tokens::TokenKind,
    parser::ast::{disambiguate, BinaryOp, DisambiguatedOp, ExprKind, LiteralValue, Type},
    Token,
};

use super::ast::{Expr, UnaryOp};

pub fn parse_expr(tokens: &[Token]) -> ParserResult<(Expr, &[Token])> {
    use TokenKind::*;
    let mut tokens = tokens;
    let mut op_stack: Vec<DisambiguatedOp> = Vec::new();
    let mut output: Vec<Expr> = Vec::new();
    let mut arg_stack: Vec<usize> = Vec::new();
    let mut prev_token = None;

    let first_tok = tokens
        .first()
        .ok_or(ParserError::Other("Empty expression".to_string()))?;

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
                    op_stack.push(token.into());
                    arg_stack.push(0);
                } else {
                    output.push(Expr(ExprKind::Ident(id.clone()), RefCell::new(None)));
                }
            }
            OpenParen => op_stack.push(token.into()),
            CloseParen => {
                while !matches!(
                    op_stack.last(),
                    Some(&DisambiguatedOp {
                        token: Token {
                            kind: OpenParen,
                            ..
                        },
                        ..
                    })
                ) {
                    if let Some(ref op) = op_stack.pop() {
                        handle_operator(op, &mut output)?;
                    } else {
                        return Err(ParserError::MismatchedParantheses(token.clone()));
                    }
                }

                // Pop the open parenthesis
                if !matches!(
                    op_stack.last(),
                    Some(&DisambiguatedOp {
                        token: Token {
                            kind: OpenParen,
                            ..
                        },
                        ..
                    })
                ) {
                    return Err(ParserError::MismatchedParantheses(
                        op_stack.last().unwrap().token.clone(),
                    ));
                }
                op_stack.pop();

                if matches!(
                    op_stack.last(),
                    Some(&DisambiguatedOp {
                        token: Token {
                            kind: Identifier(_),
                            ..
                        },
                        ..
                    })
                ) {
                    let Some(DisambiguatedOp {
                        token:
                            Token {
                                kind: Identifier(ref name),
                                ..
                            },
                        ..
                    }) = op_stack.pop()
                    else {
                        return Err(ParserError::InvalidExpression(token.clone()));
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
                    Some(&DisambiguatedOp {
                        token: Token {
                            kind: OpenParen,
                            ..
                        },
                        ..
                    })
                ) {
                    let top_op = op_stack.pop().unwrap();
                    handle_operator(&top_op, &mut output)?;
                }
                arg_stack.last_mut().map(|args| {
                    *args += 1;
                });
            }
            _ if token.kind.op_info().is_some() => {
                let disamb_op = disambiguate(token, prev_token);

                if disamb_op.is_unary {
                    op_stack.push(disamb_op);
                } else {
                    // Is an operator
                    while let Some(top_op) = op_stack.last().cloned() {
                        if let OpenParen | Identifier(_) = top_op.token.kind {
                            break;
                        }
                        if top_op.is_unary {
                            op_stack.pop();
                            handle_unary_operator(&top_op, &mut output).unwrap();
                        } else {
                            break;
                        }
                    }

                    // Next, apply any pending binary operators
                    while let Some(top_op) = op_stack.last().cloned() {
                        // Stop if we reach a parenthesis
                        if let OpenParen | Identifier(_) = top_op.token.kind {
                            break;
                        }
                        if top_op.prec >= disamb_op.prec {
                            op_stack.pop();
                            handle_binary_operator(&top_op, &mut output).unwrap();
                        } else {
                            break;
                        }
                    }

                    op_stack.push(disamb_op);
                }
            }
            _ => break,
        }
        prev_token = Some(token.kind.clone());
        tokens = &tokens[1..];
    }

    while let Some(op) = op_stack.pop() {
        if matches!(op.token.kind, TokenKind::OpenParen) {
            return Err(ParserError::MismatchedParantheses(op.token.clone()));
        }

        handle_operator(&op, &mut output)?;
    }

    if output.len() == 1 {
        Ok((output.pop().unwrap(), tokens))
    } else {
        Err(ParserError::InvalidExpression(first_tok.clone())) // TODO ERROR
    }
}

fn handle_operator(op: &DisambiguatedOp, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if op.is_unary {
        handle_unary_operator(op, output)
    } else {
        handle_binary_operator(op, output)
    }
}

fn handle_unary_operator(op: &DisambiguatedOp, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if !op.is_unary {
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

fn handle_binary_operator(op: &DisambiguatedOp, output: &mut Vec<Expr>) -> Result<(), ParserError> {
    if op.is_unary {
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
