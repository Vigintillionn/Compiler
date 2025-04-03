use std::cell::{Ref, RefCell};
use crate::Token;
use framework::{alt, delimited, many, map, opt, preceded, seperated_list, terminated, Choice, Parser as ParserTrait};
use ast::{BinaryOp, Expr, ExprKind, LiteralValue, Program, Stmt, Type, UnaryOp};

pub mod ast;
mod framework;

pub struct Parser<'a> {
  tokens: &'a [Token],
}

impl<'a> Parser<'a> {
  pub fn new(tokens: &'a [Token]) -> Self {
    Self { tokens }
  }

  fn parse_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
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
      map(terminated(Token::Break, Token::Semi), |_| Stmt::Break),
      map(terminated(Token::Continue, Token::Semi), |_| Stmt::Continue),
      map(terminated(Self::parse_expr, Token::Semi), |expr| Stmt::Expr(expr)),
    ))(tokens)?;

    Ok((stmt, tokens))
  }

  fn parse_var_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::Var.parse(tokens)?;
    let (name, tokens) = Self::parse_ident(tokens)?;

    let (colon, tokens) = opt(Token::Colon).parse(tokens)?;
    let (ty, tokens) = if let Some(_) = colon {
      Self::parse_type(tokens)
        .map(|(ty, tokens)| (Some(ty), tokens))?
    } else {
      (None, tokens)
    };

    let (_, tokens) = Token::Assign.parse(tokens)?;
    let (expr, tokens) = Self::parse_expr(tokens)?;
    let (_, tokens) = Token::Semi.parse(tokens)?;

    Ok((Stmt::Var(name, expr, RefCell::new(ty)), tokens))
  }

  fn parse_assign_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (name, tokens) = Self::parse_ident(tokens)?;
    let (_, tokens) = Token::Assign.parse(tokens)?;
    let (expr, tokens) = Self::parse_expr(tokens)?;
    let (_, tokens) = Token::Semi.parse(tokens)?;

    Ok((Stmt::Assign(name, expr), tokens))
  }

  fn parse_if_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::If.parse(tokens)?;
    let (cond, tokens) = delimited(Token::OpenParen, Self::parse_expr, Token::CloseParen)(tokens)?;
    let (then_block, tokens) = Self::parse_block(tokens)?;
    let (has_else, tokens) = opt(Token::Else)(tokens)?;
    let (else_block, tokens) = if let Some(_) = has_else {
      Self::parse_block(tokens)
        .map(|(else_block, tokens)| (Some(else_block), tokens))?
    } else {
      (None, tokens)
    };

    Ok((Stmt::If(cond, Box::new(then_block), else_block.map(Box::new)), tokens))
  }

  fn parse_loop_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::Loop.parse(tokens)?;
    let (block, tokens) = Self::parse_block(tokens)?;

    Ok((Stmt::Loop(None, None, None, Box::new(block)), tokens))
  }

  fn parse_while_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::While.parse(tokens)?;
    let (cond, tokens) = delimited(Token::OpenParen, Self::parse_expr, Token::CloseParen)(tokens)?;
    let (block, tokens) = Self::parse_block(tokens)?;

    Ok((Stmt::Loop(None, Some(cond), None, Box::new(block)), tokens))
  }

  fn parse_for_loop(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::For.parse(tokens)?;
    let (_, tokens) = Token::OpenParen.parse(tokens)?;
    let (init, tokens) = opt(alt((
      Self::parse_var_stmt,
      Self::parse_assign_stmt,
    )))(tokens)?;

    let (_, tokens) = if init.is_none() {
      Token::Semi.parse(tokens)?
    } else {
      (Token::Semi, tokens)
    };

    let (cond, tokens) = opt(Self::parse_expr)(tokens)?;
    let (_, tokens) = Token::Semi.parse(tokens)?;

    let (name, tokens) = opt(Self::parse_ident)(tokens)?;
    let (update, tokens) = if name.is_some() {
      let (_, tokens) = Token::Assign.parse(tokens)?;
      let (update, tokens) = Self::parse_expr(tokens)?;

      (Some(Stmt::Assign(name.expect("Name is not some"), update)), tokens)
    } else {
      (None, tokens)
    };

    let (_, tokens) = Token::CloseParen.parse(tokens)?;
    let (block, tokens) = Self::parse_block(tokens)?;

    Ok((Stmt::Loop(init.map(Box::new), cond, update.map(Box::new), Box::new(block)), tokens))
  }

  fn parse_ret_stmt(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (_, tokens) = Token::Ret.parse(tokens)?;
    let (expr, tokens) = opt(Self::parse_expr)(tokens)?;
    let (_, tokens) = Token::Semi.parse(tokens)?;

    Ok((Stmt::Ret(expr), tokens))
  }

  fn parse_func_decl(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let parse_arg = |tokens: &'a [Token]| {
      let (name, tokens) = Self::parse_ident(tokens)?;
      let (_, tokens) = Token::Colon.parse(tokens)?;
      let (ty, tokens) = Self::parse_type(tokens)?;

      Ok(((name, ty), tokens))
    };

    let (_, tokens) = Token::Proc.parse(tokens)?;
    let (name, tokens) = Self::parse_ident(tokens)?;
    let (_, tokens) = Token::OpenParen.parse(tokens)?;

    let (args, tokens) = opt(seperated_list(parse_arg, Token::Comma))(tokens)?;

    let (_, tokens) = Token::CloseParen.parse(tokens)?;
    let (type_dec, tokens) = opt(Token::Arrow)(tokens)?;

    let (ret_ty, tokens) = if type_dec.is_some() {
      Self::parse_type(tokens)?
    } else {
      (Type::Void, tokens)
    };

    let (block, tokens) = Self::parse_block(tokens)?;

    Ok((Stmt::Function(name, args.unwrap_or(vec![]), ret_ty, Box::new(block)), tokens))
  }

  fn parse_ident(tokens: &'a [Token]) -> Result<(String, &'a [Token]), String> {
    if let Some((Token::Identifier(name), remaining)) = tokens.split_first() {
      Ok((name.clone(), remaining))
    } else {
      Err("Expected identifier".to_string())
    }
  }

  fn parse_block(tokens: &'a [Token]) -> Result<(Stmt, &'a [Token]), String> {
    let (stmts, tokens) = delimited(
      Token::OpenBrace, 
      many(Self::parse_stmt), 
      Token::CloseBrace
    )(tokens)?;

    Ok((Stmt::Block(stmts), tokens))
  }

  fn parse_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), String> {
    let mut tokens = tokens;
    let mut output: Vec<Expr> = Vec::new();
    let mut op_stack: Vec<&Token> = Vec::new();
  
    while let Some(token) = tokens.first() {
      match token {
        Token::Identifier(name) => {
          if tokens.len() > 1 && tokens[1] == Token::OpenParen {
            tokens = &tokens[1..];
            // Parse function call arguments
            let (args, rest) = delimited(Token::OpenParen, seperated_list(Self::parse_expr, Token::Comma), Token::CloseParen)(tokens)?;
            output.push(Expr(ExprKind::Call(name.clone(), args), RefCell::new(None)));
            tokens = rest;
            continue;
          } else {
            output.push(Expr(ExprKind::Ident(name.clone()), RefCell::new(None)))
          }
        },
        Token::IntLiteral(n) => output.push(Expr(ExprKind::Literal(LiteralValue::Integer(*n)),RefCell::new(Some(Type::Int)))), // TODO: Handle typing
        Token::FloatLiteral(f) => output.push(Expr(ExprKind::Literal(LiteralValue::Float(*f)), RefCell::new(Some(Type::Float)))), // TODO: Handle typing
        Token::BoolLiteral(b) => output.push(Expr(ExprKind::Literal(LiteralValue::Boolean(*b)), RefCell::new(Some(Type::Boolean)))), // TODO: Handle typing
        _ if token.op_info().is_some() => {
          let op_info = token.op_info().expect("Token op_info is None"); // token.op_info() is not None
          if op_info.is_unary {
            op_stack.push(token);
          } else {
            // First, apply any pending unary operators
            while let Some(&top_op) = op_stack.last() {
              let top_info = top_op.op_info().expect("Token at top of stack is not an operator");
              if top_info.is_unary {
                op_stack.pop();
                let operand = output.pop().expect("No operand for unary operator");
                output.push(Expr(ExprKind::UnaryOp(UnaryOp(top_op.into(), Box::new(operand))), RefCell::new(None)));
              } else {
                break;
              }
            }

            // Next, apply any pending binary operators
            while let Some(&top_op) = op_stack.last() {
              let top_info = top_op.op_info().expect("Token at top of stack is not an operator");
              if top_info.prec >= op_info.prec {
                let rhs = output.pop().unwrap();
                let lhs = output.pop().unwrap(); // TODO: handle unwrap
                output.push(Expr(ExprKind::BinaryOp(BinaryOp(Box::new(lhs), top_op.into(), Box::new(rhs))), RefCell::new(None)));
                op_stack.push(token);
              } else {
                break;
              }
            }
            op_stack.push(token);
          }
        },
        _ => break
      }
      tokens = &tokens[1..];
    }
  
    while let Some(op) = op_stack.pop() {
      let expr = if op.op_info().unwrap().is_unary {
        let operand = output.pop().unwrap();
        Expr(ExprKind::UnaryOp(UnaryOp(op.into(), Box::new(operand))), RefCell::new(None))
      } else {
        let rhs = output.pop().unwrap();
        let lhs = output.pop().unwrap();
        Expr(ExprKind::BinaryOp(BinaryOp(Box::new(lhs), op.into(), Box::new(rhs))), RefCell::new(None))
      };

      output.push(expr);
    }
  
    if output.len() == 1 {
      Ok((output.pop().unwrap(), tokens))
    } else {
      Err(format!("bla")) // TODO ERROR
    }
  }

  fn parse_type(tokens: &'a [Token]) -> Result<(Type, &'a [Token]), String> {
    let parse_f_type = |tokens: &'a [Token]| {
      let (_, tokens) = Token::Proc.parse(tokens)?;
      let (args, tokens) = delimited(
        Token::OpenParen,
        seperated_list(
          Self::parse_type, 
          Token::Comma
        ),
      Token::CloseParen
      )(tokens)?;

      let (_, tokens) = Token::Arrow.parse(tokens)?;
      let (ret_ty, tokens) = Self::parse_type(tokens)?;

      Ok((Type::Function(args, Box::new(ret_ty)), tokens))
    };

    let (ty, tokens) = alt((
      map(Token::Int, |_| Type::Int),
      map(Token::Bool, |_| Type::Boolean),
      map(Token::Float, |_| Type::Float),
      map(Token::Str, |_| Type::String),
      map(
        preceded(
          Token::Asterisk, 
          Self::parse_type
        ),
        |ty| Type::Pointer(Box::new(ty))
      ),
      parse_f_type,
    ))(tokens)?;

    Ok((ty, tokens))
  }
}

impl Iterator for Parser<'_> {
  type Item = Result<Stmt, String>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.tokens.is_empty() {
      return None;
    }

    let (stmt, rest) = Self::parse_stmt(self.tokens).ok()?;
    self.tokens = rest;
    Some(Ok(stmt))
  }
}

pub fn parse(tokens: &[Token]) -> Result<Program, String> {
  let stmts = Parser::new(tokens)
    .collect::<Result<Vec<Stmt>, String>>()?;
  Ok(Program(stmts))
}