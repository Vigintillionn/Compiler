pub mod ast;
pub mod types;

use ast::{Expr, Literal, Stmt};
use types::Type;
use lexer::{operator_precedence, operator_to_string, Token};
use nom::{branch::alt, combinator::{map, opt}, multi::{many0, separated_list0}, sequence::{delimited, preceded, tuple}, IResult};

pub fn parse(input: &[Token]) -> Result<Stmt, String> {
  let mut input = input;
  let mut statements = Vec::new();

  while let Ok((remaining_input, stmt)) = parse_statement(input) {
    statements.push(stmt);
    input = remaining_input
  }
  
  if !input.is_empty() {
    Err(format!("Unexpected tokens remaining: {:?}", input))
  } else {
    Ok(Stmt::Program(statements))
  }
}

fn parse_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  alt((
    parse_let_statement,
    parse_while_statement,
    parse_if_statement,
    // parse_block,
    map(delimited(tag(&Token::LBrace), many0(parse_statement), tag(&Token::RBrace)), Stmt::Block),
    map(parse_expression, Stmt::Expr)
  ))(input)
}

fn parse_let_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, _) = tag(&Token::Let)(input)?;
  let (input, identifier) = parse_identifier(input)?;
  let (input, ty) = opt(|input| {
    let (input, _) = tag(&Token::Colon)(input)?;
    parse_type(input)
  })(input)?;
  let (input, _) = tag(&Token::Assign)(input)?;
  let (input, expr) = parse_expression(input)?;
  let (input, _) = tag(&Token::Semicolon)(input)?;

  Ok((input, Stmt::Let(identifier, expr, ty)))
}

fn parse_if_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, _) = tag(&Token::If)(input)?;
  let (input, cond) = parse_expression(input)?;
  let (input, then) = parse_block(input)?;
  let (input, else_do) = opt(preceded(tag(&Token::Else), parse_block))(input)?;

  Ok((input, Stmt::If(cond, Box::new(then), else_do.map(Box::new))))
}

fn parse_while_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, _) = tag(&Token::While)(input)?;
  let (input, condition) = parse_expression(input)?;
  let (input, body) = parse_block(input)?;

  Ok((input, Stmt::While(condition, Box::new(body))))
}

fn parse_type(input: &[Token]) -> IResult<&[Token], Type> {
  alt((
    map(tag(&Token::Uint), |_| Type::Uint),
    map(tag(&Token::Int), |_| Type::Int),
    map(tag(&Token::StringType), |_| Type::String),
    map(tag(&Token::Bool), |_| Type::Bool),

    // pointer: *int
    map(
      preceded(tag(&Token::Star), parse_type),
      |pointee| Type::Pointer(Box::new(pointee))
    ),

    // fn(args) -> return_type
    map(
      preceded(
        tag(&Token::Function),
        tuple((
          delimited(tag(&Token::LParen), separated_list0(tag(&Token::Comma), parse_type), tag(&Token::RParen)),
          tag(&Token::Arrow),
          parse_type,
      ))),
      |(param_types, _, return_type)| Type::FSignature(param_types, Box::new(return_type)),
    ),
  ))(input)
}

fn parse_block(input: &[Token]) -> IResult<&[Token], Stmt> {
  // let (input, _) = tag(&Token::LBrace)(input)?;
  // let (input, stmts) = many0(parse_statement)(input)?;
  // let (input, _) = tag(&Token::RBrace)(input)?;
  let (input, stmts) = delimited(tag(&Token::LBrace), many0(parse_statement), tag(&Token::RBrace))(input)?;

  Ok((input, Stmt::Block(stmts))) 
}

fn parse_identifier(input: &[Token]) -> IResult<&[Token], String> {
  if let Some((Token::Identifier(name), remaining)) = input.split_first() {
    Ok((remaining, name.clone()))
  } else {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

fn parse_expression(input: &[Token]) -> IResult<&[Token], Expr> {
  let mut input = input;
  let mut output = Vec::new();
  let mut op_stack: Vec<&Token> = Vec::new();
  
  // TODO: Add support for unary operators, parentheses, and function calls
  while let Some((token, input_next)) = input.split_first() {
    match token {
      Token::Assign => {
        if let Some(Expr::Variable(var_name, _)) = output.pop() {
          let (remaining_input, rhs) = parse_expression(input_next)?;
          return Ok((remaining_input, Expr::Assign(var_name, Box::new(rhs), Some(Type::Int))));
        } else {
          return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
        }
      },
      Token::Plus | Token::Minus | Token::Star | Token::Slash |
      Token::Equals | Token::NEquals | Token::GTEqual | Token::LTEqual | Token::LArrow | Token::RArrow => {
        while let Some(top_op) = op_stack.last() {
          if operator_precedence(top_op) >= operator_precedence(token) {
            let rhs = output.pop().expect("Expected right-hand side of binary operation");
            let lhs = output.pop().expect("Expected left-hand side of binary operation");
            output.push(Expr::BinaryOp(Box::new(lhs), operator_to_string(top_op), Box::new(rhs), Some(Type::Int)));
            op_stack.pop();
          } else {
            break;
          }
        }
        op_stack.push(token);
      }
      Token::Number(n) => output.push(Expr::Literal(Literal::Integer(*n), Some(Type::Int))),
      Token::True => output.push(Expr::Literal(Literal::Boolean(true), Some(Type::Bool))),
      Token::False => output.push(Expr::Literal(Literal::Boolean(false), Some(Type::Bool))),
      Token::Identifier(name) => output.push(Expr::Variable(name.clone(), Some(Type::Int))), // Type inference happens later
      Token::Semicolon | Token::LBrace => break,
      _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
    input = input_next;
  }

  while let Some(op) = op_stack.pop() {
    let rhs = output.pop().expect("Expected right-hand side of binary operation");
    let lhs = output.pop().expect("Expected left-hand side of binary operation");
    output.push(Expr::BinaryOp(Box::new(lhs), operator_to_string(&op), Box::new(rhs), Some(Type::Int)));
  }
  
  if output.len() == 1  {
    Ok((input, output.pop().expect("Expected expression")))
  } else {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

fn tag<'a>(expected: &'a Token) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token> {
  move |input: &'a [Token]| {
    if let Some((first, remaining)) = input.split_first() {
      if first == expected {
        return Ok((remaining, first));
      }
    }
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

#[cfg(test)]
mod tests {
  // TODO: write tests
}
