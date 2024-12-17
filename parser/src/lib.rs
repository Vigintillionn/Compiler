pub mod ast;
pub mod types;

use ast::{Expr, Literal, Stmt};
use types::Type;
use lexer::{operator_precedence, operator_to_string, Token};
use nom::{branch::alt, combinator::{map, opt}, multi::{many0, separated_list0}, sequence::{delimited, preceded, terminated, tuple}, IResult};

pub fn parse(input: &[Token]) -> Result<Stmt, String> {
  let mut input = input;
  let mut statements = Vec::new();

  while let Ok((remaining_input, stmt)) = parse_statement(input) {
    statements.push(stmt);
    input = remaining_input
  }
  
  if !input.is_empty() {
    Err(format!("Unexpected tokens remaining: {:?}\nManaged to parse: {:?}", input, statements))
  } else {
    Ok(Stmt::Program(statements))
  }
}

fn parse_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  alt((
    parse_let_statement,
    parse_while_statement,
    parse_for_statement,
    parse_if_statement,
    parse_block,
    terminated(parse_assign_stmt, tag(&Token::Semicolon)),
    map(delimited(tag(&Token::LBrace), many0(parse_statement), tag(&Token::RBrace)), Stmt::Block),
    // map(terminated(parse_expression, tag(&Token::Semicolon)), Stmt::Expr)
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

fn parse_for_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, _) = tag(&Token::For)(input)?;
  let (input, _) = tag(&Token::LParen)(input)?;
  let (mut input, initializer) = opt(
    alt((
      parse_let_statement,
      terminated(parse_assign_stmt, tag(&Token::Semicolon))
    )))(input)?;

  if initializer.is_none() {
    let (remaining, _) = tag(&Token::Semicolon)(input)?;
    input = remaining;
  }
  
  let (input, condition) = opt(parse_expression)(input)?;
  let (input, _) = tag(&Token::Semicolon)(input)?;
  let (input, updater) = terminated(opt(parse_assign_stmt), tag(&Token::RParen))(input)?; //map(parse_expression, Stmt::Expr)(input)?;
  let (input, body) = parse_block(input)?;

  Ok((input, Stmt::For(initializer.map(Box::new), condition, updater.map(Box::new), Box::new(body))))
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
  let (input, stmts) = delimited(
    tag(&Token::LBrace), 
    many0(parse_statement), 
    tag(&Token::RBrace)
  )(input)?;

  Ok((input, Stmt::Block(stmts))) 
}

fn parse_identifier(input: &[Token]) -> IResult<&[Token], String> {
  if let Some((Token::Identifier(name), remaining)) = input.split_first() {
    Ok((remaining, name.clone()))
  } else {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

fn parse_assign_stmt(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, identifier) = parse_identifier(input)?;
  let (input, _) = tag(&Token::Assign)(input)?;
  let (input, expr) = parse_expression(input)?;

  Ok((input, Stmt::Expr(Expr::Assign(identifier, Box::new(expr), None))))
}

fn parse_expression(input: &[Token]) -> IResult<&[Token], Expr> {
  let mut input = input;
  let mut output = Vec::new();
  let mut op_stack: Vec<&Token> = Vec::new();
  
  // TODO: Add support for unary operators, parentheses, and function calls
  while let Some(token) = input.first() {
    match token {
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
      Token::Identifier(name) => output.push(Expr::Variable(name.clone(), Some(Type::Int))),
      _ => break
    }
    input = &input[1..];
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
