mod ast;
use lexer::{operator_precedence, operator_to_string, Token};
use ast::{Stmt, Expr};
use nom::{branch::alt, IResult, combinator::map};

pub fn parse(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, stmts) = parse_statement(input)?;
  Ok((input, stmts))
}

fn parse_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  alt((
    parse_let_statement,
    map(parse_expression, Stmt::Expr)
  ))(input)
}

fn parse_let_statement(input: &[Token]) -> IResult<&[Token], Stmt> {
  let (input, _) = tag(&Token::Let)(input)?;
  let (input, identifier) = parse_identifier(input)?;
  let (input, _) = tag(&Token::Assign)(input)?;
  let (input, expr) = parse_expression(input)?;
  let (input, _) = tag(&Token::Semicolon)(input)?;

  Ok((input, Stmt::Let(identifier, expr)))
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
      Token::Plus | Token::Minus | Token::Star | Token::Slash => {
        while let Some(top_op) = op_stack.last() {
          if operator_precedence(top_op) >= operator_precedence(token) {
            let rhs = output.pop().expect("Expected right-hand side of binary operation");
            let lhs = output.pop().expect("Expected left-hand side of binary operation");
            output.push(Expr::BinaryOp(Box::new(lhs), operator_to_string(top_op), Box::new(rhs)));
            op_stack.pop();
          } else {
            break;
          }
        }
        op_stack.push(token);
      }
      Token::Number(n) => output.push(Expr::Literal(*n)),
      Token::Identifier(name) => output.push(Expr::Variable(name.clone())),
      Token::Semicolon => break,
      _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
    input = input_next;
  }

  while let Some(op) = op_stack.pop() {
    let rhs = output.pop().expect("Expected right-hand side of binary operation");
    let lhs = output.pop().expect("Expected left-hand side of binary operation");
    output.push(Expr::BinaryOp(Box::new(lhs), operator_to_string(&op), Box::new(rhs)));
  }

  if output.len() == 1  {
    Ok((input, output.pop().expect("Expected expression")))
  } else {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

#[cfg(test)]
mod tests {
  // TODO: write tests
}
