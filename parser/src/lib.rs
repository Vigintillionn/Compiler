mod ast;
use lexer::Token;
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
  if let Some((token, remaining)) = input.split_first() {
    match token {
      Token::Number(n) => Ok((remaining, Expr::Literal(*n))),
      Token::Identifier(name) => Ok((remaining, Expr::Variable(name.clone()))),
      _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
  } else {
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
  }
}

#[cfg(test)]
mod tests {
  // TODO: write tests
}
