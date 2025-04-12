#![allow(non_snake_case)]
use crate::{
    errors::{parser::ParserError, Location},
    lexer::tokens::{Token, TokenKind},
};

macro_rules! impl_then_tuple {
  ($th:ident $ph:ident) => {
    impl<'a, $th, $ph> Parser<'a, ($th,)> for ($ph,)
    where
      $ph: Parser<'a, $th>
    {
      fn parse(&mut self, input: &'a [Token]) -> Result<(($th,), &'a [Token]), ParserError> {
        let ($ph,) = self;
        let (res, rest) = $ph.parse(input)?;
        Ok(((res,), rest))
      }
    }
  };

  ($th:ident $ph:ident, $($t:ident $p:ident),*) => {
    impl<'a, $th, $($t),*, $ph, $($p),*> Parser<'a, ($th, $($t),*)> for ($ph, $($p),*)
    where
      $ph: Parser<'a, $th>,
      $($p: Parser<'a, $t>),*
    {
      fn parse(&mut self, input: &'a [Token]) -> Result<(($th, $($t),*), &'a [Token]), ParserError> {
        let mut rest = input;
        let ($ph, $($p,)*) = self;
        let result = (
          {
            let (res, next_rest) = $ph.parse(rest)?;
            rest = next_rest;
            res
          },
          $(
            {
              let (res, next_rest) = $p.parse(rest)?;
              rest = next_rest;
              res
            }
          ),*
        );

        Ok((result, rest))
      }
    }

    impl_then_tuple!($($t $p),*);
  };
}

impl_then_tuple!(T1 P1, T2 P2, T3 P3, T4 P4, T5 P5, T6 P6, T7 P7, T8 P8, T9 P9, T10 P10, T11 P11, T12 P12);

pub trait Parser<'a, T> {
    fn parse(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), ParserError>;
    fn and_then<P>(
        &mut self,
        mut other: P,
    ) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), ParserError>
    where
        P: Parser<'a, T>,
    {
        move |input: &'a [Token]| {
            let (_, rest) = self.parse(input)?;
            other.parse(rest)
        }
    }
}

pub trait Choice<'a, T> {
    fn choice(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), ParserError>;
}

impl<'a> Parser<'a, Token> for TokenKind {
    fn parse(&mut self, input: &'a [Token]) -> Result<(Token, &'a [Token]), ParserError> {
        if let Some((first, rest)) = input.split_first() {
            if *self == first.kind {
                return Ok(((*first).clone(), rest));
            } else {
                return Err(ParserError::MissingToken(
                    Location::from(first),
                    self.clone(),
                ));
            }
        }
        Err(ParserError::Other("".to_string())) // TODO: Handle this case properly
    }
}

impl<'a, T, F> Parser<'a, T> for F
where
    F: FnMut(&'a [Token]) -> Result<(T, &'a [Token]), ParserError>,
{
    fn parse(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), ParserError> {
        self(input)
    }
}

// impl<'a, T1, T2, P1, P1> Parser<'a, (T1, T2)> for (P1, P2)
// where
//   P1: Parser<'a, T1>,
//   P2: Parser<'a, T2>,
// {
//   fn parse(&mut self, input: &'a [Token]) -> Result<(($th, $($t),*), &'a [Token]), String> {
//     let (first, second) = self;
//     let (first_res, rest) = first.parse(input)?;
//     let (second_res, rest) = second.parse(input)?;
//     Ok((first_res, second_res), rest)
//   }
// }

// impl<'a, T, P1, P2> Choice<'a, T> for (P1, P2)
// where
//   P1: Parser<'a, T>,
//   P2: Parser<'a, T>,
// {
//   fn choice(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), String> {
//     match self.0.parse(input) {
//       Err(_) => {
//         match self.1.parse(input) {
//           Err(_) => Err("Both parsers failed".to_string()),
//           res => res
//         }
//       },
//       res => res
//     }
//   }
// }

macro_rules! choice_trait_impl {
  ($ph:ident) => {
    impl<'a, T, $ph> Choice<'a, T> for ($ph,)
    where
      $ph: Parser<'a, T>
    {
      fn choice(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), ParserError> {
        self.0.parse(input)
      }
    }
  };

  ($ph:ident $($p:ident)*) => {
    impl<'a, T, $ph, $($p),*> Choice<'a, T> for ($ph, $($p),*)
    where
      $ph: Parser<'a, T>,
      $($p: Parser<'a, T>),+
    {
      fn choice(&mut self, input: &'a [Token]) -> Result<(T, &'a [Token]), ParserError> {
        match self.0.parse(input) {
          Err(e) => choice_trait_inner!(1, self, input, e, $ph $($p)+),
          res => res
        }
      }
    }

    choice_trait_impl!($($p)*);
  }
}

macro_rules! choice_trait_inner {
  ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => {
    match $self.$it.parse($input) {
      Err(e) => {
        let err = $err.or(e);
        successor!($it, choice_trait_inner!($self, $input, err, $($id)+))
      },
      res => res
    }
  };
  ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident) => {
    // Err("All parsers failed".to_string())
    // Err(ParserError::Other("All parsers failed".to_string()))
    Err($err)
  };
}

/*

macro_rules! alt_trait(
  ($first:ident $second:ident $($id: ident)+) => (
    alt_trait!(__impl $first $second; $($id)+);
  );
  (__impl $($current:ident)*; $head:ident $($id: ident)+) => (
    alt_trait_impl!($($current)*);

    alt_trait!(__impl $($current)* $head; $($id)+);
  );
  (__impl $($current:ident)*; $head:ident) => (
    alt_trait_impl!($($current)*);
    alt_trait_impl!($($current)* $head);
  );
);

macro_rules! alt_trait_impl(
  ($($id:ident)+) => (
    impl<
      Input: Clone, Output, Error: ParseError<Input>,
      $($id: Parser<Input, Output, Error>),+
    > Alt<Input, Output, Error> for ( $($id),+ ) {

      fn choice(&mut self, input: Input) -> IResult<Input, Output, Error> {
        match self.0.parse(input.clone()) {
          Err(Err::Error(e)) => alt_trait_inner!(1, self, input, e, $($id)+),
          res => res,
        }
      }
    }
  );
);

macro_rules! alt_trait_inner(
  ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident $($id:ident)+) => (
    match $self.$it.parse($input.clone()) {
      Err(Err::Error(e)) => {
        let err = $err.or(e);
        succ!($it, alt_trait_inner!($self, $input, err, $($id)+))
      }
      res => res,
    }
  );
  ($it:tt, $self:expr, $input:expr, $err:expr, $head:ident) => (
    Err(Err::Error(Error::append($input, ErrorKind::Alt, $err)))
  );
);


*/

macro_rules! successor {
  (0, $submac:ident!($($args:tt)*)) => ($submac!(1, $($args)*));
  (1, $submac:ident!($($args:tt)*)) => ($submac!(2, $($args)*));
  (2, $submac:ident!($($args:tt)*)) => ($submac!(3, $($args)*));
  (3, $submac:ident!($($args:tt)*)) => ($submac!(4, $($args)*));
  (4, $submac:ident!($($args:tt)*)) => ($submac!(5, $($args)*));
  (5, $submac:ident!($($args:tt)*)) => ($submac!(6, $($args)*));
  (6, $submac:ident!($($args:tt)*)) => ($submac!(7, $($args)*));
  (7, $submac:ident!($($args:tt)*)) => ($submac!(8, $($args)*));
  (8, $submac:ident!($($args:tt)*)) => ($submac!(9, $($args)*));
  (9, $submac:ident!($($args:tt)*)) => ($submac!(10, $($args)*));
  (10, $submac:ident!($($args:tt)*)) => ($submac!(11, $($args)*));
  (11, $submac:ident!($($args:tt)*)) => ($submac!(12, $($args)*));
  (12, $submac:ident!($($args:tt)*)) => ($submac!(13, $($args)*));
  (13, $submac:ident!($($args:tt)*)) => ($submac!(14, $($args)*));
  (14, $submac:ident!($($args:tt)*)) => ($submac!(15, $($args)*));
  (15, $submac:ident!($($args:tt)*)) => ($submac!(16, $($args)*));
}

choice_trait_impl!(P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13 P14 P15);

// pub fn tag<'a, T, L>(parser: &'a L) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), String>
// where
//   L: Parser<'a, T>
// {
//   move |input: &'a [Token]| parser.parse(input)
// }

pub fn alt<'a, T, L>(
    mut parsers: L,
) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), ParserError>
where
    L: Choice<'a, T>,
{
    move |input: &'a [Token]| parsers.choice(input)
}

pub fn opt<'a, T, P>(
    mut parser: P,
) -> impl FnMut(&'a [Token]) -> Result<(Option<T>, &'a [Token]), ParserError>
where
    P: Parser<'a, T>,
{
    move |input: &'a [Token]| match parser.parse(input) {
        Ok((res, rest)) => Ok((Some(res), rest)),
        Err(_) => Ok((None, input)),
    }
}

pub fn many<'a, T, P>(
    mut parser: P,
) -> impl FnMut(&'a [Token]) -> Result<(Vec<T>, &'a [Token]), ParserError>
where
    P: Parser<'a, T>,
{
    move |input: &'a [Token]| {
        let mut rest = input;
        let mut results = Vec::new();
        while let Ok((res, next_rest)) = parser.parse(rest) {
            results.push(res);
            rest = next_rest;
        }
        Ok((results, rest))
    }
}

pub fn delimited<'a, T, P1, P2, P3>(
    mut open: P1,
    mut parser: P2,
    mut close: P3,
) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), ParserError>
where
    P1: Parser<'a, Token>,
    P2: Parser<'a, T>,
    P3: Parser<'a, Token>,
{
    move |input: &'a [Token]| {
        let (_, rest) = open.parse(input)?;
        let (res, rest) = parser.parse(rest)?;
        let (_, rest) = close.parse(rest)?;
        Ok((res, rest))
    }
}

pub fn preceded<'a, T, U, P1, P2>(
    mut first: P1,
    mut second: P2,
) -> impl FnMut(&'a [Token]) -> Result<(U, &'a [Token]), ParserError>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    move |input: &'a [Token]| {
        let (_, rest) = first.parse(input)?;
        let (res, rest) = second.parse(rest)?;
        Ok((res, rest))
    }
}

pub fn terminated<'a, T, U, P1, P2>(
    mut first: P1,
    mut second: P2,
) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), ParserError>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    move |input: &'a [Token]| {
        let (res, rest) = first.parse(input)?;
        let (_, rest) = second.parse(rest)?;
        Ok((res, rest))
    }
}

pub fn map<'a, T, U, F, P>(
    mut parser: P,
    mut f: F,
) -> impl FnMut(&'a [Token]) -> Result<(U, &'a [Token]), ParserError>
where
    P: Parser<'a, T>,
    F: FnMut(T) -> U,
{
    move |input: &'a [Token]| {
        let (res, rest) = parser.parse(input)?;
        Ok((f(res), rest))
    }
}

pub fn seperated_list<'a, T, U, P1, P2>(
    mut first: P1,
    mut second: P2,
) -> impl FnMut(&'a [Token]) -> Result<(Vec<T>, &'a [Token]), ParserError>
where
    P1: Parser<'a, T>,
    P2: Parser<'a, U>,
{
    move |input: &'a [Token]| {
        let (first_res, mut rest) = first.parse(input)?;
        let mut results = vec![first_res];

        while let Ok((_, next_rest)) = second.parse(rest) {
            let (res, next_rest) = first.parse(next_rest)?;
            results.push(res);
            rest = next_rest;
        }

        Ok((results, rest))
    }
}
