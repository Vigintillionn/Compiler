#![allow(non_snake_case)]
use crate::lexer::tokens::Token;

// macro_rules! impl_then_tuple {
//   ($ih:ident $oh:ident $ph:ident) => {
//     impl<'a, $th, $oh, $ph> Parser<'a, ($oh,)> for ($ph,)
//     where
//       $ph: Parser<'a, $ih, $oh>
//     {
//       fn parse(&mut self, input: $ih) -> Result<(($oh,), &'a [Token]), String> {
//         let ($ph,) = self;
//         let (res, rest) = $ph.parse(input)?;
//         Ok(((res,), rest))
//       }
//     }
//   };

//   ($ih:ident $oh:ident $ph:ident, $($i:ident $o:ident $p:ident),*) => {
//     impl<'a, $ih, $($i),*, $oh, $($o),*, $ph, $($p),*> Parser<'a,  ($oh, $($o),*)> for ($ph, $($p),*)
//     where
//       $ph: Parser<'a, $ih, $oh>,
//       $($p: Parser<'a, $i, $o>),*
//     {
//       fn parse(&mut self, input: &'a [Token]) -> Result<(($oh, $($o),*), &'a [Token]), String> {
//         let mut rest = input;
//         let ($ph, $($p,)*) = self;
//         let result = (
//           {
//             let (res, next_rest) = $ph.parse(rest)?;
//             rest = next_rest;
//             res
//           },
//           $(
//             {
//               let (res, next_rest) = $p.parse(rest)?;
//               rest = next_rest;
//               res
//             }
//           ),*
//         );

//         Ok((result, rest))
//       }
//     }

//     impl_then_tuple!($($t $p),*);
//   };
// }

// impl_then_tuple!(I1 O1 P1, I2 O2 P2);

// impl_then_tuple!(T1 P1, T2 P2, T3 P3, T4 P4, T5 P5, T6 P6, T7 P7, T8 P8, T9 P9, T10 P10, T11 P11, T12 P12);

pub trait Parser<'a, I, O> {
  fn parse(&mut self, input: I) -> Result<(O, I), String>;
}

pub trait Choice<'a, I, O> {
  fn choice(&mut self, input: I) -> Result<(O, I), String>;
}

impl<'a> Parser<'a, &'a[Token], Token> for Token {
  fn parse(&mut self, input: &'a[Token]) -> Result<(Token, &'a[Token]), String> {
    if let Some((first, rest)) = input.split_first() {
      if self == first {
        return Ok((*first, rest))
      } 
    }
    Err(format!("Expected {:?} but got {:?}", self, input))
  }
}

impl<'a, I, O, F> Parser<'a, I, O> for F
where
  F: FnMut(I) -> Result<(O, I), String>,
{
  fn parse(&mut self, input: I) -> Result<(O, I), String> {
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
    impl<'a, I, O, $ph> Choice<'a, I, O> for ($ph,)
    where
      $ph: Parser<'a, I, O>
    {
      fn choice(&mut self, input: I) -> Result<(O, I), String> {
        self.0.parse(input)
      }
    }
  };

  ($ph:ident $($p:ident)*) => {
    impl<'a, I: Clone, O, $ph, $($p),*> Choice<'a, I, O> for ($ph, $($p),*)
    where
      $ph: Parser<'a, I, O>,
      $($p: Parser<'a, I, O>),+
    {
      fn choice(&mut self, input: I) -> Result<(O, I), String> {
        match self.0.parse(input.clone()) {
          Err(_) => choice_trait_inner!(1, self, input.clone(), $ph $($p)+),
          res => res
        }
      }
    }

    choice_trait_impl!($($p)*);
  }
}

macro_rules! choice_trait_inner {
  ($it:tt, $self:expr, $input:expr, $head:ident $($id:ident)+) => {
    match $self.$it.parse($input) {
      Err(_) => successor!($it, choice_trait_inner!($self, $input, $($id)+)),
      res => res
    }
  };
  ($it:tt, $self:expr, $input:expr, $head:ident) => {
    Err("All parsers failed".to_string())
  };
}

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
}

choice_trait_impl!(P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12);


pub fn alt<'a, I, O, L>(mut parsers: L) -> impl FnMut(I) -> Result<(O, I), String>
where
  L: Choice<'a, I, O>
{
  move |input: I| parsers.choice(input)
}

pub fn opt<'a, I: Clone, O, P>(mut parser: P) -> impl FnMut(I) -> Result<(Option<O>, I), String>
where
  P: Parser<'a, I, O>
{
  move |input: I| {
    match parser.parse(input.clone()) {
      Ok((res, rest)) => Ok((Some(res), rest)),
      Err(_) => Ok((None, input))
    }
  }
}

// pub fn many<'a, T, P>(mut parser: P) -> impl FnMut(&'a [Token]) -> Result<(Vec<T>, &'a [Token]), String>
// where
//   P: Parser<'a, I, O>
// {
//   move |input: &'a [Token]| {
//     let mut rest = input;
//     let mut results = Vec::new();
//     while let Ok((res, next_rest)) = parser.parse(rest) {
//       results.push(res);
//       rest = next_rest;
//     }
//     Ok((results, rest))
//   }
// }

// pub fn delimited<'a, T, P1, P2, P3>(mut open: P1, mut parser: P2, mut close: P3) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), String>
// where
//   P1: Parser<'a, Token>,
//   P2: Parser<'a, T>,
//   P3: Parser<'a, Token>
// {
//   move |input: &'a [Token]| {
//     let (_, rest) = open.parse(input)?;
//     let (res, rest) = parser.parse(rest)?;
//     let (_, rest) = close.parse(rest)?;
//     Ok((res, rest))
//   }
// }

// pub fn preceded<'a, T, U, P1, P2>(mut first: P1, mut second: P2) -> impl FnMut(&'a [Token]) -> Result<(U, &'a [Token]), String>
// where
//   P1: Parser<'a, T>,
//   P2: Parser<'a, U>
// {
//   move |input: &'a [Token]| {
//     let (_, rest) = first.parse(input)?;
//     let (res, rest) = second.parse(rest)?;
//     Ok((res, rest))
//   }
// }

// pub fn terminated<'a, T, U, P1, P2>(mut first: P1, mut second: P2) -> impl FnMut(&'a [Token]) -> Result<(T, &'a [Token]), String>
// where
//   P1: Parser<'a, T>,
//   P2: Parser<'a, U>
// {
//   move |input: &'a [Token]| {
//     let (res, rest) = first.parse(input)?;
//     let (_, rest) = second.parse(rest)?;
//     Ok((res, rest))
//   }
// }

// pub fn map<'a, T, U, F, P>(mut parser: P, mut f: F) -> impl FnMut(&'a [Token]) -> Result<(U, &'a [Token]), String>
// where
//   P: Parser<'a, T>,
//   F: FnMut(T) -> U
// {
//   move |input: &'a [Token]| {
//     let (res, rest) = parser.parse(input)?;
//     Ok((f(res), rest))
//   }
// }

