use std::error;
use crate::parser::Parser;
use crate::lexer::tokens::Token;

pub type IResult<I, O, E> = Result<(I, O), E>;

pub trait Alt<I, O, E> {
  /// Tests each parser in the tuple and returns the result of the first one that succeeds
  fn choice(&mut self, input: I) -> IResult<I, O, E>;
}

pub fn alt<I: Clone, O, E, List: Alt<I, O, E>>(
  mut l: List,
) -> impl FnMut(I) -> IResult<I, O, E> {
  move |i: I| l.choice(i)
}

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

macro_rules! alt_trait_impl(
  ($($id:ident)+) => (
    impl<'a,
      $($id: Parser<'a, &'a [Token]>),+
    > Alt<Input, Output, Error> for ( $($id),+ ) {

      fn choice(&mut self, input: &'a [Token]) -> IResult<Input, Output, Error> {
        match self.0.parse(input.clone()) {
          Err(Err::Error(e)) => alt_trait_inner!(1, self, input, e, $($id)+),
          res => res,
        }
      }
    }
  );
);

alt_trait!(A B C);