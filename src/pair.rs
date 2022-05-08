use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;

use crate::either::Either;
use crate::parser::Parser;
use crate::{Parse, ParseResult};


#[derive(Clone)]
pub struct Pair<'a, Input, T1, T2, Error1, Error2>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<RefCell<dyn Parse<'a, Input, (T1, T2), Either<Error1, Error2>> + 'a>>,
}

impl<'a, Input, T1, T2, Error1, Error2> Pair<'a, Input, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
{
    pub fn new<P1, P2>(mut parser1: P1, mut parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1, Error1> + 'a,
        P2: Parse<'a, Input, T2, Error2> + 'a,
    {
        Self {
            parser: Rc::new(
                RefCell::new(
                move |input| {
                let (result1, input2) = match parser1.parse(input) {
                    Ok((result1, input2)) => (result1, input2),
                    Err(error) => return Err(Either::Left(error)),
                };
                let (result2, rest) = match parser2.parse(input2) {
                    Ok((result2, rest)) => (result2, rest),
                    Err(error) => return Err(Either::Right(error)),
                };
                Ok(((result1, result2), rest))
            })),
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1, Either<Error1, Error2>> {
        self.transform(move |(first, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2, Either<Error1, Error2>> {
        self.transform(move |(_, second)| second)
    }
}

impl<'a, Input, T1, T2, Error1, Error2> Parse<'a, Input, (T1, T2), Either<Error1, Error2>>
    for Pair<'a, Input, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
{
    fn parse(&mut self, input: Input) -> ParseResult<'a, Input, (T1, T2), Either<Error1, Error2>> {
        (*self.parser.borrow_mut()).parse(input)
    }
}

impl<'a, Input, T1, T2, Error1, Error2> Debug for Pair<'a, Input, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
