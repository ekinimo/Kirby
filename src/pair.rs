use std::fmt::Debug;
use std::rc::Rc;

use crate::either::Either;
use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Pair<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
{
    parser: Rc<dyn Parse<'a, Input, State, (T1, T2), Either<Error1, Error2>> + 'a>,
}

impl<'a, Input, State, T1, T2, Error1, Error2> Pair<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    State: Clone + 'a,
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, State, T1, Error1> + 'a,
        P2: Parse<'a, Input, State, T2, Error2> + 'a,
    {
        Self {
            parser: Rc::new(move |input, state| {
                let (result1, state, input2) = match parser1.parse(input, state) {
                    Ok((result1, state, input2)) => (result1, state, input2),
                    Err(error) => return Err(Either::Left(error)),
                };
                let (result2, state, rest) = match parser2.parse(input2, state) {
                    Ok((result2, state, rest)) => (result2, state, rest),
                    Err(error) => return Err(Either::Right(error)),
                };
                Ok(((result1, result2), state, rest))
            }),
        }
    }

    pub fn first(self) -> Parser<'a, Input, State, T1, Either<Error1, Error2>> {
        self.transform(move |(first, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, State, T2, Either<Error1, Error2>> {
        self.transform(move |(_, second)| second)
    }
}

impl<'a, Input, State, T1, T2, Error1, Error2>
    Parse<'a, Input, State, (T1, T2), Either<Error1, Error2>>
    for Pair<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    State: Clone,
{
    fn parse(
        &self,
        input: Input,
        state: State,
    ) -> ParseResult<'a, Input, State, (T1, T2), Either<Error1, Error2>> {
        self.parser.parse(input, state)
    }
}


impl<'a, Input, State, T1, T2, Error1, Error2,A,B>
    Parse<'a, Input, State, (T1, T2), Either<Error1, Error2>>
    for (A,B)
where
    Input: Clone + 'a + IntoIterator,
<Input as IntoIterator>::Item: Eq,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    State: Clone,
    T1 : 'a,
    T2 : 'a,
    A:Parse<'a, Input, State, T1, Error1>,
    B:Parse<'a, Input, State, T2, Error2>
{
    fn parse(
        &self,
        input: Input,
        state: State,
    ) -> ParseResult<'a, Input, State, (T1, T2), Either<Error1, Error2>> {
        let (result1, state, input2) = match self.0.parse(input, state) {
            Ok((result1, state, input2)) => (result1, state, input2),
            Err(error) => return Err(Either::Left(error)),
        };
        let (result2, state, rest) = match self.1.parse(input2, state) {
            Ok((result2, state, rest)) => (result2, state, rest),
            Err(error) => return Err(Either::Right(error)),
        };
        Ok(((result1, result2), state, rest))
    }
}

impl<'a, Input, State, T1, T2, Error1, Error2> Debug
    for Pair<'a, Input, State, T1, T2, Error1, Error2>
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
