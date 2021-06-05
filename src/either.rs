use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct EitherParser<'a, Input, T1, T2, Error>
where
    Input: Iterator + 'a,
    <Input as Iterator>::Item: Eq + Clone,
    T1: Clone,
    T2: Clone,
{
    parser: Rc<dyn Parse<'a, Input, Either<T1, T2>, (Error, Error)> + 'a>,
}

impl<'a, Input, T1, T2, Error> EitherParser<'a, Input, T1, T2, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Clone,
    T1: Clone + 'a,
    T2: Clone + 'a,
    Error: Clone + 'a,
{
    pub fn new<P1, P2>(left_parser: P1, right_parser: P2) -> Self
    where
        P1: Parse<'a, Input, T1, Error> + 'a,
        P2: Parse<'a, Input, T2, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |input: Input| match left_parser.parse(input.clone()) {
                Err(first_error_message) => match right_parser.parse(input) {
                    Err(second_error_message) => Err((first_error_message, second_error_message)),
                    Ok((output, input)) => Ok((Either::Right(output), input)),
                },
                Ok((output, input)) => Ok((Either::Left(output), input)),
            }),
        }
    }

    pub fn fold<Output>(
        self,
        left_transformation: fn(T1) -> Output,
        right_transformation: fn(T2) -> Output,
    ) -> Parser<'a, Input, Output, (Error, Error)>
    where
        <Input as Iterator>::Item: Clone + Eq,
        Input: 'a + Clone + Iterator,
        Output: Clone + 'a,
    {
        self.transform(move |either| match either {
            Either::Left(left) => left_transformation(left),
            Either::Right(right) => right_transformation(right),
        })
    }
}

impl<'a, Input, T1, T2, Error> Parse<'a, Input, Either<T1, T2>, (Error, Error)>
    for EitherParser<'a, Input, T1, T2, Error>
where
    T1: Clone,
    T2: Clone,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Clone,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Either<T1, T2>, (Error, Error)> {
        self.parser.parse(input)
    }
}

#[derive(Clone, Debug)]
pub enum Either<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    Left(T1),
    Right(T2),
}

impl<T1, T2> Either<T1, T2>
where
    T1: Clone,
    T2: Clone,
{
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(..))
    }

    pub fn as_left(&self) -> Option<T1> {
        if let Self::Left(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(..))
    }

    pub fn as_right(&self) -> Option<T2> {
        if let Self::Right(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    pub fn try_into_left(self) -> Result<T1, Self> {
        if let Self::Left(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_right(self) -> Result<T2, Self> {
        if let Self::Right(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl<'a, Input, T1, T2, Error> Debug for EitherParser<'a, Input, T1, T2, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Clone,
    T1: Clone + 'a,
    T2: Clone + 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
