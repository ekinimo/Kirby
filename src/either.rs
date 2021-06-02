use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct EitherParser<'a, Input, T1, T2>
where
    Input: Debug + Iterator + 'a,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, Either<T1, T2>> + 'a>,
}

impl<'a, Input, T1, T2> EitherParser<'a, Input, T1, T2>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1> + 'a,
        P2: Parse<'a, Input, T2> + 'a,
    {
        Self {
            parser: Rc::new(move |input: Input| match parser1.parse(input.clone()) {
                Ok((left_result, rest)) => Ok((Either::Left(left_result), rest)),
                Err(_) => match parser2.parse(input.clone()) {
                    Ok((right_result, remaining)) => Ok((Either::Right(right_result), remaining)),
                    Err(_) => Err(format!("Parser Combinator : Either parser failed, remaining {:?}",input).to_string()),
                },
            }),
        }
    }
    pub fn try_left(self) -> Parser<'a, Input, Result<T1, Either<T1, T2>>> {
        self.clone().transform(move |x| x.try_into_left())
    }

    pub fn try_right(self) -> Parser<'a, Input, Result<T2, Either<T1, T2>>> {
        self.clone().transform(move |x| x.try_into_right())
    }

    pub fn as_left(&self) -> Parser<'a, Input, Option<T1>> {
        self.clone().transform(move |x| x.as_left())
    }

    pub fn as_right(&self) -> Parser<'a, Input, Option<T2>> {
        self.clone().transform(move |x| x.as_right())
    }

    pub fn is_left(&self) -> Parser<'a, Input, bool> {
        self.clone().transform(move |x| x.is_left())
    }

    pub fn is_right(&self) -> Parser<'a, Input, bool> {
        self.clone().transform(move |x| x.is_right())
    }
}

impl<'a, Input, T1, T2> Parse<'a, Input, Either<T1, T2>> for EitherParser<'a, Input, T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Either<T1, T2>> {
        self.parser.parse(input)
    }
}

#[derive(Clone, Debug)]
pub enum Either<T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    Left(T1),
    Right(T2),
}

impl<T1, T2> Either<T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    /// Returns `true` if the either_type is [`Left`].
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

    /// Returns `true` if the either_type is [`Right`].
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

impl<'a, Input, T1, T2> Debug for EitherParser<'a, Input, T1, T2>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
