use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct EitherParser<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: IntoIterator + 'a,
    <Input as IntoIterator>::Item: Eq,
{
    parser: Rc<dyn Parse<'a, Input, State, Either<T1, T2>, (Error1, Error2)> + 'a>,
}

impl<'a, Input, State, T1, T2, Error1, Error2>
    EitherParser<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    State: Clone,
{
    pub fn new<P1, P2>(left_parser: P1, right_parser: P2) -> Self
    where
        P1: Parse<'a, Input, State, T1, Error1> + 'a,
        P2: Parse<'a, Input, State, T2, Error2> + 'a,
    {
        Self {
            parser: Rc::new(move |input: Input, state: State| {
                match left_parser.parse(input.clone(), state.clone()) {
                    Err(first_error_message) => match right_parser.parse(input, state) {
                        Err(second_error_message) => {
                            Err((first_error_message, second_error_message))
                        }
                        Ok((output, state, input)) => Ok((Either::Right(output), state, input)),
                    },
                    Ok((output, state, input)) => Ok((Either::Left(output), state, input)),
                }
            }),
        }
    }

    pub fn fold<Output>(
        self,
        left_transformation: fn(T1) -> Output,
        right_transformation: fn(T2) -> Output,
    ) -> Parser<'a, Input, State, Output, (Error1, Error2)>
    where
        <Input as IntoIterator>::Item: Eq,
        Input: 'a + Clone + IntoIterator,
        Output: 'a,
        State: 'a,
    {
        self.transform(move |either| either.fold(left_transformation, right_transformation))
    }
}

impl<'a, Input, State, T1, T2, Error1, Error2>
    Parse<'a, Input, State, Either<T1, T2>, (Error1, Error2)>
    for EitherParser<'a, Input, State, T1, T2, Error1, Error2>
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
    ) -> ParseResult<'a, Input, State, Either<T1, T2>, (Error1, Error2)> {
        self.parser.parse(input, state)
    }
}

#[derive(Clone, Debug)]
pub enum Either<T1, T2> {
    Left(T1),
    Right(T2),
}

impl<T1, T2> Either<T1, T2> {
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(..))
    }

    pub fn as_left(&self) -> Option<T1>
    where
        T1: Clone,
        T2: Clone,
    {
        if let Self::Left(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(..))
    }

    pub fn as_right(&self) -> Option<T2>
    where
        T1: Clone,
        T2: Clone,
    {
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

    pub fn fold<T>(self, left_transformer: fn(T1) -> T, right_transformer: fn(T2) -> T) -> T {
        match self {
            Either::Left(left) => left_transformer(left),
            Either::Right(right) => right_transformer(right),
        }
    }
}

impl<T> Either<T, T> {
    pub fn reduce(self) -> T {
        match self {
            Either::Left(left) => left,
            Either::Right(right) => right,
        }
    }
}

impl<'a, Input, State, T1, T2, Error1, Error2> Debug
    for EitherParser<'a, Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub enum Either3<T1, T2, T3> {
    Left(T1),
    Middle(T2),
    Right(T3),
}

impl<T1, T2, T3> Either3<T1, T2, T3> {
    pub fn fold<T>(
        self,
        left_transformer: fn(T1) -> T,
        middle_transformer: fn(T2) -> T,
        right_transformer: fn(T3) -> T,
    ) -> T {
        match self {
            Either3::Left(left) => left_transformer(left),
            Either3::Middle(middle) => middle_transformer(middle),
            Either3::Right(right) => right_transformer(right),
        }
    }
}

impl<T> Either3<T, T, T> {
    pub fn reduce(self) -> T {
        match self {
            Either3::Left(left) => left,
            Either3::Middle(middle) => middle,
            Either3::Right(right) => right,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::either::{Either, Either3, EitherParser};
    use crate::parser::match_character;
    use crate::Parse;

    #[test]
    fn either_with_failing_left_parser() {
        let left = match_character('a');
        let right = match_character('b');

        let under_test = EitherParser::new(left, right);

        let result = under_test.parse("b".chars(), ());

        match result {
            Ok((Either::Right('b'), _, input)) => {
                assert_eq!(input.as_str(), "")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn either_with_failing_right_parser() {
        let left = match_character('a');
        let right = match_character('b');

        let under_test = EitherParser::new(left, right);

        let result = under_test.parse("a".chars(), ());

        match result {
            Ok((Either::Left('a'), _, input)) => {
                assert_eq!(input.as_str(), "")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn either_with_both_failing_parsers() {
        let left = match_character('a');
        let right = match_character('b');

        let under_test = EitherParser::new(left, right);

        let result = under_test.parse("1".chars(), ());

        match result {
            Err((left, right)) => {
                assert_eq!(left, "expected 'a', got '1'".to_string());
                assert_eq!(right, "expected 'b', got '1'".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn reduce_either() {
        let under_test: Either<u8, u8> = Either::Left(1);

        assert_eq!(1, under_test.reduce())
    }

    #[test]
    fn reduce_either3() {
        let under_test: Either3<u8, u8, u8> = Either3::Right(1);

        assert_eq!(1, under_test.reduce())
    }
}
