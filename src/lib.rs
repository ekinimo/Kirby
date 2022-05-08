#![recursion_limit = "256"]
use crate::either::{EitherParser, Either};
use crate::pair::Pair;
use crate::parser::Parser;
use crate::repeated::RepeatedParser;
use crate::triple::Triple;

pub mod either;
pub mod pair;
pub mod parser;
pub mod repeated;
pub mod triple;

pub type ParseResult<'a, Input, Output, Error> = Result<(Output, Input), Error>;

pub trait Parse<'a, Input, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'a,
    Error: Clone + 'a,
{
    fn parse(&mut self, input: Input) -> ParseResult<'a, Input, Output, Error>;

    fn with_error<F, Error2>(mut self, error_mapper: F) -> Parser<'a, Input, Output, Error2>
    where
        Self: Sized + 'a,
        F: Fn(Error, Input) -> Error2 + 'a,
        Error2: Clone,
    {
        Parser::new(move |input: Input| {
            self.parse(input.clone())
                .map_err(|error_message| error_mapper(error_message, input))
        })
    }

    fn transform<TransformFunction, Output2>(
        mut self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output) -> Output2 + 'a,
    {
        Parser::new(move |input| {
            self.parse(input)
                .map(|(result, rest)| (transform_function(result), rest))
        })
    }

    fn peek_and_transform<TransformFunction,  Output2>(
        mut self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, Output2, Error>
    where
        Self: Sized + 'a ,
        Output2: 'a,
        TransformFunction:  Fn(Output,  Input) -> Output2 + 'a ,
    {
        Parser::new(move |input| {
            self.parse(input)
                .map(|(result, rest)| (transform_function(result,rest.clone()), rest))
        })
    }



    fn validate<PredicateFunction>(
        mut self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(move |input: Input| {
            let (value, next_input) = self.parse(input)?;
            if predicate(&value) {
                Ok((value, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn new_parser_from_parse_result<F, NextParser, Output2>(
        mut self,
        f: F,
    ) -> Parser<'a, Input, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        NextParser: Parse<'a, Input, Output2, Error> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a,
    {
        Parser::new(move |input| {
            let (result, next_input) = self.parse(input)?;
            f(result).parse(next_input)
        })
    }

    fn or_else<Parser2, Error2>(
         self,
        parser2: Parser2,
    ) -> Parser<'a, Input, Output, (Error, Error2)>
    where
        Self: Sized + 'a,
        Output: 'a,
        Parser2: Parse<'a, Input, Output, Error2> + 'a,
        Error2: Clone,
    {
        self.either(parser2).fold(|left| left, |right| right)
    }


    fn pair<Parser2, Output2, Error2>(
         self,
        parser2: Parser2,
    ) -> Pair<'a, Input, Output, Output2, Error, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, Output2, Error2> + 'a,
        Error2: Clone + 'a,
    {
        Pair::new(self, parser2)
    }

    fn triple<Parser2, Parser3, Output2, Output3, Error2, Error3>(
        self,
        parser2: Parser2,
        parser3: Parser3,
    ) -> Triple<'a, Input, Output, Output2, Output3, Error, Error2, Error3>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, Output2, Error2> + 'a,
        Output3: 'a,
        Parser3: Parse<'a, Input, Output3, Error3> + 'a,
        Error2: Clone + 'a,
        Error3: Clone + 'a,
    {
        Triple::new(self, parser2, parser3)
    }

    fn either<Parser2, Output2, Error2>(
        self,
        parser2: Parser2,
    ) -> EitherParser<'a, Input, Output, Output2, Error, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, Output2, Error2> + 'a,
        Error2: Clone,
    {
        EitherParser::new(self, parser2)
    }

    fn zero_or_more(self) -> RepeatedParser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        //todo!();
        RepeatedParser::zero_or_more(self)
    }

    fn one_or_more(self) -> RepeatedParser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        //todo!();
        RepeatedParser::one_or_more(self)
    }

    fn separated_by<P, Output2, Error2>(self, separator: P) -> Pair<'a, Input, Output, Vec<(Output2, Output)>, Error, Either<Error2, Error>>
    where
        Self: Sized + Clone + 'a,
        P: Parse<'a, Input, Output2, Error2> + 'a,
        Output2: 'a,
        Error2: Clone,
    {
        Pair::new(
            self.clone(),
            RepeatedParser::zero_or_more(separator.pair(self))
        )
    }

    fn skip<P, T, E>(mut self, mut skip_parser: P) -> Parser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
        P: Parse<'a, Input, T, E> + 'a,
        T: Clone + 'a,
        E: Clone + 'a,
    {
        Parser::new(move |mut input: Input| {
            while let Ok((_, new_input)) = skip_parser.parse(input.clone()) {
                input = new_input;
            }
            let (output, mut input) = self.parse(input)?;

            while let Ok((_, new_input)) = skip_parser.parse(input.clone()) {
                input = new_input;
            }

            Ok((output, input))
        })
    }
}

impl<'a, Function, Input, Output, Error> Parse<'a, Input, Output, Error> for Function
where
    Function: FnMut(Input) -> ParseResult<'a, Input, Output, Error> + 'a,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'a,
    Error: Clone + 'a,
{
    fn parse(&mut self, input: Input) -> ParseResult<'a, Input, Output, Error> {
        self(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::match_character;
    use crate::Parse;
    use crate::either::Either;

    #[test]
    fn separated_by() {
        let mut under_test = match_character('1').separated_by(match_character('-'));

        let result = under_test.parse("1".chars());

        match result {
            Ok((('1', separated), rest)) => {
                assert!(separated.is_empty());
                assert_eq!("", rest.as_str())
            }
            _ => panic!("failed with {:?}", result)
        }

        let result = under_test.parse("1-1-1-2-3".chars());

        match result {
            Ok((('1', separated), rest)) => {
                let expected = vec![
                    ('-', '1'),
                    ('-', '1')
                ];
                assert_eq!(expected, separated);
                assert_eq!("-2-3", rest.as_str())
            }
            _ => panic!("failed with {:?}", result)
        }

        let result = under_test.parse("abc".chars());

        match result {
            Err(Either::Left(message)) => {
                assert_eq!("expected '1', got 'a'", message)
            }
            _ => panic!("failed with {:?}", result)
        }
    }
}
