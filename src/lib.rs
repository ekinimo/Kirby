use crate::either::EitherParser;
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
    <Input as Iterator>::Item: Eq + Clone,
    Output: 'a,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output, Error>;

    fn with_error<F, Error2>(self, error_mapper: F) -> Parser<'a, Input, Output, Error2>
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
        self,
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

    fn validate<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(move |input: Input| {
            let (value, next_input) = self.parse(input.clone())?;
            if predicate(&value) {
                Ok((value, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn new_parser_from_parse_result<F, NextParser, Output2>(
        self,
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

    fn or_else<Parser1>(self, parser2: Parser1) -> Parser<'a, Input, Output, (Error, Error)>
    where
        Self: Sized + 'a,
        Output: 'a,
        Parser1: Parse<'a, Input, Output, Error> + 'a,
    {
        self.either(parser2).fold(|left| left, |right| right)
    }

    fn pair<Parser1, Output2>(self, parser2: Parser1) -> Pair<'a, Input, Output, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser1: Parse<'a, Input, Output2, Error> + 'a,
    {
        Pair::new(self, parser2)
    }

    fn triple<Parser1, Parser2, Output2, Output3>(
        self,
        parser2: Parser1,
        parser3: Parser2,
    ) -> Triple<'a, Input, Output, Output2, Output3, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser1: Parse<'a, Input, Output2, Error> + 'a,
        Output3: 'a,
        Parser2: Parse<'a, Input, Output3, Error> + 'a,
    {
        Triple::new(self, parser2, parser3)
    }

    fn either<Parser2, Output2>(
        self,
        parser2: Parser2,
    ) -> EitherParser<'a, Input, Output, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, Output2, Error> + 'a,
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

    fn skip<P, T>(self, skip_parser: P) -> Parser<'a, Input, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
        P: Parse<'a, Input, T, Error> + 'a,
        T: Clone + 'a,
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
    Function: Fn(Input) -> ParseResult<'a, Input, Output, Error> + 'a,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Clone,
    Output: 'a,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output, Error> {
        self(input)
    }
}
