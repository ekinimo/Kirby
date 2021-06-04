use std::fmt::Debug;

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

pub type ParseResult<'a, Input, Output> = Result<(Output, Input), String>;

pub trait Parse<'a, Input, Output>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    Output: Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output>;

    fn transform<TransformFunction, Output2: Debug>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, Output2>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        Output2: 'a + Clone,
        TransformFunction: Fn(Output) -> Output2 + 'a,
    {
        Parser::new(move |input| {
            self.parse(input)
                .map(|(result, rest)| (transform_function(result), rest))
        })
    }

    fn predicate<PredicateFunction>(
        self,
        pred_fn: PredicateFunction,
        error_message: String,
    ) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(move |input: Input| {
            let (value, next_input) = self.parse(input.clone())?;
            if pred_fn(&value) {
                Ok((value, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn new_parser_from_parse_result<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser<'a, Input, Output2>
    where
        Self: Sized + 'a,
        Output: 'a + Debug + Clone,
        Output2: 'a + Debug + Clone,
        NextParser: Parse<'a, Input, Output2> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a,
    {
        Parser::new(move |input| {
            let (result, next_input) = self.parse(input)?;
            f(result).parse(next_input)
        })
    }

    fn or_else<Parser1>(self, parser2: Parser1) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
        Parser1: Parse<'a, Input, Output> + 'a,
    {
        self.either(parser2).fold(|left| left, |right| right)
    }

    fn pair<Parser1, Output2>(self, parser2: Parser1) -> Pair<'a, Input, Output, Output2>
    where
        Self: Sized + 'a,
        Output: Debug + Clone + 'a,
        Output2: Debug + Clone + 'a,
        Parser1: Parse<'a, Input, Output2> + 'a,
    {
        Pair::new(self, parser2)
    }

    fn triple<Parser1, Parser2, Output2, Output3>(
        self,
        parser2: Parser1,
        parser3: Parser2,
    ) -> Triple<'a, Input, Output, Output2, Output3>
    where
        Self: Sized + 'a,
        Output: Debug + Clone + 'a,
        Output2: Debug + Clone + 'a,
        Parser1: Parse<'a, Input, Output2> + 'a,
        Output3: Debug + Clone + 'a,
        Parser2: Parse<'a, Input, Output3> + 'a,
    {
        Triple::new(self, parser2, parser3)
    }

    fn either<Parser2, Output2>(self, parser2: Parser2) -> EitherParser<'a, Input, Output, Output2>
    where
        Self: Sized + 'a,
        Output: Debug + Clone + 'a,
        Output2: Debug + Clone + 'a,
        Parser2: Parse<'a, Input, Output2> + 'a,
    {
        EitherParser::new(self, parser2)
    }

    fn zero_or_more(self) -> RepeatedParser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
    {
        //todo!();
        RepeatedParser::zero_or_more(self)
    }

    fn one_or_more(self) -> RepeatedParser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
    {
        //todo!();
        RepeatedParser::one_or_more(self)
    }
}

impl<'a, Function, Input, Output> Parse<'a, Input, Output> for Function
where
    Function: Fn(Input) -> ParseResult<'a, Input, Output> + 'a,
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    Output: Debug + Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output> {
        self(input)
    }
}
