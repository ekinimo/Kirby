use either::Either3;

use crate::either::{Either, EitherParser};
use crate::pair::Pair;
use crate::parser::Parser;
use crate::repeated::RepeatedParser;
use crate::triple::Triple;

pub mod either;
pub mod pair;
pub mod parser;
pub mod repeated;
pub mod triple;

pub type ParseResult<'a, Input, State, Output, Error> = Result<(Output, State, Input), Error>;

pub trait Parse<'a, Input, State, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'a,
    Error: Clone + 'a,
    State: Clone,
{
    fn parse(&self, input: Input, state: State) -> ParseResult<'a, Input, State, Output, Error>;

    fn with_error<F, Error2>(self, error_mapper: F) -> Parser<'a, Input, State, Output, Error2>
    where
        Self: Sized + 'a,
        F: Fn(Error, Input) -> Error2 + 'a,
        Error2: Clone,
    {
        Parser::new(move |input: Input, state: State| {
            self.parse(input.clone(), state)
                .map_err(|error_message| error_mapper(error_message, input))
        })
    }

    fn with_error_using_state<F, Error2>(self, error_mapper: F) -> Parser<'a, Input, State, Output, Error2>
    where
        Self: Sized + 'a,
        F: Fn(Error,State ,Input) -> Error2 + 'a,
        Error2: Clone,
    {
        Parser::new(move |input: Input, state: State| {
            self.parse(input.clone(), state.clone())
                .map_err(|error_message| error_mapper(error_message, state,input))
        })
    }

    fn transform<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output) -> Output2 + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (transform_function(result), state, rest))
        })
    }

    fn with_state_transition<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        TransformFunction: Fn(State) -> State + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (result, transform_function(state), rest))
        })
    }

    fn peek_and_change_state<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        TransformFunction: Fn(State, &Input) -> State + 'a,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (result, transform_function(state, &rest), rest))
        })
    }

    fn peek_and_change_state_using_result<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        TransformFunction: Fn(State, Output, &Input) -> State + 'a,
    Output:Clone,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| {
                    let state = transform_function(state, result.clone(), &rest);
                    (result, state, rest)
                })
        })
    }

    fn peek_and_change_state_using_result_ref<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        TransformFunction: Fn(State, &Output, &Input) -> State + 'a,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| {
                    let state = transform_function(state, &result, &rest);
                    (result, state, rest)
                })
        })
    }
    fn transform_with_error<TransformFunction, ErrorFunction, Output2, Error2>(
        self,
        transform_function: TransformFunction,
        error_function: ErrorFunction,
    ) -> Parser<'a, Input, State, Output2, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output) -> Output2 + 'a,
        ErrorFunction: Fn(Error) -> Error2 + 'a,
        Error2: Clone,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map_or_else(
                |err| Err(error_function(err)),
                |(result, state, rest)| Ok((transform_function(result), state, rest)),
            )
        })
    }

    fn transform_and_state_transition<TransformFunction, TransitionFunction, Output2>(
        self,
        transform_function: TransformFunction,
        transition_function: TransitionFunction,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output) -> Output2 + 'a,
        TransitionFunction: Fn(State) -> State + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                (transform_function(result), transition_function(state), rest)
            })
        })
    }

    fn transform_with_state<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output, State) -> Output2 + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                (transform_function(result, state.clone()), state, rest)
            })
        })
    }

    fn transform_with_state_and_error<TransformFunction, ErrorFunction, Output2, Error2>(
        self,
        transform_function: TransformFunction,
        error_function: ErrorFunction,
    ) -> Parser<'a, Input, State, Output2, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output, State) -> Output2 + 'a,
        ErrorFunction: Fn(Error) -> Error2 + 'a,
        Error2: Clone,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map_or_else(
                |err| Err(error_function(err)),
                |(result, state, rest)| {
                    Ok((transform_function(result, state.clone()), state, rest))
                },
            )
        })
    }

    fn peek_and_transform<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output, &Input) -> Output2 + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (transform_function(result, &rest), state, rest))
        })
    }

    fn peek_and_transform_with_state<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        TransformFunction: Fn(Output, &State, &Input) -> Output2 + 'a,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                (
                    transform_function(result, &state, &rest),
                    state,
                    rest,
                )
            })
        })
    }

    fn validate<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value) {
                Ok((value, state, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn validate_with_custom_error<PredicateFunction,ErrorFunc,OutputError>(
        self,
        predicate: PredicateFunction,
        error_func: ErrorFunc,
    ) -> Parser<'a, Input, State, Output, OutputError>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
    OutputError : 'a + Clone + From<Error>,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    ErrorFunc:Fn(Output,State,Input) -> OutputError + 'a
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value) {
                Ok((value, state, next_input))
            } else {
                Err(error_func(value,state,next_input))
            }
        })
    }
    fn peek_and_validate<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        PredicateFunction: Fn(&Output, &Input) -> bool + 'a,
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value, &next_input) {
                Ok((value, state, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn peek_and_validate_with_state<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        PredicateFunction: Fn(&Output, &Input, &State) -> bool + 'a,
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value, &next_input, &state) {
                Ok((value, state, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn validate_with_state<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        PredicateFunction: Fn(&Output, &State) -> bool + 'a,
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value, &state) {
                Ok((value, state, next_input))
            } else {
                Err(error_message.clone())
            }
        })
    }

    fn new_parser_from_parse_result<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        NextParser: Parse<'a, Input, State, Output2, Error> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            f(result).parse(next_input, state)
        })
    }

    fn new_parser_from_parse_result_and_rest<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        NextParser: Parse<'a, Input, State, Output2, Error> + 'a + Clone,
        F: Fn(Output, &Input) -> NextParser + 'a,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            f(result, &next_input).parse(next_input, state)
        })
    }

    fn new_parser_from_parse_result_and_state<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        NextParser: Parse<'a, Input, State, Output2, Error> + 'a + Clone,
        F: Fn(Output, &State) -> NextParser + 'a,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            //what to  do with state here
            f(result, &state).parse(next_input, state)
        })
    }

    fn new_parser_from_parse_result_state_and_rest<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser<'a, Input, State, Output2, Error>
    where
        Self: Sized + 'a,
        Output2: 'a,
        NextParser: Parse<'a, Input, State, Output2, Error> + 'a + Clone,
        F: Fn(Output, &State, &Input) -> NextParser + 'a,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            //what to  do with state here
            f(result, &state, &next_input).parse(next_input, state)
        })
    }

    fn or_else<Parser2, Error2>(
        self,
        parser2: Parser2,
    ) -> Parser<'a, Input, State, Output, (Error, Error2)>
    where
        Self: Sized + 'a,
        Output: 'a,
        Parser2: Parse<'a, Input, State, Output, Error2> + 'a,
        Error2: Clone,
        State: 'a,
    {
        self.either(parser2).fold(|left| left, |right| right)
    }

    fn pair<Parser2, Output2, Error2>(
        self,
        parser2: Parser2,
    ) -> Pair<'a, Input, State, Output, Output2, Error, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, State, Output2, Error2> + 'a,
        Error2: Clone + 'a,
        State: 'a,
    {
        Pair::new(self, parser2)
    }

    fn followed_by<Parser2, Output2, Error2,Error3,Output3,TransformFunction,ErrorMapper1,ErrorMapper2>(
        self,
        parser2: Parser2,
        transform_function: TransformFunction,
        error_mapper1: ErrorMapper1,
        error_mapper2: ErrorMapper2,
    ) ->Parser<'a, Input, State, Output3, Error3>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, State, Output2, Error2> + 'a,
        Error2: Clone + 'a,
        Error3: Clone + 'a,
        Output3: Clone + 'a,
    TransformFunction: Fn(Output,Output2)->Output3 + 'a,
    ErrorMapper1: Fn(Error,State,Input)->Error3 + 'a,
        ErrorMapper2: Fn(Error2,State,Input)->Error3 + 'a,


        State: 'a,
    {
        self.pair(parser2).transform(move |(x,y)| transform_function(x,y)).with_error_using_state( move |err,state,input| {
            match err {
                Either::Left(left) => error_mapper1(left,state,input),
                Either::Right(left) => error_mapper2(left,state,input),
        }})
    }

    fn triple<Parser2, Parser3, Output2, Output3, Error2, Error3>(
        self,
        parser2: Parser2,
        parser3: Parser3,
    ) -> Triple<'a, Input, State, Output, Output2, Output3, Error, Error2, Error3>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, State, Output2, Error2> + 'a,
        Output3: 'a,
        Parser3: Parse<'a, Input, State, Output3, Error3> + 'a,
        Error2: Clone + 'a,
        Error3: Clone + 'a,
        State: 'a,
    {
        Triple::new(self, parser2, parser3)
    }

    fn left_assoc<ParserRight, ParserMiddle, RightOutput, MiddleOutput, RightError, MiddleError>(
        self,
        middle_parser: ParserMiddle,
        right_parser: ParserRight,
        
    ) ->EitherParser<'a, Input, State, (RightOutput, MiddleOutput, Output), RightOutput, Either3<RightError, MiddleError, Error>, RightError>
        
    where
        Self: Sized + 'a + Clone,
        RightOutput: 'a,
        ParserRight: Parse<'a, Input, State, RightOutput, RightError> + 'a + Clone,
        MiddleOutput: 'a,
        ParserMiddle: Parse<'a, Input, State, MiddleOutput, MiddleError> + 'a,
        RightError: Clone + 'a,
        MiddleError: Clone + 'a,
        State: 'a,
    {
        EitherParser::new(Triple::new(right_parser.clone(), middle_parser, self), right_parser)
       
    }

    fn right_assoc<ParserLeft, ParserMiddle, LeftOutput, MiddleOutput, LeftError, MiddleError>(
        self,
        left_parser: ParserLeft ,
        middle_parser: ParserMiddle,
    ) -> EitherParser<'a, Input, State, (Output, MiddleOutput, LeftOutput), LeftOutput, Either3<Error, MiddleError, LeftError>, LeftError>
    //Pair<'a, Input, State, LeftOutput, Vec<(MiddleOutput, Output)>, LeftError, Either<MiddleError, Error>>
    where
        Self: Sized + 'a,
        LeftOutput: 'a,
        ParserLeft: Parse<'a, Input, State, LeftOutput, LeftError> + 'a + Clone,
        MiddleOutput: 'a,
        ParserMiddle: Parse<'a, Input, State, MiddleOutput, MiddleError> + 'a,
        LeftError: Clone + 'a,
        MiddleError: Clone + 'a,
        State: 'a,
    {
        EitherParser::new(Triple::new(self, middle_parser, left_parser.clone()), left_parser)
        //left_parser.pair(middle_parser.pair(self).zero_or_more())
    }

    fn either<Parser2, Output2, Error2>(
        self,
        parser2: Parser2,
    ) -> EitherParser<'a, Input, State, Output, Output2, Error, Error2>
    where
        Self: Sized + 'a,
        Output2: 'a,
        Parser2: Parse<'a, Input, State, Output2, Error2> + 'a,
        Error2: Clone,
    {
        EitherParser::new(self, parser2)
    }

    fn zero_or_more(self) -> RepeatedParser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        //todo!();
        RepeatedParser::zero_or_more(self)
    }

    fn one_or_more(self) -> RepeatedParser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        //todo!();
        RepeatedParser::one_or_more(self)
    }



    fn separated_by<P, Output2, Error2>(
        self,
        separator: P,
    ) -> Pair<'a, Input, State, Output, Vec<(Output2, Output)>, Error, Either<Error2, Error>>
    where
        Self: Sized + Clone + 'a,
        P: Parse<'a, Input, State, Output2, Error2> + 'a,
        Output2: 'a,
        Error2: Clone,
        State: 'a,
    {
        Pair::new(
            self.clone(),
            RepeatedParser::zero_or_more(separator.pair(self)),
        )
    }

    fn skip<P, T, E>(self, skip_parser: P) -> Parser<'a, Input, State, Output, Error>
    where
        Self: Sized + 'a,
        Output: 'a,
        P: Parse<'a, Input, State, T, E> + 'a,
        T: Clone + 'a,
        E: Clone + 'a,
    {
        Parser::new(move |mut input: Input, mut state: State| {
            while let Ok((_, s, new_input)) = skip_parser.parse(input.clone(), state.clone()) {
                input = new_input;
                state = s;
            }
            let (output, mut state, mut input) = self.parse(input, state)?;

            while let Ok((_, s, new_input)) = skip_parser.parse(input.clone(), state.clone()) {
                input = new_input;
                state = s;
            }

            Ok((output, state, input))
        })
    }
}

impl<'a, Function, Input, State, Output, Error> Parse<'a, Input, State, Output, Error> for Function
where
    Function: Fn(Input, State) -> ParseResult<'a, Input, State, Output, Error> + 'a,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'a,
    Error: Clone + 'a,
    State: Clone,
{
    fn parse(&self, input: Input, state: State) -> ParseResult<'a, Input, State, Output, Error> {
        self(input, state)
    }
}

#[cfg(test)]
mod tests {
    use crate::either::Either;
    use crate::parser::match_character;
    use crate::Parse;

    #[test]
    fn separated_by() {
        let under_test = match_character('1').separated_by(match_character('-'));

        let result = under_test.parse("1".chars(), ());

        match result {
            Ok((('1', separated), _, rest)) => {
                assert!(separated.is_empty());
                assert_eq!("", rest.as_str())
            }
            _ => panic!("failed with {:?}", result),
        }

        let result = under_test.parse("1-1-1-2-3".chars(), ());

        match result {
            Ok((('1', separated), _, rest)) => {
                let expected = vec![('-', '1'), ('-', '1')];
                assert_eq!(expected, separated);
                assert_eq!("-2-3", rest.as_str())
            }
            _ => panic!("failed with {:?}", result),
        }

        let result = under_test.parse("abc".chars(), ());

        match result {
            Err(Either::Left(message)) => {
                assert_eq!("expected '1', got 'a'", message)
            }
            _ => panic!("failed with {:?}", result),
        }
    }
}
