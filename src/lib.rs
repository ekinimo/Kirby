#![feature(once_cell)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(test)]

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

pub type ParseResult< Input, State, Output, Error> = Result<(Output, State, Input), Error>;

pub trait Parse< Input, State, Output, Error>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'static,
    Error: Clone + 'static,
    State: Clone,
{
    fn parse(&self, input: Input, state: State) -> ParseResult< Input, State, Output, Error>;

    fn with_error<F, Error2>(self, error_mapper: F) -> Parser< Input, State, Output, Error2>
    where
        Self: Sized + 'static,
        F: Fn(Error, Input) -> Error2 + 'static,
        Error2: Clone +'static,
    {
        Parser::new(move |input: Input, state: State| {
            self.parse(input.clone(), state)
                .map_err(|error_message| error_mapper(error_message, input))
        })
    }

    fn with_error_using_state<F, Error2>(
        self,
        error_mapper: F,
    ) -> Parser< Input, State, Output, Error2>
    where
        Self: Sized + 'static,
        F: Fn(Error, State, Input) -> Error2 + 'static,
        Error2: Clone +'static,
    {
        Parser::new(move |input: Input, state: State| {
            self.parse(input.clone(), state.clone())
                .map_err(|error_message| error_mapper(error_message, state, input))
        })
    }

    fn transform<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output) -> Output2 + 'static,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (transform_function(result), state, rest))
        })
    }

    fn with_state_transition<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        TransformFunction: Fn(State) -> State + 'static,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (result, transform_function(state), rest))
        })
    }

    fn peek_and_change_state<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        TransformFunction: Fn(State, &Input) -> State + 'static,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (result, transform_function(state, &rest), rest))
        })
    }

    fn peek_and_change_state_using_result<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        TransformFunction: Fn(State, Output, &Input) -> State + 'static,
        Output: Clone,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                let state = transform_function(state, result.clone(), &rest);
                (result, state, rest)
            })
        })
    }

    fn peek_and_change_state_using_result_ref<TransformFunction>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        TransformFunction: Fn(State, &Output, &Input) -> State + 'static,
    {
        Parser::new(move |input: Input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                let state = transform_function(state, &result, &rest);
                (result, state, rest)
            })
        })
    }
    fn transform_with_error<TransformFunction, ErrorFunction, Output2, Error2>(
        self,
        transform_function: TransformFunction,
        error_function: ErrorFunction,
    ) -> Parser< Input, State, Output2, Error2>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output) -> Output2 + 'static,
        ErrorFunction: Fn(Error) -> Error2 + 'static,
        Error2: Clone +'static+'static,
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
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output) -> Output2 + 'static,
        TransitionFunction: Fn(State) -> State + 'static,
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
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output, State) -> Output2 + 'static,
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
    ) -> Parser< Input, State, Output2, Error2>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output, State) -> Output2 + 'static,
        ErrorFunction: Fn(Error) -> Error2 + 'static,
        Error2: Clone +'static,
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
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output, &Input) -> Output2 + 'static,
    {
        Parser::new(move |input, state| {
            self.parse(input, state)
                .map(|(result, state, rest)| (transform_function(result, &rest), state, rest))
        })
    }

    fn peek_and_transform_with_state<TransformFunction, Output2>(
        self,
        transform_function: TransformFunction,
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        TransformFunction: Fn(Output, &State, &Input) -> Output2 + 'static,
    {
        Parser::new(move |input, state| {
            self.parse(input, state).map(|(result, state, rest)| {
                (transform_function(result, &state, &rest), state, rest)
            })
        })
    }

    fn validate<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static + Clone,
        PredicateFunction: Fn(&Output) -> bool + 'static,
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

    fn validate_with_custom_error<PredicateFunction, ErrorFunc, OutputError>(
        self,
        predicate: PredicateFunction,
        error_func: ErrorFunc,
    ) -> Parser< Input, State, Output, OutputError>
    where
        Self: Sized + 'static,
        Output: 'static + Clone,
        OutputError: 'static + Clone + From<Error>,
        PredicateFunction: Fn(&Output) -> bool + 'static,
        ErrorFunc: Fn(Output, State, Input) -> OutputError + 'static,
    {
        Parser::new(move |input: Input, state: State| {
            let (value, state, next_input) = self.parse(input, state)?;
            if predicate(&value) {
                Ok((value, state, next_input))
            } else {
                Err(error_func(value, state, next_input))
            }
        })
    }
    fn peek_and_validate<PredicateFunction>(
        self,
        predicate: PredicateFunction,
        error_message: Error,
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static + Clone,
        PredicateFunction: Fn(&Output, &Input) -> bool + 'static,
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
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static + Clone,
        PredicateFunction: Fn(&Output, &Input, &State) -> bool + 'static,
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
    ) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static + Clone,
        PredicateFunction: Fn(&Output, &State) -> bool + 'static,
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
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        NextParser: Parse< Input, State, Output2, Error> + 'static + Clone,
        F: Fn(Output) -> NextParser + 'static,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            f(result).parse(next_input, state)
        })
    }

    fn new_parser_from_parse_result_and_rest<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        NextParser: Parse< Input, State, Output2, Error> + 'static + Clone,
        F: Fn(Output, &Input) -> NextParser + 'static,
    {
        Parser::new(move |input, state| {
            let (result, state, next_input) = self.parse(input, state)?;
            f(result, &next_input).parse(next_input, state)
        })
    }

    fn new_parser_from_parse_result_and_state<F, NextParser, Output2>(
        self,
        f: F,
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        NextParser: Parse< Input, State, Output2, Error> + 'static + Clone,
        F: Fn(Output, &State) -> NextParser + 'static,
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
    ) -> Parser< Input, State, Output2, Error>
    where
        Self: Sized + 'static,
        Output2: 'static,
        NextParser: Parse< Input, State, Output2, Error> + 'static + Clone,
        F: Fn(Output, &State, &Input) -> NextParser + 'static,
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
    ) -> Parser< Input, State, Output, (Error, Error2)>
    where
        Self: Sized + 'static,
        Output: 'static,
        Parser2: Parse< Input, State, Output, Error2> + 'static,
        Error2: Clone+'static,
        State: 'static,
    {
        self.either(parser2).fold(|left| left, |right| right)
    }

    fn pair<Parser2, Output2, Error2>(
        self,
        parser2: Parser2,
    ) -> Pair< Input, State, Output, Output2, Error, Error2>
    where
        Self: Sized + 'static,
        Output2: 'static,
        Parser2: Parse< Input, State, Output2, Error2> + 'static,
        Error2: Clone + 'static,
        State: 'static,
    {
        Pair::new(self, parser2)
    }

    fn followed_by<
        Parser2,
        Output2,
        Error2,
        Error3,
        Output3,
        TransformFunction,
        ErrorMapper1,
        ErrorMapper2,
    >(
        self,
        parser2: Parser2,
        transform_function: TransformFunction,
        error_mapper1: ErrorMapper1,
        error_mapper2: ErrorMapper2,
    ) -> Parser< Input, State, Output3, Error3>
    where
        Self: Sized + 'static,
        Output2: 'static,
        Parser2: Parse< Input, State, Output2, Error2> + 'static,
        Error2: Clone + 'static,
        Error3: Clone + 'static,
        Output3: Clone + 'static,
        TransformFunction: Fn(Output, Output2) -> Output3 + 'static,
        ErrorMapper1: Fn(Error, State, Input) -> Error3 + 'static,
        ErrorMapper2: Fn(Error2, State, Input) -> Error3 + 'static,

        State: 'static,
    {
        self.pair(parser2)
            .transform(move |(x, y)| transform_function(x, y))
            .with_error_using_state(move |err, state, input| match err {
                Either::Left(left) => error_mapper1(left, state, input),
                Either::Right(left) => error_mapper2(left, state, input),
            })
    }

    fn triple<Parser2, Parser3, Output2, Output3, Error2, Error3>(
        self,
        parser2: Parser2,
        parser3: Parser3,
    ) -> Triple< Input, State, Output, Output2, Output3, Error, Error2, Error3>
    where
        Self: Sized + 'static,
        Output2: 'static,
        Parser2: Parse< Input, State, Output2, Error2> + 'static,
        Output3: 'static,
        Parser3: Parse< Input, State, Output3, Error3> + 'static,
        Error2: Clone + 'static,
        Error3: Clone + 'static,
        State: 'static,
    {
        Triple::new(self, parser2, parser3)
    }

    fn left_assoc<ParserRight, ParserMiddle, RightOutput, MiddleOutput, RightError, MiddleError>(
        self,
        middle_parser: ParserMiddle,
        right_parser: ParserRight,
    ) -> EitherParser<
        
        Input,
        State,
        (RightOutput, MiddleOutput, Output),
        RightOutput,
        Either3<RightError, MiddleError, Error>,
        RightError,
    >
    where
        Self: Sized + 'static + Clone,
        RightOutput: 'static,
        ParserRight: Parse< Input, State, RightOutput, RightError> + 'static + Clone,
        MiddleOutput: 'static,
        ParserMiddle: Parse< Input, State, MiddleOutput, MiddleError> + 'static,
        RightError: Clone + 'static,
        MiddleError: Clone + 'static,
        State: 'static,
    {
        EitherParser::new(
            Triple::new(right_parser.clone(), middle_parser, self),
            right_parser,
        )
    }

    fn right_assoc<ParserLeft, ParserMiddle, LeftOutput, MiddleOutput, LeftError, MiddleError>(
        self,
        left_parser: ParserLeft,
        middle_parser: ParserMiddle,
    ) -> Pair<
        
        Input,
        State,
        LeftOutput,
        Vec<(MiddleOutput, LeftOutput)>,
        LeftError,
        Either<MiddleError, LeftError>,
    >
    where
        Self: Sized + 'static,
        LeftOutput: 'static,
        ParserLeft: Parse< Input, State, LeftOutput, LeftError> + 'static + Clone,
        MiddleOutput: 'static,
        ParserMiddle: Parse< Input, State, MiddleOutput, MiddleError> + 'static,
        LeftError: Clone + 'static,
        MiddleError: Clone + 'static,
        State: 'static,
    {
        //EitherParser::new(Triple::new(self, middle_parser, left_parser.clone()), left_parser)
        left_parser
            .clone()
            .pair(middle_parser.pair(left_parser).zero_or_more())
    }

    fn either<Parser2, Output2, Error2>(
        self,
        parser2: Parser2,
    ) -> EitherParser< Input, State, Output, Output2, Error, Error2>
    where
        Self: Sized + 'static,
        Output2: 'static,
        Parser2: Parse< Input, State, Output2, Error2> + 'static,
        Error2: Clone+'static,
    {
        EitherParser::new(self, parser2)
    }

    fn zero_or_more(self) -> RepeatedParser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static,
    {
        //todo!();
        RepeatedParser::zero_or_more(self)
    }

    fn one_or_more(self) -> RepeatedParser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static,
    {
        //todo!();
        RepeatedParser::one_or_more(self)
    }

    fn separated_by<P, Output2, Error2>(
        self,
        separator: P,
    ) -> Pair< Input, State, Output, Vec<(Output2, Output)>, Error, Either<Error2, Error>>
    where
        Self: Sized + Clone + 'static,
        P: Parse< Input, State, Output2, Error2> + 'static,
        Output2: 'static,
        Error2: Clone + 'static,
        State: 'static,
    {
        Pair::new(
            self.clone(),
            RepeatedParser::zero_or_more(separator.pair(self)),
        )
    }

    fn skip<P, T, E>(self, skip_parser: P) -> Parser< Input, State, Output, Error>
    where
        Self: Sized + 'static,
        Output: 'static,
        P: Parse< Input, State, T, E> + 'static,
        T: Clone + 'static,
        E: Clone + 'static,
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

impl< Function, Input, State, Output, Error> Parse< Input, State, Output, Error> for Function
where
    Function: Fn(Input, State) -> ParseResult< Input, State, Output, Error> + 'static,
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    Output: 'static,
    Error: Clone + 'static,
    State: Clone,
{
    fn parse(&self, input: Input, state: State) -> ParseResult< Input, State, Output, Error> {
        self(input, state)
    }
}



extern crate test;

#[cfg(test)]
mod tests {
    use crate::either::Either;
    use crate::parser::*;
    use crate::Parse;
    use test::Bencher;

    #[bench]
    fn bench_inline_parse(b: &mut Bencher) {
        b.iter(|| {
    fn increment(x: i32) -> i32 {
            /*println!("hello {x}");*/
            x + 1
        }

        use std::str::Chars;

        let parse_digit = vec![
            Parser::new(match_literal("1".chars(), increment)),
            Parser::new(match_literal("2".chars(), increment)),
            Parser::new(match_literal("3".chars(), increment)),
            Parser::new(match_literal("3".chars(), increment)),
            Parser::new(match_literal("4".chars(), increment)),
            Parser::new(match_literal("5".chars(), increment)),
            Parser::new(match_literal("6".chars(), increment)),
            Parser::new(match_literal("7".chars(), increment)),
            Parser::new(match_literal("8".chars(), increment)),
            Parser::new(match_literal("9".chars(), increment)),
            Parser::new(match_literal("0".chars(), increment)),
        ]
        .iter()
        .fold(
            Parser::new(match_literal("0".chars(), increment)),
            |x: Parser<Chars, i32, Chars, String>, y: &Parser<Chars, i32, Chars, String>| {
                x.or_else(y.clone()).with_error(|_, _| "error".to_string())
            },
        );
        let parse_digits = parse_digit.clone().one_or_more();

        let parse_natural_numbers = parse_digits.clone().transform(|s| {
            let mut digits = String::from("");
            for digit in s {
                digits.push_str(digit.as_str());
            }
            //    println!("digits = {:?}", digits);
            digits.parse::<i32>().unwrap()
        });

        let mut expr = ForwardRef::new();

        let factor = match_literal("(".chars(), increment)
            .triple(expr.clone(), match_literal(")".chars(), increment))
            .second()
            .or_else(parse_natural_numbers.clone())
            .with_error(|(_, _), _| "error".to_string());
        let term = factor
            .clone()
            .pair(
                match_literal("*".chars(), increment)
                    .pair(factor.clone())
                    .second()
                    .zero_or_more(),
            )
            .transform(|(x, y)| y.iter().fold(x, |a, b| a * b))
            .with_error(|_, _| "error".to_string());
        
        let expr2:Parser<Chars,i32,i32,String>  = term
            .clone()
            .pair(
                match_literal("+".chars(), increment)
                    .pair(term.clone())
                    .zero_or_more(),
            )
            .transform(|(x, y)| y.iter().fold(x, |a, (_, b)| a + b))
            .with_error(|_, _| "error".to_string());

        expr.set_parser(expr2);

        let  top_level = expr
            .clone()
            .pair(match_literal(";".chars(), increment))
            .first()
            .with_error(|_, _| "error".to_string());

        let result = top_level.parse("1+2*3+4-1/5;".chars(), 0);
    
        });
    }

    
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
