use std::rc::Rc;

use crate::either::Either3;
use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Triple< Input, State, T1, T2, T3, Error1, Error2, Error3>
where
    Input: 'static + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<dyn Parse< Input, State, (T1, T2, T3), Either3<Error1, Error2, Error3>> + 'static>,
}

impl< Input, State, T1, T2, T3, Error1, Error2, Error3>
    Triple< Input, State, T1, T2, T3, Error1, Error2, Error3>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'static,
    T2: 'static,
    T3: 'static,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    Error3: Clone + 'static,
    State: Clone + 'static,
{
    pub fn new<P1, P2, P3>(parser1: P1, parser2: P2, parser3: P3) -> Self
    where
        P1: Parse< Input, State, T1, Error1> + 'static,
        P2: Parse< Input, State, T2, Error2> + 'static,
        P3: Parse< Input, State, T3, Error3> + 'static,
    {
        Self {
            parser: Rc::new(move |input, state| {
                let (result1, state, input2) = match parser1.parse(input, state) {
                    Ok((result1, state, input2)) => (result1, state, input2),
                    Err(error) => return Err(Either3::Left(error)),
                };
                let (result2, state, input3) = match parser2.parse(input2, state) {
                    Ok((result2, state, input3)) => (result2, state, input3),
                    Err(error) => return Err(Either3::Middle(error)),
                };
                let (result3, state, rest) = match parser3.parse(input3, state) {
                    Ok((result3, state, rest)) => (result3, state, rest),
                    Err(error) => return Err(Either3::Right(error)),
                };
                Ok(((result1, result2, result3), state, rest))
            }),
        }
    }

    pub fn first(self) -> Parser< Input, State, T1, Either3<Error1, Error2, Error3>> {
        self.transform(move |(first, _, _)| first)
    }

    pub fn second(self) -> Parser< Input, State, T2, Either3<Error1, Error2, Error3>> {
        self.transform(move |(_, second, _)| second)
    }
    pub fn third(self) -> Parser< Input, State, T3, Either3<Error1, Error2, Error3>> {
        self.transform(move |(_, _, third)| third)
    }
}

impl< Input, State, T1, T2, T3, Error1, Error2, Error3>
    Parse< Input, State, (T1, T2, T3), Either3<Error1, Error2, Error3>>
    for Triple< Input, State, T1, T2, T3, Error1, Error2, Error3>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    Error3: Clone + 'static,
    State: Clone,
    T1: 'static,
    T2: 'static,
    T3: 'static,

{
    fn parse(
        &self,
        input: Input,
        state: State,
    ) -> ParseResult< Input, State, (T1, T2, T3), Either3<Error1, Error2, Error3>> {
        self.parser.parse(input, state)
    }
}

impl< Input, State, T1, T2, T3, Error1, Error2, Error3>

    FnOnce<(Input,State )>     for Triple< Input, State, T1, T2, T3, Error1, Error2, Error3>
where
    Input: Clone + 'static + Iterator,
<Input as Iterator>::Item: Eq,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    Error3: Clone + 'static,
    State: Clone,
    T1: 'static,
    T2: 'static,
    T3: 'static,

        
{
    type Output = ParseResult< Input, State, (T1, T2, T3), Either3<Error1, Error2, Error3>>;
    extern "rust-call" fn call_once(self, b: (Input,State )) -> Self::Output {
        self.parse(b.0,b.1)
    }
}









#[cfg(test)]
mod tests {
    use crate::either::Either3;
    use crate::parser::{match_character, match_literal};
    use crate::triple::Triple;
    use crate::Parse;

    #[test]
    fn all_parsers_succeed() {
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("abcdef".chars(), ());

        match result {
            Ok((('a', 'b', 'c'), (), input)) => {
                assert_eq!(input.as_str(), "def")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn first_fails() {
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("xbcdef".chars(), ());

        match result {
            Err(Either3::Left(message)) => {
                assert_eq!(message, "expected 'a', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn second_fails() {
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("axcdef".chars(), ());

        match result {
            Err(Either3::Middle(message)) => {
                assert_eq!(message, "expected 'b', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn third_fails() {
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("abxdef".chars(), ());

        match result {
            Err(Either3::Right(message)) => {
                assert_eq!(message, "expected 'c', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn triple_or_else_other_both_fail() {
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        )
        .second()
        .or_else(match_character('z'));

        let result = under_test.parse("b".chars(), ());

        match result {
            Err((Either3::Left(left), right)) => {
                assert_eq!(left, "expected 'a', got 'b'".to_string());
                assert_eq!(right, "expected 'z', got 'b'".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }
    fn id<T>(x: T) -> T {
        x
    }

    #[test]
    fn failing_triple_match_literal() {
        let under_test = Triple::new(
            match_literal("a".chars(), id),
            match_literal("b".chars(), id),
            match_literal("c".chars(), id),
        );

        let result = under_test.parse("ab".chars(), ());

        match result {
            Err(Either3::Right(message)) => {
                assert_eq!(
                    message,
                    "match_literal failed: expected Chars(['c']), got Chars([])".to_string()
                );
            }
            _ => panic!("failed: {:?}", result),
        }
    }
}
