use std::rc::Rc;
use std::cell::RefCell;

use crate::either::Either3;
use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Triple<'a, Input, T1, T2, T3, Error1, Error2, Error3>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<RefCell<dyn Parse<'a, Input, (T1, T2, T3), Either3<Error1, Error2, Error3>> + 'a>>,
}

impl<'a, Input, T1, T2, T3, Error1, Error2, Error3>
    Triple<'a, Input, T1, T2, T3, Error1, Error2, Error3>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    T3: 'a,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    Error3: Clone + 'a,
{
    pub fn new<P1, P2, P3>(mut parser1: P1, mut parser2: P2, mut parser3: P3) -> Self
    where
        P1: Parse<'a, Input, T1, Error1> + 'a,
        P2: Parse<'a, Input, T2, Error2> + 'a,
        P3: Parse<'a, Input, T3, Error3> + 'a,
    {
        let temp = RefCell::new(move |input| {
            let (result1, input2) = match parser1.parse(input) {
                Ok((result1, input2)) => (result1, input2),
                Err(error) => return Err(Either3::Left(error)),
            };
            let (result2, input3) = match parser2.parse(input2) {
                Ok((result2, input3)) => (result2, input3),
                Err(error) => return Err(Either3::Middle(error)),
            };
            let (result3, rest) = match parser3.parse(input3) {
                Ok((result3, rest)) => (result3, rest),
                Err(error) => return Err(Either3::Right(error)),
            };
            Ok(((result1, result2, result3), rest))
        });

        Self {
            parser: Rc::new(temp) ,
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1, Either3<Error1, Error2, Error3>> {
        self.transform(move |(first, _, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2, Either3<Error1, Error2, Error3>> {
        self.transform(move |(_, second, _)| second)
    }
    pub fn third(self) -> Parser<'a, Input, T3, Either3<Error1, Error2, Error3>> {
        self.transform(move |(_, _, third)| third)
    }
}

impl<'a, Input, T1, T2, T3, Error1, Error2, Error3>
    Parse<'a, Input, (T1, T2, T3), Either3<Error1, Error2, Error3>>
    for Triple<'a, Input, T1, T2, T3, Error1, Error2, Error3>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
    Error3: Clone + 'a,
{
    fn parse(
        &mut self,
        input: Input,
    ) -> ParseResult<'a, Input, (T1, T2, T3), Either3<Error1, Error2, Error3>> {
        (*self.parser.borrow_mut()).parse(input)
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
        let mut under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("abcdef".chars());

        match result {
            Ok((('a', 'b', 'c'), input)) => {
                assert_eq!(input.as_str(), "def")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn first_fails() {
        let mut under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("xbcdef".chars());

        match result {
            Err(Either3::Left(message)) => {
                assert_eq!(message, "expected 'a', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn second_fails() {
        let mut under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("axcdef".chars());

        match result {
            Err(Either3::Middle(message)) => {
                assert_eq!(message, "expected 'b', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn third_fails() {
        let mut under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("abxdef".chars());

        match result {
            Err(Either3::Right(message)) => {
                assert_eq!(message, "expected 'c', got 'x'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn triple_or_else_other_both_fail() {
        let mut under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        )
        .second()
        .or_else(match_character('z'));

        let result = under_test.parse("b".chars());

        match result {
            Err((Either3::Left(left), right)) => {
                assert_eq!(left, "expected 'a', got 'b'".to_string());
                assert_eq!(right, "expected 'z', got 'b'".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn failing_triple_match_literal() {
        let mut under_test = Triple::new(
            match_literal("a".chars()),
            match_literal("b".chars()),
            match_literal("c".chars()),
        );

        let result = under_test.parse("ab".chars());

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
