use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Triple<'a, Input, T1, T2, T3, Error>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<dyn Parse<'a, Input, (T1, T2, T3), Error> + 'a>,
}

impl<'a, Input, T1, T2, T3, Error> Triple<'a, Input, T1, T2, T3, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    T3: 'a,
    Error: Clone + 'a,
{
    pub fn new<P1, P2, P3>(parser1: P1, parser2: P2, parser3: P3) -> Self
    where
        P1: Parse<'a, Input, T1, Error> + 'a,
        P2: Parse<'a, Input, T2, Error> + 'a,
        P3: Parse<'a, Input, T3, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |input| {
                let (result1, input2) = parser1.parse(input)?;
                let (result2, input3) = parser2.parse(input2)?;
                let (result3, rest) = parser3.parse(input3)?;
                Ok(((result1, result2, result3), rest))
            }),
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1, Error> {
        self.transform(move |(first, _, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2, Error> {
        self.transform(move |(_, second, _)| second)
    }
    pub fn third(self) -> Parser<'a, Input, T3, Error> {
        self.transform(move |(_, _, third)| third)
    }
}

impl<'a, Input, T1, T2, T3, Error> Parse<'a, Input, (T1, T2, T3), Error>
    for Triple<'a, Input, T1, T2, T3, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2, T3), Error> {
        self.parser.parse(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::match_character;
    use crate::triple::Triple;
    use crate::Parse;

    #[test]
    fn all_parsers_succeed() {
        let under_test = Triple::new(
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
        let under_test = Triple::new(
            match_character('a'),
            match_character('b'),
            match_character('c'),
        );

        let result = under_test.parse("xbcdef".chars());

        match result {
            Err(message) => {
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

        let result = under_test.parse("axcdef".chars());

        match result {
            Err(message) => {
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

        let result = under_test.parse("abxdef".chars());

        match result {
            Err(message) => {
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

        let result = under_test.parse("b".chars());

        match result {
            Err((left, right)) => {
                assert_eq!(left, "expected 'a', got 'b'".to_string());
                assert_eq!(right, "expected 'z', got 'b'".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }
}
