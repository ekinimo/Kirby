use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Triple<'a, Input, T1, T2, T3, Error>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
    T2: Debug + Clone,
    T3: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, (T1, T2, T3), Error> + 'a>,
}

impl<'a, Input, T1, T2, T3, Error> Triple<'a, Input, T1, T2, T3, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
    T3: Debug + Clone + 'a,
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
    T1: Debug + Clone,
    T2: Debug + Clone,
    T3: Debug + Clone,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2, T3), Error> {
        self.parser.parse(input)
    }
}
