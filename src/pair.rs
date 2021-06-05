use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Pair<'a, Input, T1, T2, Error>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, (T1, T2), Error> + 'a>,
}

impl<'a, Input, T1, T2, Error> Pair<'a, Input, T1, T2, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
    Error: Clone + 'a,
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1, Error> + 'a,
        P2: Parse<'a, Input, T2, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |input| {
                let (result1, input2) = parser1.parse(input)?;
                let (result2, rest) = parser2.parse(input2)?;
                Ok(((result1, result2), rest))
            }),
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1, Error> {
        self.transform(move |(first, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2, Error> {
        self.transform(move |(_, second)| second)
    }
}

impl<'a, Input, T1, T2, Error> Parse<'a, Input, (T1, T2), Error> for Pair<'a, Input, T1, T2, Error>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2), Error> {
        self.parser.parse(input)
    }
}

impl<'a, Input, T1, T2, Error> Debug for Pair<'a, Input, T1, T2, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
