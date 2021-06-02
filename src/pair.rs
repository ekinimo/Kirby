use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::{Parse, ParseResult, Parser};

#[derive(Clone)]
pub struct Pair<'a, Input, T1, T2>
where
    Input: Debug + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, (T1, T2)> + 'a>,
}

impl<'a, Input, T1, T2> Pair<'a, Input, T1, T2>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1> + 'a,
        P2: Parse<'a, Input, T2> + 'a,
    {
        Self {
            parser: Rc::new(move |input| {
                let (result1, input2) = parser1.parse(input)?;
                let (result2, rest) = parser2.parse(input2)?;
                Ok(((result1, result2), rest))
            }),
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1> {
        self.transform(move |(first, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2> {
        self.transform(move |(_, second)| second)
    }
}

impl<'a, Input, T1, T2> Parse<'a, Input, (T1, T2)> for Pair<'a, Input, T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2)> {
        self.parser.parse(input)
    }
}

impl<'a, Input, T1, T2> Debug for Pair<'a, Input, T1, T2>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
