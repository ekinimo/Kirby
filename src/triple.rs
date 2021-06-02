use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Triple<'a, Input, T1, T2, T3>
where
    Input: Debug + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
    T2: Debug + Clone,
    T3: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, (T1, T2, T3)> + 'a>,
}

impl<'a, Input, T1, T2, T3> Triple<'a, Input, T1, T2, T3>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    T2: Debug + Clone + 'a,
    T3: Debug + Clone + 'a,
{
    pub fn new<P1, P2, P3>(parser1: P1, parser2: P2, parser3: P3) -> Self
    where
        P1: Parse<'a, Input, T1> + 'a,
        P2: Parse<'a, Input, T2> + 'a,
        P3: Parse<'a, Input, T3> + 'a,
    {
        Self {
            parser: Rc::new(move |input| match parser1.parse(input) {
                Ok((result1, input2)) => match parser2.parse(input2) {
                    Ok((result2, input3)) => {
                        match parser3.parse(input3) {
                            Ok((result3,rest)) => {Ok(((result1,result2,result3),rest))},
                            Err(mut err) => {
                                err.push_str("Parser Combinator : Triple parser, third parser failed \n");
                                Err(err)
                            }
                        }
                    },
                    Err(mut err) => {
                        err.push_str("Parser Combinator : Triple parser, second parser failed \n");
                        Err(err)
                    }
                },
                Err(mut err) => {
                    err.push_str("Parser Combinator : Triple parser, first parser failed \n");
                    Err(err)
                }
            }),
        }

    }

    pub fn first(self) -> Parser<'a, Input, T1> {
        self.transform(move |(first, _, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T2> {
        self.transform(move |(_, second, _)| second)
    }
    pub fn third(self) -> Parser<'a, Input, T3> {
        self.transform(move |(_, _, third)| third)
    }
}

impl<'a, Input, T1, T2, T3> Parse<'a, Input, (T1, T2, T3)> for Triple<'a, Input, T1, T2, T3>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
    T3: Debug + Clone,
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2, T3)> {
        self.parser.parse(input)
    }
}
