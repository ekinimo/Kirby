use std::fmt::Debug;
use std::rc::Rc;

use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct RepeatedParser<'a, Input, T1, Error>
where
    Input: Iterator + 'a,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, Vec<T1>, Error> + 'a>,
}

impl<'a, Input, T1, Error> RepeatedParser<'a, Input, T1, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
    Error: Clone + 'a,
{
    pub fn zero_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, T1, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |mut input: Input| {
                let mut result = Vec::new();

                while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
                    input = next_input;
                    result.push(next_item);
                }

                Ok((result, input))
            }),
        }
    }

    pub fn one_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, T1, Error> + 'a,
        Input: Clone + 'a + Iterator,
        <Input as Iterator>::Item: Eq + Debug + Clone,
    {
        Self {
            parser: Rc::new(move |input: Input| {
                let (first_item, mut input) = parser.parse(input.clone())?;
                let mut result = vec![first_item];

                while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
                    input = next_input;
                    result.push(next_item);
                }

                Ok((result, input))
            }),
        }
    }

    // pub fn get(self,n : usize) -> Parser<'a, Input, T1> {
    //     self.transform(move |vec| match vec.get(n) {
    //         Some(n) => {todo!()},
    //         None => {todo!()},
    //     }
    // }
}

impl<'a, Input, T1, Error> Parse<'a, Input, Vec<T1>, Error> for RepeatedParser<'a, Input, T1, Error>
where
    T1: Debug + Clone,
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Vec<T1>, Error> {
        self.parser.parse(input)
    }
}
