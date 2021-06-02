use std::fmt::Debug;
use std::rc::Rc;

use crate::parser::{Parse, ParseResult};

#[derive(Clone)]
pub struct RepeatedParser<'a, Input, T1>
where
    Input: Debug + Iterator + 'a,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone,
{
    parser: Rc<dyn Parse<'a, Input, Vec<T1>> + 'a>,
}

impl<'a, Input, T1> RepeatedParser<'a, Input, T1>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
    T1: Debug + Clone + 'a,
{
    pub fn zero_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, T1> + 'a,
        Input: Debug + Clone + 'a + Iterator,
        <Input as Iterator>::Item: Eq + Debug + Clone,
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
        Parser: Parse<'a, Input, T1> + 'a,
        Input: Debug + Clone + 'a + Iterator,
        <Input as Iterator>::Item: Eq + Debug + Clone,
    {
        Self {
            parser: Rc::new(move |mut input: Input| {
                let mut result = Vec::new();

                if let Ok((first_item, next_input)) = parser.parse(input.clone()) {
                    input = next_input;
                    result.push(first_item);
                } else {
                    return Err("error".to_string());
                }

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

impl<'a, Input, T1> Parse<'a, Input, Vec<T1>> for RepeatedParser<'a, Input, T1>
where
    T1: Debug + Clone,

    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq + Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Vec<T1>> {
        self.parser.parse(input)
    }
}
