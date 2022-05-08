use std::rc::Rc;
use std::cell::RefCell;

use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct RepeatedParser<'a, Input, T1, Error>
where
    Input: Iterator + 'a,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<RefCell<dyn Parse<'a, Input, Vec<T1>, Error> + 'a>>,
}

impl<'a, Input, T1, Error> RepeatedParser<'a, Input, T1, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    Error: Clone + 'a,
{
    pub fn zero_or_more<Parser>(mut parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, T1, Error> + 'a,
    {
        Self {
            parser: Rc::new(
                RefCell::new(
                move |mut input: Input| {
                let mut result = Vec::new();

                while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
                    input = next_input;
                    result.push(next_item);
                }

                Ok((result, input))
            }))
        }
    }

    pub fn one_or_more<Parser>(mut parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, T1, Error> + 'a,
    {
        Self {
            parser: Rc::new(RefCell::new (move |input: Input| {
                let (first_item, mut input) = parser.parse(input)?;
                let mut result = vec![first_item];

                while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
                    input = next_input;
                    result.push(next_item);
                }

                Ok((result, input))
            })),
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
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
{
    fn parse(&mut self, input: Input) -> ParseResult<'a, Input, Vec<T1>, Error> {
        (*self.parser.borrow_mut()).parse(input)
    }
}
