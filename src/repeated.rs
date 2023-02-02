use std::rc::Rc;

use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct RepeatedParser<'a, Input, State, T1, Error>
where
    Input: IntoIterator + 'a,
    <Input as IntoIterator>::Item: Eq,
{
    parser: Rc<dyn Parse<'a, Input, State, Vec<T1>, Error> + 'a>,
}

impl<'a, Input, State, T1, Error> RepeatedParser<'a, Input, State, T1, Error>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    T1: 'a,
    Error: Clone + 'a,
    State: Clone,
{



    pub fn zero_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, State, T1, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |mut input: Input, mut state: State| {
                let mut results: Vec<T1> = Vec::new();

                loop {
                    let res = parser.parse(input.clone(), state.clone());
                    match res {
                        Ok((result, state2, input2)) => {
                            results.push(result);
                            state = state2;
                            input = input2
                        }
                        _ => {
                            break;
                        }
                    }
                }

                Ok((results, state, input))
            }),
        }
    }

    pub fn one_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse<'a, Input, State, T1, Error> + 'a,
    {
        Self {
            parser: Rc::new(move |input: Input, state: State| {
                let (first_item, mut state, mut input) = parser.parse(input, state)?;
                let mut results = vec![first_item];

                loop {
                    let res = parser.parse(input.clone(), state.clone());
                    match res {
                        Ok((result, state2, input2)) => {
                            results.push(result);
                            state = state2;
                            input = input2
                        }
                        _ => {
                            break;
                        }
                    }
                }

                Ok((results, state, input))
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

impl<'a, Input, State, T1, Error> Parse<'a, Input, State, Vec<T1>, Error>
    for RepeatedParser<'a, Input, State, T1, Error>
where
    Input: Clone + 'a + IntoIterator,
    <Input as IntoIterator>::Item: Eq,
    Error: Clone + 'a,
    State: Clone,
{
    fn parse(&self, input: Input, state: State) -> ParseResult<'a, Input, State, Vec<T1>, Error> {
        self.parser.parse(input, state)
    }
}
