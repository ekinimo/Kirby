use std::rc::Rc;

use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct RepeatedParser< Input, State, T1, Error>
where
    Input: Iterator + 'static,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<dyn Parse< Input, State, Vec<T1>, Error> + 'static>,
}

impl< Input, State, T1, Error> RepeatedParser< Input, State, T1, Error>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'static,
    Error: Clone + 'static,
    State: Clone,
{



    pub fn zero_or_more<Parser>(parser: Parser) -> Self
    where
        Parser: Parse< Input, State, T1, Error> + 'static,
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
        Parser: Parse< Input, State, T1, Error> + 'static,
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

    


     // pub fn get(self,n : usize) -> Parser< Input, T1> {
     //     self.transform(move |vec| match vec.get(n) {
     //         Some(n) => {todo!()},
     //         None => {todo!()},
     //     }
     // }
}

impl< Input, State, T1, Error> Parse< Input, State, Vec<T1>, Error>
    for RepeatedParser< Input, State, T1, Error>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'static,
    State: Clone,
    T1: 'static
{
    fn parse(&self, input: Input, state: State) -> ParseResult< Input, State, Vec<T1>, Error> {
        self.parser.parse(input, state)
    }
}

impl< Input, State, T1, Error>  FnOnce<(Input,State )> for RepeatedParser< Input, State, T1, Error>
where
    Input: Clone + 'static + Iterator,
<Input as Iterator>::Item: Eq,
    Error: Clone + 'static,
    State: Clone,
    T1: 'static
    
{
    type Output = ParseResult< Input, State, Vec<T1>, Error>;
    extern "rust-call" fn call_once(self, b: (Input,State )) -> Self::Output {
        self.parse(b.0,b.1)
    }
}
