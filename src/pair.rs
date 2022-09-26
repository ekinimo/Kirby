use std::fmt::Debug;
use std::rc::Rc;

use crate::either::Either;
use crate::parser::Parser;
use crate::{Parse, ParseResult};

#[derive(Clone)]
pub struct Pair< Input, State, T1, T2, Error1, Error2>
where
    Input: 'static + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<dyn Parse< Input, State, (T1, T2), Either<Error1, Error2>> + 'static>,
}

impl< Input, State, T1, T2, Error1, Error2> Pair< Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'static,
    T2: 'static,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    State: Clone + 'static,
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse< Input, State, T1, Error1> + 'static,
        P2: Parse< Input, State, T2, Error2> + 'static,
    {
        Self {
            parser: Rc::new(move |input, state| {
                let (result1, state, input2) = match parser1.parse(input, state) {
                    Ok((result1, state, input2)) => (result1, state, input2),
                    Err(error) => return Err(Either::Left(error)),
                };
                let (result2, state, rest) = match parser2.parse(input2, state) {
                    Ok((result2, state, rest)) => (result2, state, rest),
                    Err(error) => return Err(Either::Right(error)),
                };
                Ok(((result1, result2), state, rest))
            }),
        }
    }

    pub fn first(self) -> Parser< Input, State, T1, Either<Error1, Error2>> {
        self.transform(move |(first, _)| first)
    }

    pub fn second(self) -> Parser< Input, State, T2, Either<Error1, Error2>> {
        self.transform(move |(_, second)| second)
    }
}

impl< Input, State, T1, T2, Error1, Error2>
    Parse< Input, State, (T1, T2), Either<Error1, Error2>>
    for Pair< Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    State: Clone,
T1 : 'static,
T2 : 'static,

{
    fn parse(
        &self,
        input: Input,
        state: State,
    ) -> ParseResult< Input, State, (T1, T2), Either<Error1, Error2>> {
        self.parser.parse(input, state)
    }
}


 impl< Input, State, T1:'static, T2:'static, Error1, Error2>    FnOnce<(Input,State )> for Pair< Input, State, T1, T2, Error1, Error2>
 where
     Input: Clone + 'static + Iterator,
 <Input as Iterator>::Item: Eq,
     Error1: Clone + 'static,
     Error2: Clone + 'static,
     State: Clone,

 {
     type Output = ParseResult< Input, State, (T1, T2), Either<Error1, Error2>>;
     extern "rust-call" fn call_once(self, b: (Input,State )) -> ParseResult< Input, State, (T1, T2), Either<Error1, Error2>> {
         self.parse(b.0,b.1)
     }
 }
   


impl< Input, State, T1, T2, Error1, Error2,A,B>
    Parse< Input, State, (T1, T2), Either<Error1, Error2>>
    for (A,B)
where
    Input: Clone + 'static + Iterator,
<Input as Iterator>::Item: Eq,
    Error1: Clone + 'static,
    Error2: Clone + 'static,
    State: Clone,
    T1 : 'static,
    T2 : 'static,
    A:Parse< Input, State, T1, Error1>,
    B:Parse< Input, State, T2, Error2>
{
    fn parse(
        &self,
        input: Input,
        state: State,
    ) -> ParseResult< Input, State, (T1, T2), Either<Error1, Error2>> {
        let (result1, state, input2) = match self.0.parse(input, state) {
            Ok((result1, state, input2)) => (result1, state, input2),
            Err(error) => return Err(Either::Left(error)),
        };
        let (result2, state, rest) = match self.1.parse(input2, state) {
            Ok((result2, state, rest)) => (result2, state, rest),
            Err(error) => return Err(Either::Right(error)),
        };
        Ok(((result1, result2), state, rest))
    }
}

impl< Input, State, T1, T2, Error1, Error2> Debug
    for Pair< Input, State, T1, T2, Error1, Error2>
where
    Input: Clone + 'static + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'static,
    T2: 'static,
{
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
