use std::rc::Rc;
use std::cell::RefCell;
use frunk::hlist::{*};
use frunk::coproduct::{*};


use crate::parser::Parser;
use crate::{Parse, ParseResult};




pub struct Tuple<'a, Input, T1, T2, Error1, Error2>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    parser: Rc<RefCell<dyn Parse<'a, Input, HList!(T1, T2), Coprod!(Error1, Error2)> + 'a>>,
}






impl<'a, Input, T1, T2, Error1, Error2> Tuple<'a, Input, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    T1: 'a,
    T2: 'a,
    Error1: Clone + 'a,
    Error2: Clone + 'a,

{
    pub fn new<P1, P2>(mut parser1: P1, mut parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1, Error1> + 'a,
        P2: Parse<'a, Input, T2, Error2> + 'a,
    {
        
        // Inject things into our Coproduct type
        Self {
            parser: Rc::new(
                RefCell::new(
                move |input| {
                let (result1, input2) = match parser1.parse(input) {
                    Ok((result1, input2)) => (result1, input2),
                    Err(error) => return Err(
                        <Coprod!(Error1, Error2)>::inject(error)),
                        //Either::Left(error)),
                };
                let (result2, rest) = match parser2.parse(input2) {
                    Ok((result2, rest)) => (result2, rest),
                    Err(error) => return Err(
                        <Coprod!(Error1, Error2)>::inject(error)),
                        //Either::Right(error)),
                };
                Ok((hlist![result1, result2], rest))
            })),
        }
    }

    pub fn get<ResultT:Clone,Index>(self) -> Parser<'a, Input, ResultT, Coprod!(Error1, Error2)> where
        HCons<T1, HCons<T2, HNil>>: frunk::hlist::Selector<ResultT, Index>
    {
        self.transform(move |mut x| (*x.get_mut::<ResultT,Index>()).clone())
    }
   


}

impl<'a, Input, T1, T2, Error1, Error2> Parse<'a, Input, HList!(T1, T2), Coprod!(Error1, Error2)>
    for Tuple<'a, Input, T1, T2, Error1, Error2>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error1: Clone + 'a,
    Error2: Clone + 'a,
{
    fn parse(&mut self, input: Input) -> ParseResult<'a, Input, HList!(T1, T2), Coprod!(Error1, Error2)> {
        (*self.parser.borrow_mut()).parse(input)
    }
}







#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{match_character, match_literal};
    use crate::Parse;

    #[test]
    fn tuple1() {
        let mut under_test = 
            match_character('a')
            .and_then(
                match_character('b'))
            .and_then(
                match_character('c'))
        ;

        let result = under_test.parse("abcdef".chars());

        println!("{:?}",result);
        match result {
            Ok((_, input)) => {
                assert_eq!(input.as_str(), "ef")
            }
            _ => panic!("failed: {:?}", result),
        }
    }
}
