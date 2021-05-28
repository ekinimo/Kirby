use std::{fmt::Debug, rc::Rc, str::Chars};

pub struct Parser2<Input, Output>
where
    Input: Iterator<Item: Eq>,
{
    parser: Rc<dyn Parse2<Input, Output>>,
}
pub trait Parse2<Input, Output : Eq>
where
    Input: Iterator<Item=Output>,
{
    fn parse(&self, input: Input) -> Result<(Output, Input), String>;


    fn predicate<PredicateFunction>(self, pred_fn: PredicateFunction) -> Parser2<Input, Output>
    where
        Self: Sized + 'static,
        PredicateFunction: Fn(&Output) -> bool + 'static,
    {
        Parser2::new(move |input: Input| {
            
            if let Ok((value, next_input)) = self.parse(input) {
                if pred_fn(&value) {
                    return Ok((value, next_input));
                }
            }
            Err("error".to_string())
        })
    }
}

impl<Input: Iterator<Item =Output>, Output : Eq, P: Parse2<Input, Output>> Parser2<Input, Output> {
    pub fn new(parser: P) -> Self
    {
        Self {
            parser: Rc::from(parser),
        }
    }
     pub fn any()->Self{
         Self {
             parser:Rc::from(|mut x: Input| {match x.next() {
                 Some(res) => Ok((res,x)),
                 None  => Err("error".to_string())
             }})}
     }
}

impl<Input: Iterator<Item: Eq> +  Iterator<Item = Output>, Output: Eq> Parse2<Input, Output>
    for Parser2<Input, Output>
where
    
{
    fn parse(&self, input: Input) -> Result<(Output, Input), String> {
        self.parser.parse(input)
    }
}

impl<Function ,Input: Iterator<Item: Eq> + Iterator<Item = Output>, Output : Eq> Parse2<Input, Output>
    for Function
where
    Function : Fn(Input)->Result<(Output, Input), String>
{
    fn parse(&self, input: Input) -> Result<(Output, Input), String> {
        self(input)
    }
}

