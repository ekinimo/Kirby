use std::{fmt::{Debug, write}, rc::Rc};


type ParseResult<'a,  Output> = Result<(&'a str, Output), &'a str>;

//Core
// pub trait Parse<'a, Output >   {
//     fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
// }





pub trait Parse<'a, Output : Debug + Clone  >   {
    fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
    fn transform<TransformFunction,Output2:Debug>(self,transfomfunc:TransformFunction)-> Parser<'a,Output2>
    where
        Self: Sized + 'a + Clone,
    Output : 'a + Clone,
    Output2 : 'a + Clone,
        TransformFunction: Fn(Output)->Output2 + Clone +'a ,
    {
        Parser::new(transform(self,transfomfunc))
    }
    fn predicate<PredicateFunction>(self, pred_fn: PredicateFunction) -> Parser<'a, Output>
    where
        Self: Sized + 'a + Clone,
        Output: 'a + Clone,
        PredicateFunction: Fn(&Output) -> bool + 'a + Clone,
    {
        Parser::new(predicate(self, pred_fn))
    }

    fn and_then<F, NextParser, Output2>(self, f: F) -> Parser<'a, Output2>
    where
        Self: Sized + 'a + Clone,
        Output: 'a + Debug + Clone,
        Output2: 'a + Debug+Clone,
        NextParser: Parse<'a, Output2> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a + Clone,
    {
        Parser::new(and_then(self, f))
    }
    
    fn or_else< Parser1>(self , parser2: Self) -> Parser<'a,Output>
    where
        Self: Sized + 'a + Clone,
        Output: 'a + Debug + Clone,
        Parser1: Parse<'a,Output> + Clone, 
    
    {Parser::new(or_else(self, parser2))}

}


 #[derive(Clone)]
pub struct Parser<'a, T:Clone+Debug> {
     parser:Rc<dyn Parse<'a,T>+'a>
 }


impl<'a, T: Clone+Debug> Parser<'a, T>  {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parse<'a, T> + 'a,
    { Self { parser:Rc::new(parser) } }
    
}


impl<'a , Output:Debug+Clone> Default for Parser<'a,Output> {
    fn default() -> Self {
        todo!()
    }
}

impl<'a, T:Debug+Clone> Parse<'a,T> for Parser<'a,T> {
    fn parse(&self, input: &'a str) -> ParseResult<'a,T> {
        self.parser.parse(input)
    }
}



impl<'a, Function, Output:Debug+Clone> Parse<'a,Output>  for Function
where
    Function: Fn(&'a str) -> ParseResult<Output> + Clone    ,
{
    fn parse( &self,input:  &'a str) -> ParseResult<'a,Output> {
        self(&input)
    }
}


pub fn match_literal<'a>(expected: &'a str) -> impl Parse<'a,&'a str >   {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], &input[..expected.len()])),
        _ => Err(input),
    }
}


#[test]
fn match_literal_test_1() {
    let parser = match_literal("1");
    let input = "1";
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"");result},
        
        Err(er) => {er},
    };
    
    assert_eq!(input,"1")
}

#[test]
fn match_literal_test_2() {
    let parser = match_literal("1");
    let input = "12";
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"2");result},
        
        Err(er) => {er},
    };
    
    assert_eq!(input,"12")
}

#[test]
fn match_literal_test_3() {
    let parser = match_literal("123");
    let input = "123";
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"");result},
        
        Err(er) => {er},
    };
    
    assert_eq!(input,"123")
}

#[test]
fn match_literal_test_4() {
    let parser = match_literal("123");
    let input = "12345";
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"45");result},
        
        Err(er) => {er},
    };
    
    assert_eq!(input,"12345")
}

#[test]
fn match_literal_test_5() {
    let parser = match_literal("123");
    let input = "00012345";
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"00012345");result},
        
        Err(er) => {er},
    };
    
    assert_eq!(input,"00012345")
}

pub fn one_or_more<'a, Parser1, Result1:Debug+Clone>(parser: Parser1) -> impl Parse<'a, Vec<Result1>>
where
    Parser1: Parse<'a, Result1>+Clone,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn one_or_more_test_1() {
    let input="1111111111111111";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = vec!["1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1",];
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"");result},
        
        Err(er) => {panic!()},
    };
    
    assert_eq!(should,result)
}

#[test]
fn one_or_more_test_2() {
    let input="1111111111111111222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = vec!["1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1",];
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"222");result},
        
        Err(er) => {panic!()},
    };
    
    assert_eq!(should,result)
}

#[test]
fn one_or_more_test_3() {
    let input="222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {panic!()},
        Err(er) => {er},
    };
    
    assert_eq!(input,result)
}

pub fn zero_or_more<'a, Parser1, Result1:Debug+Clone>(parser: Parser1) -> impl Parse<'a, Vec<Result1>>
where
    Parser1: Parse<'a, Result1> + Clone,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn zero_or_more_test_1() {
    let input="1111111111111111";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = zero_or_more(parser1);
    let should = vec!["1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1",];
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"");result},
        
        Err(er) => {panic!()},
    };
    
    assert_eq!(should,result)
}

#[test]
fn zero_or_more_test_2() {
    let input="1111111111111111222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = vec!["1","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1",];
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {assert_eq!(remaining,"222");result},
        
        Err(er) => {panic!()},
    };
    
    assert_eq!(should,result)
}

#[test]
fn zero_or_more_test_3() {
    let input="222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let result = match parser.parse(input) {
        Ok((remaining,result)) => {panic!()},
        Err(er) => {er},
    };
    
    assert_eq!(input,result)
}

pub fn transform<'a,Parser,TransformFunction, Output1:Debug+Clone,Output2:Debug+Clone>(parser:Parser,transfomfunc:TransformFunction)-> impl Parse<'a,Output2>
where
    Parser: Parse<'a,Output1> + 'a + Clone,
    TransformFunction: Fn(Output1)->Output2 + Clone  ,
{
    move |input| {
        parser.parse(input).map(|(rest,result)|(rest,transfomfunc(result)))

    }
}



#[test]
fn transform_0() {
    let input="222";
    let init = match_literal("2");
    let parser1 = Parser::new(init);
    let parser = Parser::new(one_or_more(parser1)).transform(|s| {
        let mut digits =String::from("");
        for digit in s{
            digits.push_str(digit);
        }
        digits.parse::<i32>().unwrap()
    });
    let result = parser.parse(input);
    let expected = Ok(("",222));
    assert_eq!(expected,result)
}




pub fn predicate<'a, Parser1, Result1:Debug+Clone, PredicateFunction>(parser: Parser1, predicate: PredicateFunction) -> impl Parse<'a, Result1>
where
    Parser1: Parse<'a, Result1> + Clone,
    PredicateFunction: Fn(&Result1) -> bool + Clone,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}


#[test]
fn predicate_0() {
    let input="222";
    let init = match_literal("2");
    let parser1 = Parser::new(init);
    let parser = Parser::new(one_or_more(parser1)).predicate(|s| {
        let mut digits =String::from("");
        for digit in s{
            digits.push_str(*digit);
        }
        match digits.parse::<i32>() {
            Ok(_) => {true},
            Err(_) => {false},
        }
    });
    let result = parser.parse(input);
    let expected = Ok(("",vec!["2","2","2"]));
    assert_eq!(expected,result)
}


fn and_then<'a, P, F, A:Debug+Clone, B:Debug+Clone, NextP>(parser: P, f: F) -> impl Parse<'a, B>
where
    P: Parse<'a, A> + Clone,
    NextP: Parse<'a, B>+ Clone,
    F: Fn(A) -> NextP + Clone,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}


#[test]
fn and_then_0() {
    let input="223";
    let init = match_literal("2");
    let parser1 = Parser::new(init).and_then(|x|{
        Parser::new(match_literal(x.clone()))
    });
    
    let result = parser1.parse(input);
    let expected = Ok(("3","2"));
    assert_eq!(expected,result)
}


pub fn or_else< 'a,Parser1,  Result1:Debug+Clone>(parser1: Parser1 , parser2: Parser1) -> impl Parse<'a, Result1>
where
    Parser1: Parse<'a,Result1>+Clone, 

{
    move |input|{
        match parser1.parse(input) {
            res @ Ok(_) => {return res},
            Err(_) => {parser2.parse(input)},
        }
    }

}

#[test]
fn or_else_0() {
    let input="2123";
    let init1 = match_literal("1");
    let init2 = match_literal("2");

    let parser2 = Parser::new(init2);
    let parser1 = Parser::new(init1).or_else::<Parser<'_,&str>>(parser2);
    

    let result = parser1.parse(input);
    let expected = Ok(("123","2"));
    assert_eq!(expected,result);

    let input2 = "21";
    let result = parser1.parse(input2);
    let expected = Ok(("1","2"));
    assert_eq!(expected,result);

    let input3 = "12";
    let result = parser1.parse(input3);
    let expected = Ok(("2","1"));
    assert_eq!(expected,result)


}

fn any(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}


//todo testsPa
//     pair/left/right                this one needs some massaging its not a member func of Parser
//                                    i dont like it as a standalone function
//     one_of, between, starts_with / ends_with  ---
//     digit /float /integer /id / whitespace / newline
//     somehow put constructors into Parser object like Parser::any(..) etc. 


pub fn pair< 'a,Parser1, Parser2, Result1:Debug+Clone, Result2:Debug+Clone>(parser1: Parser1, parser2: Parser2) -> impl Parse<'a,Rc<(Result1,Result2)>>
 where
    Parser1: Parse<'a,Result1> + Clone + 'a,
     Parser2: Parse<'a,Result2> + Clone + 'a,
 {
     move |input|{
         match parser1.parse(input) {
             Ok((rest,left_result)) => {
                 println!("Ok Ok   :{:?} {:?}",rest,left_result);
                 match parser2.parse(rest) {
                  
                     Ok((rest2,right_result)) => {
                         println!("Ok Ok  :{:?} {:?}",rest2,right_result);
                         Ok((rest2,Rc::new((left_result,right_result))))},
                     Err(err) => {
                         println!("Ok Err :{:?}",err);
                         Err(err)},
                 }
             },
             Err(er) => {Err(er)},
         }
     }

}

#[test]
fn pair_0() {
    let input="21";
    let init1 = match_literal("2");
    let init2 = match_literal("1");
    let pair_parse = pair(Parser::new(init1), Parser::new(init2));
    
    let result = pair_parse.parse(input);
    let expected = Ok(("",Rc::new(("2","1"))));
    assert_eq!(expected,result)

    // let input2 = "21";
    // let result = parser1.parse(input2);
    // let expected = Ok(("1","2"));
    // assert_eq!(expected,result);

    // let input3 = "12";
    // let result = parser1.parse(input3);
    // let expected = Ok(("2","1"));
    // assert_eq!(expected,result)


}

// pub fn first<'a, Parser1, Parser2, Result1, Result2>(parser1: Parser1, parser2: Parser2) -> impl Parse<'a, Result1>
// where
//     Parser1: Parse<'a, Result1> + Clone,
//     Parser2: Parse<'a, Result2> + Clone,  Result1: Debug + Clone, Result2: Debug + Clone
// {
//     transform(pair(parser1, parser2), |(first, _)| first)
// }

// pub fn second<'a, Parser1, Parser2, Result1, Result2>(parser1: Parser1, parser2: Parser2) -> impl Parse<'a, Result2>
// where
//     Parser1: Parse<'a, Result1>+Clone,
//     Parser2: Parse<'a, Result2>+Clone, Result1: Debug+Clone, Result2: Debug+Clone
// {
    
//     transform((parser1, parser2), |(_, second)| second)
// }






