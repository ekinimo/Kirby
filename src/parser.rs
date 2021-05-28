use std::{fmt::Debug, rc::Rc};

//type ParseResult<'a, Input, Output> = (Result<Output, &'a str>, Input);
type ParseResult<'a, Input, Output> = Result<(Output, Input), &'a str>;
//Core
// pub trait Parse<'a, Output >   {
//     fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
// }


#[derive(Clone)]
pub struct Parser<'a, Input: Debug + 'a, T: Debug> {
    parser: Rc<dyn Parse<'a, Input, T> + 'a>,
}

#[derive(Clone)]
pub struct Pair<'a, Input: Debug + 'a, T1: Debug, T2: Debug> {
    parser: Rc<dyn Parse<'a, Input, (T1, T2)> + 'a>,
}

#[derive(Clone)]
pub struct Either<'a, Input: Debug + 'a, T1: Debug + Clone, T2: Debug + Clone> {
    parser: Rc<dyn Parse<'a, Input, EitherType<T1, T2>> + 'a>,
}

#[derive(Clone, Debug)]
pub enum EitherType<T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    Left(T1),
    Right(T2),
}





pub trait Parse<'a, Input: Debug + Clone + 'a, Output: Debug + Clone> {
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output>;
    fn transform<TransformFunction, Output2: Debug>(
        self,
        transfomfunc: TransformFunction,
    ) -> Parser<'a, Input, Output2>
    where
        Self: Sized + 'a,
        Output: 'a + Clone,
        Output2: 'a + Clone,
        TransformFunction: Fn(Output) -> Output2 + 'a,
    {
        Parser::new(move |input| {
            self.parse(input)
                .map(|(result, rest)| (transfomfunc(result), rest))
        })
    }
    fn predicate<PredicateFunction>(self, pred_fn: PredicateFunction) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(move |input: Input| {
            let input_ = input.clone();
            if let Ok((value, next_input)) = self.parse(input.clone()) {
                if pred_fn(&value) {
                    return Ok((value, next_input));
                }
            }
            Err("error")
        })
    }

    fn new_parser_from_parse_result<F, NextParser, Output2>(self, f: F) -> Parser<'a, Input, Output2>
    where
        Self: Sized + 'a,
        Output: 'a + Debug + Clone,
        Output2: 'a + Debug + Clone,
        NextParser: Parse<'a, Input, Output2> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a,
    {
        Parser::new(

            move |input| match self.parse(input) {
                Ok((result, next_input)) => f(result).parse(next_input),
                Err(err) => Err(err),
            })

    }

    fn or_else<Parser1>(self, parser2: Self) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
        Parser1: Parse<'a, Input, Output>,
    {
        Parser::new(
            move |input: Input| match self.parse(input.clone()) {
                res @ Ok(_) => res,
                Err(_) => parser2.parse(input),
            })
    }

    fn pair<Parser1, Output2>(self, parser2: Parser1) -> Pair<'a, Input, Output, Output2>
    where
        Self: Sized + 'a,
        Output: Debug + Clone + 'a,
        Output2: Debug + Clone + 'a,
        Parser1: Parse<'a, Input, Output2> + 'a,
    {
        Pair::new(self, parser2)
    }

    fn either<Parser1, Output2>(self, parser2: Parser1) -> Either<'a, Input, Output, Output2>
    where
        Self: Sized + 'a,
        Output: Debug + Clone + 'a,
        Output2: Debug + Clone + 'a,
        Parser1: Parse<'a, Input, Output2> + 'a,
    {
        Either::new(self, parser2)
    }

    fn zero_or_more(self) -> Parser<'a, Input, Vec<Output>>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
    {
        //todo!();
        Parser::new(zero_or_more(self))
    }
    fn one_or_more(self) -> Parser<'a, Input, Vec<Output>>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
    {
        //todo!();
        Parser::new(zero_or_more(self))
    }
}


pub fn one_or_more<'a, Parser1, Input: Debug + Clone + 'a, Result1: Debug + Clone + 'a>(
    parser: Parser1,
) -> impl Parse<'a, Input, Vec<Result1>>
where
    Parser1: Parse<'a, Input, Result1>,
{
    move |mut input: Input| {
        let mut result = Vec::new();

        if let Ok((first_item, next_input)) = parser.parse(input.clone()) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err("error");
        }

        while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
            input = next_input;
            result.push(next_item);
        }

        Ok((result, input))
    }
}

pub fn zero_or_more<'a, Parser1, Input: Debug + Clone + 'a, Result1: Debug + Clone + 'a>(
    parser: Parser1,
) -> impl Parse<'a, Input, Vec<Result1>>
where
    Parser1: Parse<'a, Input, Result1>,
{
    move |mut input: Input| {
        let mut result = Vec::new();

        while let Ok((next_item, next_input)) = parser.parse(input.clone()) {
            input = next_input;
            result.push(next_item);
        }

        Ok((result, input))
    }
}


impl<T1, T2> EitherType<T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    /// Returns `true` if the either_type is [`Left`].
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(..))
    }

    pub fn as_left(&self) -> Option<T1> {
        if let Self::Left(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    /// Returns `true` if the either_type is [`Right`].
    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(..))
    }

    pub fn as_right(&self) -> Option<T2> {
        if let Self::Right(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    pub fn try_into_left(self) -> Result<T1, Self> {
        if let Self::Left(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_right(self) -> Result<T2, Self> {
        if let Self::Right(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}


impl<'a, Input: Debug + Clone + 'a, T1: Debug + Clone + 'a, T2: Debug + Clone + 'a> Debug
    for Pair<'a, Input, T1, T2>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl<'a, Input: Debug + Clone + 'a, T1: Debug + Clone + 'a, T2: Debug + Clone + 'a> Debug
    for Either<'a, Input, T1, T2>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a, Input: Debug + Clone + 'a, T1: Debug + Clone + 'a, T2: Debug + Clone + 'a>
    Pair<'a, Input, T1, T2>
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1> + 'a,
        P2: Parse<'a, Input, T2> + 'a,
    {
        Self {
            parser: Rc::new(move |input| match parser1.parse(input) {
                Ok((left_result, rest)) => {
                    println!("Ok Ok   :{:?} {:?}", rest, left_result);
                    match parser2.parse(rest) {
                        Ok((right_result, rest2)) => {
                            println!("Ok Ok  :{:?} {:?}", rest2, right_result);
                            Ok(((left_result, right_result), rest2))
                        }
                        Err(err) => {
                            println!("Ok Err :{:?}", err);
                            Err(err)
                        }
                    }
                }
                Err(er) => Err(er),
            }),
        }
    }

    pub fn first(self) -> Parser<'a, Input, T1> {
        self.transform(|(first, _)| first)
    }

    pub fn second(self) -> Parser<'a, Input, T1> {
        self.transform(|(first, _)| first)
    }
}

impl<'a, Input: Debug + Clone + 'a, T1: Debug + Clone + 'a, T2: Debug + Clone + 'a>
    Either<'a, Input, T1, T2>
{
    pub fn new<P1, P2>(parser1: P1, parser2: P2) -> Self
    where
        P1: Parse<'a, Input, T1> + 'a,
        P2: Parse<'a, Input, T2> + 'a,
    {
        Self {
            parser: Rc::new(move |input: Input| match parser1.parse(input.clone()) {
                Ok((left_result, rest)) => Ok((EitherType::Left(left_result), rest)),
                Err(_) => match parser2.parse(input) {
                    Ok((right_result, remaining)) => {
                        Ok((EitherType::Right(right_result), remaining))
                    }
                    Err(_) => Err("error"),
                },
            }),
        }
    }
    pub fn try_left(self) -> Parser<'a, Input, Result<T1, EitherType<T1, T2>>> {
        self.transform(|x| x.try_into_left())
    }

    pub fn try_right(self) -> Parser<'a, Input, Result<T2, EitherType<T1, T2>>> {
        self.transform(|x| x.try_into_right())
    }
    pub fn as_left(self) -> Parser<'a, Input, Option<T1>> {
        self.transform(|x| x.as_left())
    }

    pub fn as_right(self) -> Parser<'a, Input, Option<T2>> {
        Parser::new(self.clone().transform(|y| y.as_right()))
    }

    pub fn is_left(self) -> Parser<'a, Input, bool> {
        self.transform(|x| x.is_left())
    }

    pub fn is_right(self) -> Parser<'a, Input, bool> {
        self.transform(|x| x.is_right())
    }
}

impl<'a, Input: Debug + Clone + 'a, T1, T2> Parse<'a, Input, (T1, T2)> for Pair<'a, Input, T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, (T1, T2)> {
        self.parser.parse(input)
    }
}

impl<'a, Input: Debug + Clone + 'a, T1, T2> Parse<'a, Input, EitherType<T1, T2>>
    for Either<'a, Input, T1, T2>
where
    T1: Debug + Clone,
    T2: Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, EitherType<T1, T2>> {
        self.parser.parse(input)
    }
}

impl<'a, Input: Debug + Clone + 'a, T: Debug + Clone> Parser<'a, Input, T> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parse<'a, Input, T> + 'a,
    {
        Self {
            parser: Rc::from(parser),
        }
    }
}

impl<'a, Input: Debug + Clone + 'a, T> Parse<'a, Input, T> for Parser<'a, Input, T>
where
    T: Debug + Clone,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, T> {
        self.parser.parse(input)
    }
}

impl<'a, Function, Input: Debug + Clone + 'a, Output: Debug + Clone + 'a> Parse<'a, Input, Output>
    for Function
where
    Function: Fn(Input) -> ParseResult<'a, Input, Output>,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output> {
        self(input)
    }
}

pub fn match_literal<'a>(expected: &'a str) -> impl Parse<'a, &str, &str> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[..expected.len()], &input[expected.len()..])),
        _ => Err("error"),
    }
}

#[test]
fn match_literal_test_1() {
    let parser = match_literal("1");
    let input = "1";
    let result = parser.parse(input);

    assert_eq!(result, Ok(("1", "")))
}

#[test]
fn match_literal_test_2() {
    let parser = match_literal("1");
    let input = "12";
    let result = parser.parse(input);

    assert_eq!(result, Ok(("1", "2")))
}

#[test]
fn match_literal_test_3() {
    let parser = match_literal("123");
    let input = "123";
    let result = parser.parse(input); //.clone()
    assert_eq!(result, Ok(("123", "")))
}

#[test]
fn match_literal_test_4() {
    let parser = match_literal("123");
    let input = "12345";
    let result = parser.parse(input);
    assert_eq!(result, Ok(("123", "45")))
}

#[test]
fn match_literal_test_5() {
    let parser = match_literal("123");
    let input = "00012345";
    let result = parser.parse(input);
    assert_eq!(result, Err("error"))
}

#[test]
fn one_or_more_test_1() {
    let input = "1111111111111111";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = Ok((
        vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ],
        "",
    ));
    let result = parser.parse(input);

    assert_eq!(should, result);
    let fun = &|input: &'static str| match input.chars().next() {
        Some(next) => {
            let rest = &input[next.len_utf8()..];
            Ok((next, rest))
        }
        _ => Err(input),
    };
    let parser = Parser::new(fun);
    let result = parser.parse("1");
    let should = Ok(('1', ""));
    assert_eq!(should, result)
}

#[test]
fn one_or_more_test_2() {
    let input = "1111111111111111222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = Ok((
        vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ],
        "222",
    ));
    let result = parser.parse(input);
    assert_eq!(should, result)
}

#[test]
fn one_or_more_test_3() {
    let input = "222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let result = parser.parse(input);

    assert_eq!(Err("error"), result)
}



 #[test]
 fn zero_or_more_test_1() {
     let input = "1111111111111111";
     let init = match_literal("1");
     let parser1 = Parser::new(init);
     let parser = zero_or_more(parser1);
     let should =
         Ok((vec![
             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
         ],
         ""))
     ;
     let result = parser.parse(input);

     assert_eq!(should, result)
 }

 // #[test]
 // fn zero_or_more_test_2() {
 //     let input = "1111111111111111222";
 //     let init = match_literal("1");
 //     let parser1 = Parser::new(init);
 //     let parser = zero_or_more(parser1);
 //     let should = (
 //         Ok(vec![
 //             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
 //         ]),
 //         "222",
 //     );
 //     let result = parser.parse(input);

 //     assert_eq!(should, result)
 // }

 // #[test]
 // fn zero_or_more_test_3() {
 //     let input = "222";
 //     let init = match_literal("1");
 //     let parser1 = Parser::new(init);
 //     let parser = zero_or_more(parser1);
 //     let result = parser.parse(input);

 //     assert_eq!((Ok(vec![]), "222"), result)
 // }

 pub fn transform<
     'a,
     Parser,
     TransformFunction,
     Input: Debug + Clone + 'a,
     Output1: Debug + Clone,
     Output2: Debug + Clone + 'a,
 >(
     parser: Parser,
     transfomfunc: TransformFunction,
 ) -> impl Parse<'a, Input, Output2>
 where
     Parser: Parse<'a, Input, Output1> + 'a,
     TransformFunction: Fn(Output1) -> Output2,
 {
     move |input| {
         parser.parse(input).map(|(result,rest)| (transfomfunc(result), rest))

     }
 }

#[test]
fn transform_0() {
    let input = "222";
    let init = match_literal("2");
    let parser1 = Parser::new(init);
    let parser = Parser::new(one_or_more(parser1)).transform(|s| {
        let mut digits = String::from("");
        for digit in s {
            digits.push_str(digit);
        }
        digits.parse::<i32>().unwrap()
    });
    let result = parser.parse(input);
    let expected = Ok((22, ""));
    assert_eq!(expected, result)
}



#[test]
fn predicate_0() {
    let input = "222";
    let init = match_literal("2");
    let parser1 = Parser::new(init);
    let parser = Parser::new(one_or_more(parser1)).predicate(|s| {
        let mut digits = String::from("");
        for digit in s {
            digits.push_str(*digit);
        }
        match digits.parse::<i32>() {
            Ok(_) => true,
            Err(_) => false,
        }
    });
    let result = parser.parse(input);
    let expected = Ok((vec!["2", "2", "2"], ""));
    assert_eq!(expected, result)
}


#[test]
fn and_then_0() {
    let input = "223";
    let init = match_literal("2");
    let parser1 = Parser::new(init).new_parser_from_parse_result(|x| Parser::new(match_literal(x)));

    let result = parser1.parse(input);
    let expected = Ok(("2", "3"));
    assert_eq!(expected, result)
}


#[test]
fn or_else_0() {
    let input = "2123";
    let init1 = match_literal("1");
    let init2 = match_literal("2");

    let parser2 = Parser::new(init2);
    let parser1 = Parser::new(init1).or_else::<Parser<'_, &str, &str>>(parser2);

    let result = parser1.parse(input);
    let expected = Ok(("2", "123"));
    assert_eq!(expected, result);

    let input2 = "21";
    let result = parser1.parse(input2);
    let expected = Ok(("2", "1"));
    assert_eq!(expected, result);

    let input3 = "12";
    let result = parser1.parse(input3);
    let expected = Ok(("1", "2"));
    assert_eq!(expected, result)
}

fn any(input: &str) -> ParseResult<&str, char> {
    match input.chars().next() {
        Some(next) => Ok((next, &input[next.len_utf8()..])),
        _ => Err("error"),
    }
}

//todo testsPa
//     Input shall implement iterable and equality
//     Vec struct for one_or_many etc.
//     one_of, between, starts_with / ends_with  ---
//     digit /float /integer /id / whitespace / newline
//     somehow put constructors into Parser object like Parser::any(..) etc.
