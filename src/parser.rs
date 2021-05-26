use std::{fmt::Debug, rc::Rc};

type ParseResult<'a, Input, Output> = (Result<Output, &'a str>, Input);

//Core
// pub trait Parse<'a, Output >   {
//     fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
// }

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
        Parser::new(transform(self, transfomfunc))
    }
    fn predicate<PredicateFunction>(self, pred_fn: PredicateFunction) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        PredicateFunction: Fn(&Output) -> bool + 'a,
    {
        Parser::new(predicate(self, pred_fn))
    }

    fn and_then<F, NextParser, Output2>(self, f: F) -> Parser<'a, Input, Output2>
    where
        Self: Sized + 'a,
        Output: 'a + Debug + Clone,
        Output2: 'a + Debug + Clone,
        NextParser: Parse<'a, Input, Output2> + 'a + Clone,
        F: Fn(Output) -> NextParser + 'a,
    {
        Parser::new(and_then(self, f))
    }

    fn or_else<Parser1>(self, parser2: Self) -> Parser<'a, Input, Output>
    where
        Self: Sized + 'a,
        Output: 'a + Debug,
        Parser1: Parse<'a, Input, Output>,
    {
        Parser::new(or_else(self, parser2))
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

#[derive(Clone)]
pub struct Parser<'a, Input: Debug + 'a, T: Debug> {
    parser: Rc<dyn Parse<'a, Input, T> + 'a>,
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

impl<'a, Function, Input: Debug + Clone + 'a, Output: Debug + Clone> Parse<'a, Input, Output>
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
        Some(next) if next == expected => (Ok(&input[..expected.len()]), &input[expected.len()..]),
        _ => (Err("error"), input),
    }
}

#[test]
fn match_literal_test_1() {
    let parser = match_literal("1");
    let input = "1";
    let result = parser.parse(input);

    assert_eq!(result, (Ok("1"), ""))
}

#[test]
fn match_literal_test_2() {
    let parser = match_literal("1");
    let input = "12";
    let result = parser.parse(input);

    assert_eq!(result, (Ok("1"), "2"))
}

#[test]
fn match_literal_test_3() {
    let parser = match_literal("123");
    let input = "123";
    let result = parser.parse(input);
    assert_eq!(result, (Ok("123"), ""))
}

#[test]
fn match_literal_test_4() {
    let parser = match_literal("123");
    let input = "12345";
    let result = parser.parse(input);
    assert_eq!(result, (Ok("123"), "45"))
}

#[test]
fn match_literal_test_5() {
    let parser = match_literal("123");
    let input = "00012345";
    let result = parser.parse(input);
    assert_eq!(result, (Err("error"), "00012345"))
}

pub fn one_or_more<'a, Parser1, Input: Debug + Clone + 'a, Result1: Debug + Clone>(
    parser: Parser1,
) -> impl Parse<'a, Input, Vec<Result1>>
where
    Parser1: Parse<'a, Input, Result1>,
{
    move |mut input: Input| {
        let mut result = Vec::new();

        if let (Ok(first_item), next_input) = parser.parse(input.clone()) {
            input = next_input;
            result.push(first_item);
        } else {
            return (Err("error"), input);
        }

        while let (Ok(next_item), next_input) = parser.parse(input.clone()) {
            input = next_input;
            result.push(next_item);
        }

        (Ok(result), input)
    }
}

#[test]
fn one_or_more_test_1() {
    let input = "1111111111111111";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = (
        Ok(vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ]),
        "",
    );
    let result = parser.parse(input);

    assert_eq!(should, result);

    let parser = Parser::new(move |input: &'static str| match input.chars().next() {
        Some(next) => {
            let rest = &input[next.len_utf8()..];
            (Ok(next), rest)
        }
        _ => (Err(input), input),
    });
    let result = parser.parse("1");
    let should = (Ok('1'), "");
    assert_eq!(should, result)
}

#[test]
fn one_or_more_test_2() {
    let input = "1111111111111111222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = one_or_more(parser1);
    let should = (
        Ok(vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ]),
        "222",
    );
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

    assert_eq!((Err("error"), input), result)
}

pub fn zero_or_more<'a, Parser1, Input: Debug + Clone + 'a, Result1: Debug + Clone>(
    parser: Parser1,
) -> impl Parse<'a, Input, Vec<Result1>>
where
    Parser1: Parse<'a, Input, Result1>,
{
    move |mut input: Input| {
        let mut result = Vec::new();

        while let (Ok(next_item), next_input) = parser.parse(input.clone()) {
            input = next_input;
            result.push(next_item);
        }

        (Ok(result), input)
    }
}

#[test]
fn zero_or_more_test_1() {
    let input = "1111111111111111";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = zero_or_more(parser1);
    let should = (
        Ok(vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ]),
        "",
    );
    let result = parser.parse(input);

    assert_eq!(should, result)
}

#[test]
fn zero_or_more_test_2() {
    let input = "1111111111111111222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = zero_or_more(parser1);
    let should = (
        Ok(vec![
            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
        ]),
        "222",
    );
    let result = parser.parse(input);

    assert_eq!(should, result)
}

#[test]
fn zero_or_more_test_3() {
    let input = "222";
    let init = match_literal("1");
    let parser1 = Parser::new(init);
    let parser = zero_or_more(parser1);
    let result = parser.parse(input);

    assert_eq!((Ok(vec![]), "222"), result)
}

pub fn transform<
    'a,
    Parser,
    TransformFunction,
    Input: Debug + Clone + 'a,
    Output1: Debug + Clone,
    Output2: Debug + Clone,
>(
    parser: Parser,
    transfomfunc: TransformFunction,
) -> impl Parse<'a, Input, Output2>
where
    Parser: Parse<'a, Input, Output1> + 'a,
    TransformFunction: Fn(Output1) -> Output2,
{
    move |input| {
        let (result, rest) = parser.parse(input);
        match result {
            Ok(ret) => (Ok(transfomfunc(ret)), rest),
            Err(_) => (Err("error"), rest),
        }
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
    let expected = (Ok(222), "");
    assert_eq!(expected, result)
}

pub fn predicate<
    'a,
    Parser1,
    Input: Debug + Clone + 'a,
    Result1: Debug + Clone,
    PredicateFunction,
>(
    parser: Parser1,
    predicate: PredicateFunction,
) -> impl Parse<'a, Input, Result1>
where
    Parser1: Parse<'a, Input, Result1> + 'a,
    PredicateFunction: Fn(&Result1) -> bool + 'a,
{
    move |input: Input| {
        let input_ = input.clone();
        if let (Ok(value), next_input) = parser.parse(input.clone()) {
            if predicate(&value) {
                return (Ok(value), next_input);
            }
        }
        (Err("error"), input)
    }
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
    let expected = (Ok(vec!["2", "2", "2"]), "");
    assert_eq!(expected, result)
}

fn and_then<'a, Input: Debug + Clone + 'a, P, F, A: Debug + Clone, B: Debug + Clone, NextP>(
    parser: P,
    f: F,
) -> impl Parse<'a, Input, B>
where
    P: Parse<'a, Input, A>,
    NextP: Parse<'a, Input, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        (Ok(result), next_input) => f(result).parse(next_input),
        (Err(err), rest) => (Err(err), rest),
    }
}

#[test]
fn and_then_0() {
    let input = "223";
    let init = match_literal("2");
    let parser1 = Parser::new(init).and_then(|x| Parser::new(match_literal(x)));

    let result = parser1.parse(input);
    let expected = (Ok("2"), "3");
    assert_eq!(expected, result)
}

pub fn or_else<'a, Parser1, Input: Debug + Clone + 'a, Result1: Debug + Clone>(
    parser1: Parser1,
    parser2: Parser1,
) -> impl Parse<'a, Input, Result1>
where
    Parser1: Parse<'a, Input, Result1>,
{
    move |input: Input| match parser1.parse(input.clone()) {
        res @ (Ok(_), _) => res,
        (Err(_), _) => parser2.parse(input),
    }
}

#[test]
fn or_else_0() {
    let input = "2123";
    let init1 = match_literal("1");
    let init2 = match_literal("2");

    let parser2 = Parser::new(init2);
    let parser1 = Parser::new(init1).or_else::<Parser<'_, &str, &str>>(parser2);

    let result = parser1.parse(input);
    let expected = (Ok("2"), "123");
    assert_eq!(expected, result);

    let input2 = "21";
    let result = parser1.parse(input2);
    let expected = (Ok("2"), "1");
    assert_eq!(expected, result);

    let input3 = "12";
    let result = parser1.parse(input3);
    let expected = (Ok("1"), "2");
    assert_eq!(expected, result)
}

pub fn any(input: &str) -> ParseResult<&str, char> {
    match input.chars().next() {
        Some(next) => (Ok(next), &input[next.len_utf8()..]),
        _ => (Err("error"), input),
    }
}

//todo testsPa
//     pair/left/right                this one needs some massaging its not a member func of Parser
//                                    i dont like it as a standalone function
//     one_of, between, starts_with / ends_with  ---
//     digit /float /integer /id / whitespace / newline
//     somehow put constructors into Parser object like Parser::any(..) etc.

pub fn pair<
    'a,
    Parser1,
    Parser2,
    Input: Debug + Clone + 'a,
    Result1: Debug + Clone,
    Result2: Debug + Clone,
>(
    parser1: Parser1,
    parser2: Parser2,
) -> impl Parse<'a, Input, Rc<(Result1, Result2)>>
where
    Parser1: Parse<'a, Input, Result1> + 'a,
    Parser2: Parse<'a, Input, Result2> + 'a,
{
    move |input| match parser1.parse(input) {
        (Ok(left_result), rest) => {
            println!("Ok Ok   :{:?} {:?}", rest, left_result);
            match parser2.parse(rest) {
                (Ok(right_result), rest2) => {
                    println!("Ok Ok  :{:?} {:?}", rest2, right_result);
                    (Ok(Rc::new((left_result, right_result))), rest2)
                }
                (Err(err), rest) => {
                    println!("Ok Err :{:?}", err);
                    (Err(err), rest)
                }
            }
        }
        (Err(er), rest) => (Err(er), rest),
    }
}

#[test]
fn pair_0() {
    let input = "21";
    let init1 = match_literal("2");
    let init2 = match_literal("1");
    let pair_parse = pair(Parser::new(init1), Parser::new(init2));

    let result = pair_parse.parse(input);
    let expected = (Ok(Rc::new(("2", "1"))), "");
    assert_eq!(expected, result)

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
//     Parser1: Parse<'a, Result1> ,
//     Parser2: Parse<'a, Result2> ,  Result1: Debug ,y Result2: Debug +
// {
//     transform(pair(parser1, parser2), |(first, _)| first)
// }

// pub fn second<'a, Parser1, Parser2, Result1, Result2>(parser1: Parser1, parser2: Parser2) -> impl Parse<'a, Result2>
// where
//     Parser1: Parse<'a, Result1>+,
//     Parser2: Parse<'a, Result2>+, Result1: Debug+, Result2: Debug+
// {

//     transform((parser1, parser2), |(_, second)| second)
// }
