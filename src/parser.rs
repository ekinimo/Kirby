use std::{cell::RefCell, fmt::Debug, rc::Rc, marker::PhantomData};

use crate::{Parse, ParseResult};

//type ParseResult<'a, Input, Output> = (Result<Output, &'a str>, Input);
//Core
// pub trait Parse<'a, Output >   {
//     fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
// }

#[derive(Clone)]
pub struct Parser<'a, Input, State, Output, Error>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq + 'a,
{
    parser: Rc<dyn Parse<'a, Input, State, Output, Error> + 'a>,
}

impl<'a, Input, State, Output, Error> Parser<'a, Input, State, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
    State: Clone,
{
    pub fn new<P>(parser: P) -> Self
    where
        P: Parse<'a, Input, State, Output, Error> + 'a,
    {
        Self {
            parser: Rc::from(parser),
        }
    }
}

impl<'a, Input, State, Output, Error> Parse<'a, Input, State, Output, Error>
    for Parser<'a, Input, State, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
    State: Clone + 'a,
{
    fn parse(&self, input: Input, state: State) -> ParseResult<'a, Input, State, Output, Error> {
        self.parser.parse(input, state)
    }
}

#[derive(Clone)]
pub struct ForwardRef<Input, Output, State, Error,P>
where
    Input:Clone  + Iterator+ 'static,
<Input as Iterator>::Item: Eq,
    Error: Clone + 'static,
    Output: Clone ,
    State: Clone + 'static,
    P : Parse<'static, Input, Output, State, Error>
{
    phantom1: PhantomData<Input>,
    phantom2: PhantomData<Output>,
    phantom3: PhantomData<State>,
    phantom4: PhantomData<Error>,
    
    parser: Rc<RefCell<Option<P>>>,
}


















impl<Input, Output, Error, State,P> ForwardRef< Input, Output, State, Error,P>
where
    Input: Clone +  Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone ,
    Output: Clone ,
    State: Clone ,
P : for<'a >Parse<'a, Input, Output, State, Error>
{
    pub fn new() -> Self {
        Self {
            parser: Rc::new(RefCell::new(None)),
            phantom1:PhantomData,
            phantom2:PhantomData,
            phantom3:PhantomData,
            phantom4:PhantomData,
        }
    }
    pub fn set_parser(&mut self, parser: P) {
        *self.parser.borrow_mut() = Some(parser)
    }
}








impl<Input, Output, Error, State,P> Default for ForwardRef< Input, Output, State, Error,P>
where
    Input:Clone  + Iterator+ 'static,
<Input as Iterator>::Item: Eq,
    Error: Clone + 'static,
    Output: Clone ,
    State: Clone + 'static,
P : Parse<'static, Input, Output, State, Error>
{
    fn default() -> Self {
        Self { phantom1: Default::default(), phantom2: Default::default(), phantom3: Default::default(), phantom4: Default::default(), parser: Default::default() }
    }
}











impl< Input, State, Output, Error,P> Parse<'static, Input, State, Output, Error>
    for
    ForwardRef< Input, State, Output, Error,P>
where
    Input: Clone  + Iterator ,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'static,
    State: Clone + 'static,
    Output: Clone  + 'static,
P : Parse<'static, Input,  State,Output, Error> + Fn(Input, State)->Result<(Output, State, Input), Error>+'static
{
    fn parse(&self, input: Input, state: State) -> ParseResult<'static, Input, State, Output, Error> {
        //self.parser.parse(input, state)
        match &*self.parser.borrow() {
            None => {
                panic!("Forward Ref is not set yet")
            }
            Some(p) => p.parse(input, state),
        }
    }
}

pub fn match_literal<'a, Input, State, F>(
    to_match: Input,
    state_transformer: F,
) -> Parser<'a, Input, State, Input, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    State: Clone,
    F: Fn(State) -> State + 'a,
{
    Parser::new(move |mut input: Input, state: State| {
        let to_match_length = to_match.clone().count();
        let first_input_elements = input.clone().take(to_match_length);

        if first_input_elements.clone().count() != to_match_length {
            return Err(format!(
                "match_literal failed: expected {:?}, got {:?}",
                to_match, input
            ));
        }

        match first_input_elements
            .zip(to_match.clone())
            .all(|(x, y)| x == y)
        {
            true => {
                for _ in 0..to_match_length {
                    input.next();
                }
                Ok((to_match.to_owned(), state_transformer(state), input))
            }
            false => Err(format!(
                "match_literal failed: expected {:?}, got {:?}",
                to_match, input
            )),
        }
    })
}

pub fn match_anything<'a, Input, State, F>(
    transition_fun: F,
) -> Parser<'a, Input, State, <Input as Iterator>::Item, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    State: Clone,
    F: Fn(State) -> State + 'a,
{
    Parser::new(move |mut input: Input, state: State| match input.next() {
        Some(x) => Ok((x, transition_fun(state), input)),
        None => Err(format!(
            "Parser Combinator : match_anything failed. expected input got {:?}",
            input
        )),
    })
}

//we need to deprecate this
pub fn match_character<'a, Input, State>(
    character: <Input as Iterator>::Item,
) -> Parser<'a, Input, State, <Input as Iterator>::Item, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Debug + Eq,
    State: Clone,
{
    Parser::new(move |mut input: Input, state: State| match input.next() {
        Some(x) if x == character => Ok((x, state, input)),
        Some(x) => Err(format!("expected {:?}, got {:?}", character, x)),
        None => Err(format!("expected {:?}, got nothing", character)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{match_character, match_literal};

    use super::ForwardRef;
    fn id<T>(x: T) -> T {
        x
    }

    #[test]
    fn match_character_succeeds() {
        let parser = match_character('1');

        let result = parser.parse("1".chars(), ());

        match result {
            Ok(('1', _, input)) => {
                assert_eq!(input.as_str(), "")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn forward_succeeds() {
        
        let mut a = ForwardRef::new();
        let b = a.clone().triple(match_character('1'), a.clone());
        a.set_parser(&|i, s| match_character('3').parse(i, s));
        let result = b.parse("313".chars(), ());

        match result {
            Ok((output, _, _rest)) => {
                assert_eq!(output, ('3', '1', '3'));
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn forward_succeeds_2() {
        fn increment(x: i32) -> i32 {
            /*println!("hello {x}");*/
            x + 1
        }

        use std::str::Chars;

        let parse_digit = vec![
            Parser::new(match_literal("1".chars(), increment)),
            Parser::new(match_literal("2".chars(), increment)),
            Parser::new(match_literal("3".chars(), increment)),
            Parser::new(match_literal("3".chars(), increment)),
            Parser::new(match_literal("4".chars(), increment)),
            Parser::new(match_literal("5".chars(), increment)),
            Parser::new(match_literal("6".chars(), increment)),
            Parser::new(match_literal("7".chars(), increment)),
            Parser::new(match_literal("8".chars(), increment)),
            Parser::new(match_literal("9".chars(), increment)),
            Parser::new(match_literal("0".chars(), increment)),
        ]
        .iter()
        .fold(
            Parser::new(match_literal("0".chars(), increment)),
            |x: Parser<Chars, i32, Chars, String>, y: &Parser<Chars, i32, Chars, String>| {
                x.or_else(y.clone()).with_error(|_, _| "error".to_string())
            },
        );
        let parse_digits = parse_digit.clone().one_or_more();

        let parse_natural_numbers = parse_digits.clone().transform(|s| {
            let mut digits = String::from("");
            for digit in s {
                digits.push_str(digit.as_str());
            }
            //    println!("digits = {:?}", digits);
            digits.parse::<i32>().unwrap()
        });

        let mut expr = ForwardRef::new();

        let factor = match_literal("(".chars(), increment)
            .triple(expr.clone(), match_literal(")".chars(), increment))
            .second()
            .or_else(parse_natural_numbers.clone())
            .with_error(|(_, _), _| "error".to_string());
        let term = factor
            .clone()
            .pair(
                match_literal("*".chars(), increment)
                    .pair(factor.clone())
                    .second()
                    .zero_or_more(),
            )
            .transform(|(x, y)| y.iter().fold(x, |a, b| a * b))
            .with_error(|_, _| "error".to_string());
        let _top_level = expr
            .clone()
            .pair(match_literal(";".chars(), increment))
            .first()
            .with_error(|_, _| "error".to_string());

        let expr2:Parser<Chars,i32,i32,String>  = term
            .clone()
            .pair(
                match_literal("+".chars(), increment)
                    .pair(term.clone())
                    .zero_or_more(),
            )
            .transform(|(x, y)| y.iter().fold(x, |a, (_, b)| a + b))
            .with_error(|_, _| "error".to_string());

        expr.set_parser(move |i,s| expr2.clone().parse(i, s));

        let result = expr.parse("1+2*3".chars(), 0);
        match result {
            Ok((output, _, _rest)) => {
                assert_eq!(output, 7);
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_character_fails() {
        let parser = match_character('1');

        let result = parser.parse("a".chars(), ());

        match result {
            Err(message) => {
                assert_eq!(message, "expected '1', got 'a'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_of_length_1_succeeds() {
        let under_test = match_literal("a".chars(), id);

        let result = under_test.parse("abc".chars(), ());

        match result {
            Ok((output, _, rest)) => {
                assert_eq!(output.as_str(), "a".to_string());
                assert_eq!(rest.as_str(), "bc".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_of_length_1_fails() {
        let under_test = match_literal("a".chars(), id);

        let result = under_test.parse("def".chars(), ());

        match result {
            Err(message) => {
                assert_eq!(
                    message,
                    "match_literal failed: expected Chars(['a']), got Chars(['d', 'e', 'f'])"
                        .to_string()
                );
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_with_empty_input_fails() {
        let under_test = match_literal("a".chars(), id);

        let result = under_test.parse("".chars(), ());

        match result {
            Err(message) => {
                assert_eq!(
                    message,
                    "match_literal failed: expected Chars(['a']), got Chars([])".to_string()
                );
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_of_size_3_with_input_length_less_than_to_match_fails() {
        let under_test = match_literal("abc".chars(), id);

        let result = under_test.parse("a".chars(), ());

        match result {
            Err(message) => {
                assert_eq!(
                    message,
                    "match_literal failed: expected Chars(['a', 'b', 'c']), got Chars(['a'])"
                        .to_string()
                );
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn or_else() {
        let input = "2123";

        let left = match_character('1');
        let right = match_character('2');

        let under_test = left.or_else(right);

        let result = under_test.parse(input.chars(), ());

        match result {
            Ok(('2', _, input)) => {
                assert_eq!(input.as_str(), "123")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input2 = "21";
        let result = under_test.parse(input2.chars(), ());

        match result {
            Ok(('2', _, input)) => {
                assert_eq!(input.as_str(), "1")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input3 = "12";
        let result = under_test.parse(input3.chars(), ());

        match result {
            Ok(('1', (), input)) => {
                assert_eq!(input.as_str(), "2")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input4 = "abc";
        let result = under_test.parse(input4.chars(), ());

        match result {
            Err((left, right)) => {
                assert_eq!(left, "expected '1', got 'a'".to_string());
                assert_eq!(right, "expected '2', got 'a'".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }
}

// #[test]
// fn one_or_more_test_1() {
//     let input = "1111111111111111";
//     let init = match_literal("1");
//     let parser1 = Parser::new(init);
//     let parser = one_or_more(parser1);
//     let should = Ok((
//         vec![
//             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
//         ],
//         "",
//     ));
//     let result = parser.parse(input);

//     assert_eq!(should, result);
//     let fun = &|input: &'static str| match input.chars().next() {
//         Some(next) => {
//             let rest = &input[next.len_utf8()..];
//             Ok((next, rest))
//         }
//         _ => Err(input.to_string()),
//     };
//     let parser = Parser::new(fun);
//     let result = parser.parse("1");
//     let should = Ok(('1', ""));
//     assert_eq!(should, result)
// }

// #[test]
// fn one_or_more_test_2() {
//     let input = "1111111111111111222";
//     let init = match_literal("1");
//     let parser1 = Parser::new(init);
//     let parser = one_or_more(parser1);
//     let should = Ok((
//         vec![
//             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
//         ],
//         "222",
//     ));
//     let result = parser.parse(input);
//     assert_eq!(should, result)
// }

// #[test]
// fn one_or_more_test_3() {
//     let input = "222";
//     let init = match_literal("1");
//     let parser1 = Parser::new(init);
//     let parser = one_or_more(parser1);
//     let result = parser.parse(input);

//     assert_eq!(Err("error".to_string()), result)
// }

// #[test]
// fn zero_or_more_test_1() {
//     let input = "1111111111111111";
//     let init = match_literal("1");
//     let parser1 = Parser::new(init);
//     let parser = zero_or_more(parser1);
//     let should = Ok((
//         vec![
//             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
//         ],
//         "",
//     ));
//     let result = parser.parse(input);

//     assert_eq!(should, result)
// }

// // #[test]
// // fn zero_or_more_test_2() {
// //     let input = "1111111111111111222";
// //     let init = match_literal("1");
// //     let parser1 = Parser::new(init);
// //     let parser = zero_or_more(parser1);
// //     let should = (
// //         Ok(vec![
// //             "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
// //         ]),
// //         "222",
// //     );
// //     let result = parser.parse(input);

// //     assert_eq!(should, result)
// // }

// // #[test]
// // fn zero_or_more_test_3() {
// //     let input = "222";
// //     let init = match_literal("1");
// //     let parser1 = Parser::new(init);
// //     let parser = zero_or_more(parser1);
// //     let result = parser.parse(input);

// //     assert_eq!((Ok(vec![]), "222"), result)
// // }

// #[test]
// fn transform_0() {
//     let input = "222";
//     let init = match_literal("2");
//     let parser1 = Parser::new(init);
//     let parser = Parser::new(one_or_more(parser1)).transform(|s| {
//         let mut digits = String::from("");
//         for digit in s {
//             digits.push_str(digit);
//         }
//         digits.parse::<i32>().unwrap()
//     });
//     let result = parser.parse(input);
//     let expected = Ok((22, ""));
//     assert_eq!(expected, result)
// }

// #[test]
// fn predicate_0() {
//     let input = "222";
//     let init = match_literal("2");
//     let parser1 = Parser::new(init);
//     let parser = Parser::new(one_or_more(parser1)).predicate(|s| {
//         let mut digits = String::from("");
//         for digit in s {
//             digits.push_str(*digit);
//         }
//         match digits.parse::<i32>() {
//             Ok(_) => true,
//             Err(_) => false,
//         }
//     });
//     let result = parser.parse(input);
//     let expected = Ok((vec!["2", "2", "2"], ""));
//     assert_eq!(expected, result)
// }

// #[test]
// fn and_then_0() {
//     let input = "223";
//     let init = match_literal("2");
//     let parser1 = Parser::new(init).new_parser_from_parse_result(|x| Parser::new(match_literal(x)));

//     let result = parser1.parse(input);
//     let expected = Ok(("2", "3"));
//     assert_eq!(expected, result)
// }

// fn any(input: &str) -> ParseResult<&str, char> {
//     match input.chars().next() {
//         Some(next) => Ok((next, &input[next.len_utf8()..])),
//         _ => Err("error".to_string()),
//     }
// }

//todo testsPa
//     Input shall implement iterable and equality
//     Vec struct for one_or_many etc.
//     one_of, between, starts_with / ends_with  ---
//     digit /float /integer /id / whitespace / newline
//     somehow put constructors into Parser object like Parser::any(..) etc.
