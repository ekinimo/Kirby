use std::{fmt::Debug, rc::Rc, str::Chars};

use crate::{Parse, ParseResult};

//type ParseResult<'a, Input, Output> = (Result<Output, &'a str>, Input);
//Core
// pub trait Parse<'a, Output >   {
//     fn parse(&self, input: &'a str) -> ParseResult<'a,Output>;
// }

#[derive(Clone)]
pub struct Parser<'a, Input, Output, Error>
where
    Input: 'a + Iterator,
    <Input as Iterator>::Item: Eq + 'a,
{
    parser: Rc<dyn Parse<'a, Input, Output, Error> + 'a>,
}

impl<'a, Input, Output, Error> Parser<'a, Input, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
{
    pub fn new<P>(parser: P) -> Self
    where
        P: Parse<'a, Input, Output, Error> + 'a,
    {
        Self {
            parser: Rc::from(parser),
        }
    }
}

impl<'a, Input, Output, Error> Parse<'a, Input, Output, Error> for Parser<'a, Input, Output, Error>
where
    Input: Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
    Error: Clone + 'a,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output, Error> {
        self.parser.parse(input)
    }
}

pub fn match_literal<'a, Input>(to_match: Input) -> Parser<'a, Input, Input, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    Parser::new(move |mut input: Input| {
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
                Ok((to_match.to_owned(), input))
            }
            false => Err(format!(
                "match_literal failed: expected {:?}, got {:?}",
                to_match, input
            )),
        }
    })
}

pub fn match_anything<'a, Input>() -> Parser<'a, Input, <Input as Iterator>::Item, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Eq,
{
    Parser::new(move |mut input: Input| match input.next() {
        Some(x) => Ok((x, input)),
        None => Err(format!(
            "Parser Combinator : match_anything failed. expected input got {:?}",
            input
        )),
    })
}

pub fn match_character<'a, Input>(
    character: <Input as Iterator>::Item,
) -> Parser<'a, Input, <Input as Iterator>::Item, String>
where
    Input: Debug + Clone + 'a + Iterator,
    <Input as Iterator>::Item: Debug + Eq,
{
    Parser::new(move |mut input: Input| match input.next() {
        Some(x) if x == character => Ok((x, input)),
        Some(x) => Err(format!("expected {:?}, got {:?}", character, x)),
        None => Err(format!("expected {:?}, got nothing", character)),
    })
}

pub fn match_literal_str<'a>(expected: &'a str) -> impl Parse<'a, Chars<'a>, Chars<'a>, String> {
    move |input1: Chars<'a>| {
        let input = input1.clone().collect::<String>();
        match input.as_str().get(0..expected.len()) {
            Some(next) if next == expected => {
                let (x, y) = (
                    &input1.as_str()[..expected.len()],
                    &input1.as_str()[expected.len()..],
                );
                //Ok((*(&input1.as_str()[..expected.len()].chars().clone()),
                //    *(&input1.as_str()[expected.len()..].chars().clone())))},
                Ok((x.chars(), y.chars()))
            }
            _ => Err(format!(
                "Parser Combinator : match_literal_str failed. expected {:?} got {:?}",
                expected, input
            )
            .to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{match_character, match_literal};
    use crate::Parse;

    #[test]
    fn match_character_succeeds() {
        let parser = match_character('1');

        let result = parser.parse("1".chars());

        match result {
            Ok(('1', input)) => {
                assert_eq!(input.as_str(), "")
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_character_fails() {
        let parser = match_character('1');

        let result = parser.parse("a".chars());

        match result {
            Err(message) => {
                assert_eq!(message, "expected '1', got 'a'".to_string())
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_of_length_1_succeeds() {
        let under_test = match_literal("a".chars());

        let result = under_test.parse("abc".chars());

        match result {
            Ok((output, rest)) => {
                assert_eq!(output.as_str(), "a".to_string());
                assert_eq!(rest.as_str(), "bc".to_string());
            }
            _ => panic!("failed: {:?}", result),
        }
    }

    #[test]
    fn match_literal_of_length_1_fails() {
        let under_test = match_literal("a".chars());

        let result = under_test.parse("def".chars());

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
        let under_test = match_literal("a".chars());

        let result = under_test.parse("".chars());

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
        let under_test = match_literal("abc".chars());

        let result = under_test.parse("a".chars());

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

        let result = under_test.parse(input.chars());

        match result {
            Ok(('2', input)) => {
                assert_eq!(input.as_str(), "123")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input2 = "21";
        let result = under_test.parse(input2.chars());

        match result {
            Ok(('2', input)) => {
                assert_eq!(input.as_str(), "1")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input3 = "12";
        let result = under_test.parse(input3.chars());

        match result {
            Ok(('1', input)) => {
                assert_eq!(input.as_str(), "2")
            }
            _ => panic!("failed: {:?}", result),
        }

        let input4 = "abc";
        let result = under_test.parse(input4.chars());

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
