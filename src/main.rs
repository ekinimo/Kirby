use std::str::Chars;

use parser_combinator::pair::Pair;
use parser_combinator::parser::{match_literal, Parser};
use parser_combinator::repeated::RepeatedParser;
use parser_combinator::triple::Triple;
use parser_combinator::{parser, Parse, ParseResult};

fn main() {
    //Calculator stuff
    println!("{:?}", top_level("1+2;".chars(), 0));
    println!("{:?}", top_level("1+2+3+4+5;".chars(), 0));
    println!("{:?}", top_level("1+(2+3)*4;".chars(), 0));
    println!("{:?}", top_level("1+2+3*4+1;".chars(), 0));
    println!("{:?}", top_level("1*2*3*4;".chars(), 0));
    println!("{:?}", top_level("1*(2+3*4);".chars(), 0));
    println!("{:?}", top_level("1+2+3*4+5+2+6;".chars(), 0));
}

//type ParseResult<'a, Input, Output> = Result<(Output, Input), String>;

fn increment(x: i32) -> i32 {
    /*println!("hello {x}");*/
    x + 1
}

pub fn top_level(input: Chars, _state: i32) -> ParseResult<Chars, i32, i32, String> {
    expression
        .pair(match_literal(";".chars(), increment))
        .first()
        .with_error(|_, _| "error".to_string())
        .parse(input, _state)
}

pub fn expression(input: Chars, _state: i32) -> ParseResult<Chars, i32, i32, String> {
    Pair::new(
        term,
        RepeatedParser::zero_or_more(match_literal("+".chars(), increment).pair(term)),
    )
    .transform(|(x, y)| y.iter().fold(x, |a, (_, b)| a + b))
    .with_error(|_, _| "error".to_string())
    .parse(input.clone(), _state)
}

pub fn term(input: Chars, _state: i32) -> ParseResult<Chars, i32, i32, String> {
    Pair::new(
        factor,
        RepeatedParser::zero_or_more(match_literal("*".chars(), increment).pair(factor).second()),
    )
    .transform(|(x, y)| y.iter().fold(x, |a, b| a * b))
    .with_error(|_, _| "error".to_string())
    .parse(input.clone(), _state)
}

pub fn factor(input: Chars, _state: i32) -> ParseResult<Chars, i32, i32, String> {
    //println!("factor = {:?}", input);
    let parse_digit = vec![
        parser::Parser::new(match_literal("1".chars(), increment)),
        parser::Parser::new(match_literal("2".chars(), increment)),
        parser::Parser::new(match_literal("3".chars(), increment)),
        parser::Parser::new(match_literal("3".chars(), increment)),
        parser::Parser::new(match_literal("4".chars(), increment)),
        parser::Parser::new(match_literal("5".chars(), increment)),
        parser::Parser::new(match_literal("6".chars(), increment)),
        parser::Parser::new(match_literal("7".chars(), increment)),
        parser::Parser::new(match_literal("8".chars(), increment)),
        parser::Parser::new(match_literal("9".chars(), increment)),
        parser::Parser::new(match_literal("0".chars(), increment)),
    ]
    .iter()
    .fold(
        parser::Parser::new(match_literal("0".chars(), increment)),
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

    Triple::new(
        match_literal("(".chars(), increment),
        expression,
        match_literal(")".chars(), increment),
    )
    .second()
    .or_else(parse_natural_numbers.clone())
    .with_error(|(_, _), _| "error".to_string())
    .parse(input.clone(), _state)
}

// println!("Hello, world!");
// let parse_digit = vec![
//     parser::Parser::new(match_literal_str("1")),
//     parser::Parser::new(match_literal_str("2")),
//     parser::Parser::new(match_literal_str("3")),
//     parser::Parser::new(match_literal_str("3")),
//     parser::Parser::new(match_literal_str("5")),
//     parser::Parser::new(match_literal_str("4")),
//     parser::Parser::new(match_literal_str("7")),
//     parser::Parser::new(match_literal_str("8")),
//     parser::Parser::new(match_literal_str("9")),
// ]
// .iter()
// .fold(
//     parser::Parser::new(match_literal_str("0")),
//     |x: Parser<Chars, Chars>, y| x.or_else::<Parser<Chars, Chars>>(y.clone()),
// );

// // println!("{:?}", parse_digit.parse("23".chars()));

// let parse_digits = parse_digit.clone().zero_or_more();

// let parse_natural_numbers = parse_digits.clone().transform(|s| {
//     let mut digits = String::from("");
//     for digit in s {
//         digits.push_str(digit.as_str());
//     }
//     digits.parse::<i32>().unwrap()
// });

// //  println!("{:?}", parse_natural_numbers.parse("23".chars()));

// let parse_ones = Parser::new(match_literal_str("1")).zero_or_more();
// //println!("{:?}", parse_ones.parse("11111123".chars()));

// let parse_twos = Parser::new(match_literal_str("2")).zero_or_more();
// // println!("{:?}", parse_twos.clone().parse("11111122".chars()));

// let parse_ones_then_twos = parse_ones.clone().pair(parse_twos.clone());
// //  println!("{:?}", parse_ones_then_twos.parse("11111122".chars()));

// let parse_ones_then_twos_first = parse_ones.clone().pair(parse_twos.clone()).first();
// //    println!("{:?}", parse_ones_then_twos_first.parse("11111122".chars()));

// let parse_ones_then_twos_second = parse_ones.pair(parse_twos).second();
// // println!(
// //     "{:?}",
// //     parse_ones_then_twos_second.parse("11111122".chars())
// // );

// let parse_whitespace = Many::zero_or_more(match_literal(" ".chars()));
// let a = parse_digit.clone().pair(parse_whitespace.clone()).first();
// let match_lparen = match_literal("(".chars());
// let match_rparen = match_literal(")".chars());

// let b = match_lparen
//     .triple(parse_natural_numbers.clone(), match_rparen)
//     .either(parse_digit);
