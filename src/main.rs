//#![feature(member_constraints)]
//#![feature(toowned_clone_into)]

use std::str::Chars;

use parser::{match_literal, match_literal_str};

use crate::pair::Pair;
use crate::parser::{Parse, Parser, ParseResult};
use crate::repeated::RepeatedParser;
use crate::triple::Triple;

mod pair;
mod parser;
mod repeated;
mod triple;

fn main() {
    //Calculator stuff
    println!("{:?}", top_level("1+2+3*4+6;".chars()));
    println!("{:?}", top_level("1+2+3+4+5;".chars()));
    println!("{:?}", top_level("1+(2+3)*4;".chars()));
    println!("{:?}", top_level("1+2+3*4+1;".chars()));
    println!("{:?}", top_level("1*2*3*4;".chars()));
    println!("{:?}", top_level("1*(2+3*4);".chars()));
    println!("{:?}", top_level("1+2+3*4+5+2+6;".chars()));
}

//type ParseResult<'a, Input, Output> = Result<(Output, Input), String>;

pub fn top_level<'a>(mut input: Chars<'a>) -> ParseResult<'a, Chars<'a>, i32> {
    expression
        .pair(match_literal(";".chars()))
        .first()
        .parse(input)
}

pub fn expression<'a>(mut input: Chars<'a>) -> ParseResult<'a, Chars<'a>, i32> {
    let res = Pair::new(
        term,
        RepeatedParser::zero_or_more(match_literal("+".chars()).pair(term).second()),
    )
    .transform(|(x, y)| y.iter().fold(x, |a, b| a + b))
    .parse(input.clone());
    res
}

pub fn term<'a>(mut input: Chars<'a>) -> ParseResult<'a, Chars<'a>, i32> {
    let res = Pair::new(
        factor,
        RepeatedParser::zero_or_more(match_literal("*".chars()).pair(factor).second()),
    )
    .transform(|(x, y)| y.iter().fold(x, |a, b| a * b))
    .parse(input.clone());
    res
}

pub fn factor<'a>(mut input: Chars<'a>) -> ParseResult<'a, Chars<'a>, i32> {
    //println!("factor = {:?}", input);
    let parse_digit = vec![
        parser::Parser::new(match_literal_str("1")),
        parser::Parser::new(match_literal_str("2")),
        parser::Parser::new(match_literal_str("3")),
        parser::Parser::new(match_literal_str("3")),
        parser::Parser::new(match_literal_str("4")),
        parser::Parser::new(match_literal_str("5")),
        parser::Parser::new(match_literal_str("6")),
        parser::Parser::new(match_literal_str("7")),
        parser::Parser::new(match_literal_str("8")),
        parser::Parser::new(match_literal_str("9")),
        parser::Parser::new(match_literal_str("0")),
    ]
    .iter()
    .fold(
        parser::Parser::new(match_literal_str("0")),
        |x: Parser<Chars, Chars>, y| x.or_else::<Parser<Chars, Chars>>(y.clone()),
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

    let res = Triple::new(
        match_literal("(".chars()),
        expression,
        match_literal(")".chars()),
    )
    .second()
    .or_else::<Parser<Chars, i32>>(parse_natural_numbers.clone())
    .parse(input.clone());
    res
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
