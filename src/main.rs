#![feature(member_constraints)]
#![feature(toowned_clone_into)]

use parser::{match_literal,match_literal_str, Parse, ParseResult, Parser};
use std::str::Chars;

mod parser;

fn main() {
    println!("Hello, world!");
    let parse_digit = vec![
        parser::Parser::new(match_literal_str("1")),
        parser::Parser::new(match_literal_str("2")),
        parser::Parser::new(match_literal_str("3")),
        parser::Parser::new(match_literal_str("3")),
        parser::Parser::new(match_literal_str("5")),
        parser::Parser::new(match_literal_str("4")),
        parser::Parser::new(match_literal_str("7")),
        parser::Parser::new(match_literal_str("8")),
        parser::Parser::new(match_literal_str("9")),
    ]
    .iter()
    .fold(
        parser::Parser::new(match_literal_str("0")),
        |x: Parser<Chars, Chars>, y| x.or_else::<Parser<Chars, Chars>>(y.clone()),
    );

    println!("{:?}", parse_digit.parse("23".chars()));

    let parse_digits = parse_digit.clone().zero_or_more();

    let parse_natural_numbers = parse_digits.clone().transform(|s| {
        let mut digits = String::from("");
        for digit in s {
            digits.push_str(digit.as_str());
        }
        digits.parse::<i32>().unwrap()
    });

    println!("{:?}", parse_natural_numbers.parse("23".chars()));

    let parse_ones = Parser::new(match_literal_str("1")).zero_or_more();
    println!("{:?}", parse_ones.parse("11111123".chars()));

    let parse_twos = Parser::new(match_literal_str("2")).zero_or_more();
    println!("{:?}", parse_twos.clone().parse("11111122".chars()));

    let parse_ones_then_twos = parse_ones.clone().pair(parse_twos.clone());
    println!("{:?}", parse_ones_then_twos.parse("11111122".chars()));

    let parse_ones_then_twos_first = parse_ones.clone().pair(parse_twos.clone()).first();
    println!("{:?}", parse_ones_then_twos_first.parse("11111122".chars()));

    let parse_ones_then_twos_second = parse_ones.pair(parse_twos).second();
    println!(
        "{:?}",
        parse_ones_then_twos_second.parse("11111122".chars())
    );

    let _any_parser = Parser::new(any);
}

//type ParseResult<'a, Input, Output> = Result<(Output, Input), String>;

pub fn any(mut input: Chars) -> ParseResult<Chars, char> {
    match input.next() {
        Some(next) => Ok((next, input)),
        _ => Err("error".to_string()),
    }
}
