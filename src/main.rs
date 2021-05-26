use parser::{match_literal, one_or_more, zero_or_more, Parse, Parser};

mod parser;

fn main() {
    println!("Hello, world!");
    let parse_digit = vec![
        parser::Parser::new(match_literal("1")),
        parser::Parser::new(match_literal("2")),
        parser::Parser::new(match_literal("3")),
        parser::Parser::new(match_literal("3")),
        parser::Parser::new(match_literal("5")),
        parser::Parser::new(match_literal("4")),
        parser::Parser::new(match_literal("7")),
        parser::Parser::new(match_literal("8")),
        parser::Parser::new(match_literal("9")),
    ]
    .iter()
    .fold(
        parser::Parser::new(match_literal("0")),
        |x: Parser<&str, &str>, y| x.or_else::<Parser<&str, &str>>(y.clone()),
    );

    println!("{:?}", parse_digit.parse("23"));

    let parse_digits = parse_digit.clone().zero_or_more();

    let parse_natural_numbers = parse_digits.clone().transform(|s| {
        let mut digits = String::from("");
        for digit in s {
            digits.push_str(digit);
        }
        digits.parse::<i32>().unwrap()
    });

    println!("{:?}", parse_natural_numbers.parse("23"));

    let parse_ones = Parser::new(match_literal("1")).zero_or_more();
    println!("{:?}", parse_ones.parse("11111123"));

    let parse_twos = Parser::new(match_literal("2")).zero_or_more();
    println!("{:?}", parse_twos.clone().parse("11111122"));

    let parse_ones_then_twos = parse_ones.clone().pair(parse_twos.clone());
    println!("{:?}", parse_ones_then_twos.parse("11111122"));

    let parse_ones_then_twos_first = parse_ones.clone().pair(parse_twos.clone()).first();
    println!("{:?}", parse_ones_then_twos_first.parse("11111122"));

    let parse_ones_then_twos_second = parse_ones.pair(parse_twos).second();
    println!("{:?}", parse_ones_then_twos_second.parse("11111122"));

    // let any_parser = move|input : &str|
    // {
    //     match input.chars().next() {
            
    //         Some(next) =>
    //         {
    //             //let res = &input[next.len_utf8()..];
    //             (Ok(next),"")
    //         }
    //         _ => {(Err("error"), input)},
    //     }
        
    // };

    let any_parser = Parser::new( any);
}

type ParseResult<'a, Input, Output> = (Result<Output, &'a str>, Input);
pub fn any(input: &str) -> ParseResult<&str, char> {
    match input.chars().next() {
        Some(next) => (Ok(next), &input[next.len_utf8()..]),
        _ => (Err("error"), input),
    }
}
