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

    let deneme = Parser::new(match_literal("1")).zero_or_more().zero_or_more();
    println!("{:?}", deneme.parse("11111123"));
}
