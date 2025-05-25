use std::collections::VecDeque;

use crate::data::syms::Symbol;
use crate::data::ops::*;
// use crate::data::;
use super::token::*;

// enum SyntaxType
// {
// 	CLike,
// 	RustLike
// }

// struct Options
// {
// 	syntax_type: SyntaxType
// }

// enum Result
// {
// 	Success
// }

// struct Output
// {
// 	result: Result,
// 	string: String,
// 	tokens: Vec<Token>
// }

pub fn lex(data: String) -> VecDeque<Token>
{
	let mut chars = data.chars().peekable();

	let mut line: i16 = 1;
	let mut column: i16 = 1;

	let mut tokens: VecDeque<Token> = VecDeque::new();

	while let Some(c) = chars.next()
	{
		let info = TokenInfo
		{
			line,
			column
		};

		column += 1;

		match c
		{
			' ' | '\t' =>
			{
				continue;
			}

			'\n' =>
			{
				line += 1;
				column = 1;
			}

			'+' =>
			{
				tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Add));
			}

			'-' =>
			{
				tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Subtract));
			}

			'*' =>
			{
				tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Multiply));
			}

			'/' =>
			{
				tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Divide));
			}

			'0'..='9' =>
			{
				let mut num = String::new();
				num.push(c);

				while let Some(current) = chars.peek()
				{
					if !current.is_ascii_digit()
					{
						break;
					}

					num.push(*current);
					chars.next();
					column += 1;
				}

				if let Ok(val) = num.parse::<i32>()
				{
					tokens.push_back(Token::new_integer_literal(info, val));
				}
			}

			'a'..='z' | 'A'..='Z' | '_' =>
			{
				let mut ident = String::new();
				ident.push(c); // include the first character

				while let Some(current) = chars.peek()
				{
					if !(current.is_ascii_alphanumeric() || *current == '_')
					{
						break;
					}

					ident.push(*current);
					chars.next();
					column += 1;
				}

				match ident.as_str()
				{
					"true" => tokens.push_back(Token::new_boolean_literal(info, true)),
					"false" => tokens.push_back(Token::new_boolean_literal(info, false)),

					"and" => tokens.push_back(Token::new_boolean(info, BooleanOperation::And)),
					"or" => tokens.push_back(Token::new_boolean(info, BooleanOperation::Or)),

					_ => tokens.push_back(Token::new_identifier(info, ident))
				}
			}


			'=' =>
			{
				if *chars.peek().unwrap() == '='
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsEqual));
					chars.next();
					column += 1;
				}
				else
				{
					tokens.push_back(Token::new_symbol(info, Symbol::Equal));
				}
			}

			'!' =>
			{
				if *chars.peek().unwrap() == '='
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsNotEqual));
					chars.next();
					column += 1;
				}
				else 
				{
					tokens.push_back(Token::new_symbol(info, Symbol::Bang));
				}
			}

			'<' =>
			{
				if *chars.peek().unwrap() == '='
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLessOrEqual));
					chars.next();
					column += 1;
				}
				else
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLess));
				}
			}

			'>' =>
			{
				if *chars.peek().unwrap() == '='
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreaterOrEqual));
					chars.next();
					column += 1;
				}
				else
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreater));
				}
			}

			'(' =>
			{
				tokens.push_back(Token::new_symbol(info, Symbol::LeftParen));
			}

			')' =>
			{
				tokens.push_back(Token::new_symbol(info, Symbol::RightParen));
			}

			';' =>
			{
				tokens.push_back(Token::new_symbol(info, Symbol::Semicolon));
			}

			unknown =>
			{
				panic!("Unknown character `{unknown}`: line {line}, column {column}");
			}
		}
	}

	return tokens;
}
