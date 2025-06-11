use std::collections::VecDeque;

use crate::data::{
	vtype::VType,
	syms::Symbol,
	ops::*
};

use super::token::*;

pub fn lex(data: String) -> VecDeque<Token>
{
	let mut chars = data.chars().peekable();

	let mut line: usize = 1;
	let mut column: usize = 1;

	let mut tokens: VecDeque<Token> = VecDeque::new();

	fn make_info(line: usize, column_begin: usize, token_len: usize) -> TokenInfo
	{
		TokenInfo
		{
			line,
			column_begin,
			column_end: column_begin + token_len - 1
		}
	}

	while let Some(c) = chars.next()
	{
		match c
		{
			' ' | '\t' =>
			{
				column += 1;
				continue;
			}

			'\n' =>
			{
				line += 1;
				column = 1;
			}

			'+' | '-' | '*' | '/' =>
			{
				let info = make_info(line, column, 1);
				column += 1;

				match c
				{
					'+' => tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Add)),
					'-' => tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Subtract)),
					'*' => tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Multiply)),
					'/' => tokens.push_back(Token::new_arithmetic(info, ArithmeticOperation::Divide)),
					_ => unreachable!()
				}
			}

			'0'..='9' =>
			{
				let mut num = String::new();
				num.push(c);

				let mut token_len = 1;

				while let Some(current) = chars.peek()
				{
					if !current.is_ascii_digit()
					{
						break;
					}
					num.push(*current);
					chars.next();
					token_len += 1;
				}

				let info = make_info(line, column, token_len);
				column += token_len;

				if let Ok(val) = num.parse::<i32>()
				{
					tokens.push_back(Token::new_integer_literal(info, val));
				}
			}

			'a'..='z' | 'A'..='Z' | '_' =>
			{
				let mut ident = String::new();
				ident.push(c);

				let mut token_len = 1;

				while let Some(current) = chars.peek()
				{
					if !(current.is_ascii_alphanumeric() || *current == '_')
					{
						break;
					}
					ident.push(*current);
					chars.next();
					token_len += 1;
				}

				let info = make_info(line, column, token_len);
				column += token_len;

				match ident.as_str()
				{
					"true" => tokens.push_back(Token::new_boolean_literal(info, true)),
					"false" => tokens.push_back(Token::new_boolean_literal(info, false)),

					"void" => tokens.push_back(Token::new_type(info, VType::Void)),
					"int" => tokens.push_back(Token::new_type(info, VType::Integer)),
					"bool" => tokens.push_back(Token::new_type(info, VType::Boolean)),

					"and" => tokens.push_back(Token::new_boolean(info, BooleanOperation::And)),
					"or" => tokens.push_back(Token::new_boolean(info, BooleanOperation::Or)),

					_ => tokens.push_back(Token::new_identifier(info, ident))
				}
			}

			'=' =>
			{
				let mut token_len = 1;

				if let Some('=') = chars.peek()
				{
					chars.next();
					token_len += 1;
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsEqual));
				}
				else
				{
					let info = make_info(line, column, 1);
					tokens.push_back(Token::new_symbol(info, Symbol::Equal));
				}

				column += token_len;
			}

			'!' =>
			{
				let mut token_len = 1;

				if let Some('=') = chars.peek()
				{
					chars.next();
					token_len += 1;
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsNotEqual));
				}
				else
				{
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_symbol(info, Symbol::Bang));
				}

				column += token_len;
			}

			'<' =>
			{
				let mut token_len = 1;

				if let Some('=') = chars.peek()
				{
					chars.next();
					token_len += 1;
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLessOrEqual));
				}
				else
				{
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLess));
				}

				column += token_len;
			}

			'>' =>
			{
				let mut token_len = 1;

				if let Some('=') = chars.peek()
				{
					chars.next();
					token_len += 1;
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreaterOrEqual));
				}
				else
				{
					let info = make_info(line, column, token_len);
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreater));
				}
				
				column += token_len;
			}

			'(' | ')' | '{' | '}' | ';' =>
			{
				let info = make_info(line, column, 1);
				column += 1;

				match c
				{
					'(' => tokens.push_back(Token::new_symbol(info, Symbol::LeftParen)),
					')' => tokens.push_back(Token::new_symbol(info, Symbol::RightParen)),
					'{' => tokens.push_back(Token::new_symbol(info, Symbol::LeftBrace)),
					'}' => tokens.push_back(Token::new_symbol(info, Symbol::RightBrace)),
					';' => tokens.push_back(Token::new_symbol(info, Symbol::Semicolon)),
					_ => unreachable!()
				}
			}

			unknown =>
			{
				panic!("Unknown character `{unknown}`: line {line}, column {column}");
			}
		}
	}

	tokens
}
