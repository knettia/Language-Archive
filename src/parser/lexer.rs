use std::collections::VecDeque;

use crate::data::vtype::VType;
use crate::data::syms::Symbol;
use crate::data::ops::*;

use super::token::*;

pub fn lex(data: String) -> VecDeque<Token>
{
	let mut chars = data.chars().peekable();

	let mut line: i16 = 1;
	let mut column: i16 = 1;

	let mut tokens: VecDeque<Token> = VecDeque::new();

	while let Some(c) = chars.next()
	{
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

			'+' | '-' | '*' | '/' =>
			{
				let info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column + 1
				};

				match c
				{
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

					_ => unreachable!()
				}

			}

			'0'..='9' =>
			{
				let column_begin = column;

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

				let info = TokenInfo
				{
					line,
					column_begin: column_begin,
					column_end: column
				};

				if let Ok(val) = num.parse::<i32>()
				{
					tokens.push_back(Token::new_integer_literal(info, val));
				}
			}

			'a'..='z' | 'A'..='Z' | '_' =>
			{
				let column_begin = column;

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

				let info = TokenInfo
				{
					line,
					column_begin: column_begin,
					column_end: column
				};

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
				let mut info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column
				};

				if *chars.peek().unwrap() == '='
				{
					column += 1;
					info.column_end = column;

					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsEqual));
					chars.next();
				}
				else
				{
					tokens.push_back(Token::new_symbol(info, Symbol::Equal));
				}
			}

			'!' =>
			{
				let mut info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column
				};

				if *chars.peek().unwrap() == '='
				{
					column += 1;
					info.column_end = column;

					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsNotEqual));
					chars.next();
				}
				else 
				{
					tokens.push_back(Token::new_symbol(info, Symbol::Bang));
				}
			}

			'<' =>
			{
				let mut info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column
				};

				if *chars.peek().unwrap() == '='
				{
					column += 1;
					info.column_end = column;
					
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLessOrEqual));
					chars.next();
				}
				else
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsLess));
				}
			}

			'>' =>
			{
				let mut info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column
				};

				if *chars.peek().unwrap() == '='
				{
					column += 1;
					info.column_end = column;
					
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreaterOrEqual));
					chars.next();
				}
				else
				{
					tokens.push_back(Token::new_comparison(info, ComparisonOperation::IsGreater));
				}
			}

			'(' | ')' | '{' | '}' | ';' =>
			{
				let info = TokenInfo
				{
					line,
					column_begin: column,
					column_end: column + 1
				};

				match c
				{
					'(' =>
					{
						tokens.push_back(Token::new_symbol(info, Symbol::LeftParen));
					}
		
					')' =>
					{
						tokens.push_back(Token::new_symbol(info, Symbol::RightParen));
					}
		
					'{' =>
					{
						tokens.push_back(Token::new_symbol(info, Symbol::LeftBrace));
					}
		
					'}' =>
					{
						tokens.push_back(Token::new_symbol(info, Symbol::RightBrace));
					}
		
					';' =>
					{
						tokens.push_back(Token::new_symbol(info, Symbol::Semicolon));
					}

					_ => unreachable!()
				}
			}

			unknown =>
			{
				panic!("Unknown character `{unknown}`: line {line}, column {column}");
			}
		}
	}

	return tokens;
}
