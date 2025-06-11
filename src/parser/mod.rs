pub mod token;
use token::*;

pub mod lexer;
use lexer::*;

pub mod symbols_table;
use symbols_table::*;

use std::collections::VecDeque;

use crate::ast::{
	root::*,
	statement::*,
	literal::*,
	expression::*
};

use crate::data::{
	syms::*,
	ops::*,
	vtype::*
};

pub struct ParserError
{
	pub message: String,
	pub line: usize,
	pub column_begin: usize,
	pub column_end: usize,
	pub context_line: String
}

pub struct ParserContext<'a>
{
	pub source: &'a str,
	pub tokens: VecDeque<Token>,
	pub symbols_table: SymbolsTable,
	pub errors: Vec<ParserError>
}

fn record_error(parser_context: &mut ParserContext, message: &str, info: &TokenInfo)
{
	parser_context.errors.push(
		ParserError
		{
			message: message.to_string(),
			line: info.line as usize,
			column_begin: info.column_begin as usize,
			column_end: info.column_end as usize,
			context_line: parser_context.source.lines().nth((info.line - 1) as usize).unwrap_or_default().to_string()
		}
	);
}

fn recover_token_stream(tokens: &mut VecDeque<Token>) -> ()
{
	while let Some(token) = tokens.pop_front()
	{
		match token.get_type()
		{
			TokenType::Symbol =>
			{
				let symbol_token = token.as_token::<SymbolToken>().unwrap();

				match symbol_token.sym()
				{
					Symbol::Semicolon | Symbol::RightBrace => { break; }

					_ => { continue; }
				}
			},
		
			_ => { continue; }
		}
	}
}

macro_rules! parser_error
{
	($context:expr, $info:expr, $fmt:literal $(, $args:expr)* $(,)?) =>
	{
		{
			record_error(
				$context,
				&format!($fmt $(, $args)*),
				&$info);

			recover_token_stream(&mut $context.tokens);

			return None;
		}
	};
}

macro_rules! next_token
{
	($context:expr, $last_token:expr, $expected_desc:expr) =>
	{{
		let token_opt = $context.tokens.pop_front();
		if token_opt.is_none()
		{
			record_error(
				$context,
				concat!("tokens should not end here, expected ", $expected_desc),
				&$last_token.info());
			
			$context.tokens.clear();
			
			return None;
		}
		token_opt.unwrap()
	}};
}

macro_rules! expect_token_type
{
	($context:expr, $token:expr, $ty:ty, $err_msg:literal $(, $args:expr)* $(,)?) =>
	{
		match $token.as_token::<$ty>()
		{
			Some(inner) => inner,
			None =>
			{
				record_error(
					$context,
					&format!($err_msg $(, $args)*),
					&$token.info());

				recover_token_stream(&mut $context.tokens);

				return None;
			}
		}
	};
}

fn parse_expression(parser_context: &mut ParserContext) -> Option<Expression>
{
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	enum Assoc
	{
		Left,
		Right
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	enum Operator
	{
		Add,
		Sub,
		Mul,
		Div,
		Eq,
		Neq,
		Gt,
		Gte,
		Lt,
		Lte,
		And,
		Or
	}

	fn precedence_of(op: Operator) -> (u8, Assoc)
	{
		use Operator::*;

		match op
		{
			Mul | Div             => (3, Assoc::Left),
			Add | Sub             => (2, Assoc::Left),
			Gt | Gte | Lt | Lte |
			Eq | Neq              => (1, Assoc::Left),
			And                   => (0, Assoc::Left),
			Or                    => (0, Assoc::Left)
		}
	}

	fn apply_operator(op: Operator, stack: &mut Vec<Expression>)
	{
		let rhs = stack.pop().expect("Missing RHS expression");
		let lhs = stack.pop().expect("Missing LHS expression");

		use Operator::*;

		match op
		{
			Add | Sub | Mul | Div =>
			{
				let arith_op = match op
				{
					Add => ArithmeticOperation::Add,
					Sub => ArithmeticOperation::Subtract,
					Mul => ArithmeticOperation::Multiply,
					Div => ArithmeticOperation::Divide,
					_   => unreachable!()
				};

				stack.push(Expression::new_arithmetic(
					VType::Integer,
					arith_op,
					lhs,
					rhs
				));
			}

			Eq | Neq | Gt | Gte | Lt | Lte =>
			{
				let cmp_op = match op
				{
					Eq  => ComparisonOperation::IsEqual,
					Neq => ComparisonOperation::IsNotEqual,
					Gt  => ComparisonOperation::IsGreater,
					Gte => ComparisonOperation::IsGreaterOrEqual,
					Lt  => ComparisonOperation::IsLess,
					Lte => ComparisonOperation::IsLessOrEqual,
					_   => unreachable!()
				};

				stack.push(Expression::new_comparison(
					cmp_op,
					lhs,
					rhs
				));
			}

			And | Or =>
			{
				let bool_op = match op
				{
					And => BooleanOperation::And,
					Or  => BooleanOperation::Or,
					_   => unreachable!()
				};

				stack.push(Expression::new_boolean(
					bool_op,
					lhs,
					rhs
				));
			}
		}
	}

	let mut output_stack: Vec<Expression> = Vec::new();
	let mut operator_stack: Vec<Operator> = Vec::new();

	let binding = parser_context.tokens.clone();
 	let first_token = binding.front();

	while let Some(token) = parser_context.tokens.pop_front()
	{
		match token.get_type()
		{
			TokenType::IntegerLiteral =>
			{
				let lit = token.as_token::<IntegerLiteralToken>().unwrap().clone();
				output_stack.push(Expression::new_literal(Literal::new_integer(lit.value())));
			}

			TokenType::BooleanLiteral =>
			{
				let lit = token.as_token::<BooleanLiteralToken>().unwrap().clone();
				output_stack.push(Expression::new_literal(Literal::new_boolean(lit.value())));
			}

			TokenType::Arithmetic =>
			{
				let arith_token = token.as_token::<ArithmeticToken>().unwrap().clone();
				let op = match arith_token.op()
				{
					ArithmeticOperation::Add      => Operator::Add,
					ArithmeticOperation::Subtract => Operator::Sub,
					ArithmeticOperation::Multiply => Operator::Mul,
					ArithmeticOperation::Divide   => Operator::Div
				};

				let (prec, assoc) = precedence_of(op);

				while let Some(&top_op) = operator_stack.last()
				{
					let (top_prec, _) = precedence_of(top_op);

					let should_apply = match assoc
					{
						Assoc::Left => prec <= top_prec,
						Assoc::Right => prec < top_prec
					};

					if should_apply
					{
						operator_stack.pop().map(|op| apply_operator(op, &mut output_stack));
					}
					else
					{
						break;
					}
				}

				operator_stack.push(op);
			}

			TokenType::Comparison =>
			{
				let cmp_token = token.as_token::<ComparisonToken>().unwrap().clone();
				let op = match cmp_token.op()
				{
					ComparisonOperation::IsEqual            => Operator::Eq,
					ComparisonOperation::IsNotEqual         => Operator::Neq,
					ComparisonOperation::IsGreater          => Operator::Gt,
					ComparisonOperation::IsGreaterOrEqual   => Operator::Gte,
					ComparisonOperation::IsLess             => Operator::Lt,
					ComparisonOperation::IsLessOrEqual      => Operator::Lte
				};

				let (prec, assoc) = precedence_of(op);

				while let Some(&top_op) = operator_stack.last()
				{
					let (top_prec, _) = precedence_of(top_op);

					let should_apply = match assoc
					{
						Assoc::Left => prec <= top_prec,
						Assoc::Right => prec < top_prec
					};

					if should_apply
					{
						operator_stack.pop().map(|op| apply_operator(op, &mut output_stack));
					}
					else
					{
						break;
					}
				}

				operator_stack.push(op);
			}

			TokenType::Boolean =>
			{
				let bool_token = token.as_token::<BooleanToken>().unwrap().clone();
				let op = match bool_token.op()
				{
					BooleanOperation::And => Operator::And,
					BooleanOperation::Or  => Operator::Or
				};

				let (prec, assoc) = precedence_of(op);

				while let Some(&top_op) = operator_stack.last()
				{
					let (top_prec, _) = precedence_of(top_op);

					let should_apply = match assoc
					{
						Assoc::Left => prec <= top_prec,
						Assoc::Right => prec < top_prec
					};

					if should_apply
					{
						operator_stack.pop().map(|op| apply_operator(op, &mut output_stack));
					}
					else
					{
						break;
					}
				}

				operator_stack.push(op);
			}

			TokenType::Symbol =>
			{
				let sym_token = token.as_token::<SymbolToken>().unwrap().clone();

				match sym_token.sym()
				{
					Symbol::LeftParen =>
					{
						let mut depth = 1;
						let mut sub_tokens = VecDeque::new();

						while let Some(next_token) = parser_context.tokens.pop_front()
						{
							if next_token.get_type() == TokenType::Symbol
							{
								let sub_sym_token = next_token.as_token::<SymbolToken>().unwrap();

								match sub_sym_token.sym()
								{
									Symbol::LeftParen => depth += 1,
									Symbol::RightParen =>
									{
										depth -= 1;
										if depth == 0
										{
											break;
										}
									}
									_ => {}
								}
							}

							sub_tokens.push_back(next_token);
						}

						if depth != 0
						{
							parser_error!(
								parser_context,
								sym_token.info(),
								"no close parenthesis found for expression"
							);
						}

						let mut inner_context = ParserContext
						{
							source: parser_context.source,
							tokens: sub_tokens,
							symbols_table: parser_context.symbols_table.clone(),
							errors: vec![]
						};

						let inner_expr = parse_expression(&mut inner_context);

						if inner_expr.is_none()
						{
							parser_error!(
								parser_context,
								sym_token.info(),
								"no inner expression parsed"
							);
						}

						parser_context.errors.append(&mut inner_context.errors);

						output_stack.push(inner_expr.unwrap());
					},

					Symbol::RightParen =>
					{
						parser_error!(
							parser_context,
							sym_token.info(),
							"expected a matched right parenthesis"
						);
					},

					_ =>
					{
						parser_error!(
							parser_context,
							sym_token.info(),
							"expected symbol `{:?}` at beginsing of expression, got `{:?}`",
							Symbol::LeftParen,
							sym_token.sym()
						);
					}
				}
			}

			TokenType::Identifier =>
			{
				let ident_token = token.as_token::<IdentifierToken>().unwrap();
				let name = ident_token.name();

				if name == "invoke"
				{
					let ident_token = next_token!(parser_context, ident_token, "an identifier token");
					let func_name = expect_token_type!(
						parser_context,
						ident_token,
						IdentifierToken,
						"expected identifier token after `invoke`"
					).name();

					let func_sign_opt = parser_context.symbols_table
						.get_function(&func_name);

					if func_sign_opt.is_none()
					{
						parser_error!(
							parser_context,
							ident_token.info(),
							"function `{}` not declared in the current module",
							func_name
						);
					}

					let func_sign = func_sign_opt.unwrap();

					let return_type = func_sign.return_type().clone();
					
					let begin_token: Token = next_token!(parser_context, ident_token, "a symbol token");
					let begin_token = expect_token_type!(
						parser_context,
						begin_token,
						SymbolToken,
						"expected a symbol token to begin param list"
					);
					
					if begin_token.sym() != Symbol::LeftParen
					{
						parser_error!(
							parser_context,
							begin_token.info(),
							"expected symbol `{:?}` to begin param list, got `{:?}`",
							Symbol::LeftParen,
							begin_token.sym()
						);
					}

					let mut depth = 1;
					let mut sub_tokens = VecDeque::new();

					while let Some(next_token) = parser_context.tokens.pop_front()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							let sub_sym_token = next_token.as_token::<SymbolToken>().unwrap();

							match sub_sym_token.sym()
							{
								Symbol::LeftParen => depth += 1,
								Symbol::RightParen =>
								{
									depth -= 1;
									if depth == 0
									{
										break;
									}
								}
								_ => {}
							}
						}

						sub_tokens.push_back(next_token);
					}

					if depth != 0
					{
						parser_error!(
							parser_context,
							begin_token.info(),
							"no close function param list"
						);
					}

					let mut expressions_passed: VecDeque<VecDeque<Token>> = VecDeque::new();
					let mut current_expression = VecDeque::new();

					while let Some(sub_token) = sub_tokens.pop_front()
					{
						if sub_token.get_type() == TokenType::Symbol
						{
							let sub_token = expect_token_type!(
								parser_context,
								sub_token,
								SymbolToken,
								"???"
							);

							if sub_token.sym() == Symbol::Comma
							{
								expressions_passed.push_back(current_expression.clone());
								current_expression.clear();
								continue;
							}
						}

						current_expression.push_back(sub_token);
					}

					if current_expression.len() != 0
					{
						expressions_passed.push_back(current_expression);
					}

					if expressions_passed.len() != func_sign.parameters().len()
					{
						parser_error!(
							parser_context,
							begin_token.info(),
							"mismatched argument length, expected {}, got {}",
							func_sign.parameters().len(),
							expressions_passed.len()
						);
					}

					let mut passed_arguments = VecDeque::new();

					for expr_tokens in expressions_passed
					{
						let mut expr_context = ParserContext
						{
							source: parser_context.source,
							tokens: expr_tokens.clone(),
							symbols_table: parser_context.symbols_table.clone(),
							errors: vec![]
						};

						let expr = parse_expression(&mut expr_context);

						if expr.is_none()
						{
							parser_error!(
								parser_context,
								expr_tokens[0].info(),
								"no expression parsed for argument"
							);
						}

						parser_context.errors.append(&mut expr_context.errors);

						passed_arguments.push_back(expr.unwrap());
					}

					let function_call_expr = Expression::new_function_call(return_type, func_name, passed_arguments);
					output_stack.push(function_call_expr);
				}
				else
				{
					let id = parser_context.symbols_table.get_id(&name);

					if id.is_none()
					{
						parser_error!(
							parser_context,
							token.info(),
							"identifier `{}` not declared in the current scope",
							name
						);
					}

					let id = id.unwrap();
	
					let vtype = parser_context.symbols_table.lookup(&name).unwrap();

					let var_ref_expr = Expression::new_variable(vtype.clone(), id);
					output_stack.push(var_ref_expr);
				}
			}

			_ =>
			{
				parser_error!(
					parser_context,
					token.info(),
					"unexpected token `{:?}` in expression",
					token.get_type()
				);
			}
		}
	}

	while let Some(op) = operator_stack.pop()
	{
		apply_operator(op, &mut output_stack);
	}

	if output_stack.len() != 1
	{
		parser_error!(
			parser_context,
			first_token.unwrap().info(),
			"shunting yard algorithm failed, stack expected to finish with one expression, got {}",
			output_stack.len()
		);
	}

	output_stack.pop()
}

pub fn parse_statement(parser_context: &mut ParserContext, manage_scope: bool) -> Option<Statement>
{
	while let Some(t) = parser_context.tokens.pop_front()
	{
		match t.get_type()
		{
			token::TokenType::Symbol =>
			{
				let t = t.as_token::<SymbolToken>().unwrap().clone();

				if t.sym() != Symbol::LeftBrace
				{
					parser_error!(
						parser_context,
						t.info(),
						"expected symbol `{:?}` when beginning a statement, got `{:?}`",
						Symbol::LeftBrace,
						t.sym()
					);
				}

				let mut depth = 1;
				let mut sub_tokens: VecDeque<Token> = VecDeque::new();

				while let Some(next_token) = parser_context.tokens.pop_front()
				{
					if next_token.get_type() == TokenType::Symbol
					{
						let sub_sym_token = next_token.as_token::<SymbolToken>().unwrap();

						match sub_sym_token.sym()
						{
							Symbol::LeftBrace => depth += 1,
							Symbol::RightBrace =>
							{
								depth -= 1;
								if depth == 0
								{
									break;
								}
							}
							_ => {}
						}
					}

					sub_tokens.push_back(next_token);
				}

				if depth != 0
				{
					parser_error!(
						parser_context,
						t.info(),
						"no close braces found for compound statement"
					);
				}

				let mut sub_context = ParserContext
				{
					source: parser_context.source,
					tokens: sub_tokens,
					symbols_table: parser_context.symbols_table.clone(),
					errors: vec![],
				};

				if manage_scope
				{
					sub_context.symbols_table.push_scope();
				}

				let mut statements = VecDeque::new();


				while !sub_context.tokens.is_empty()
				{
					let result = parse_statement(&mut sub_context, true);

					if result.is_some()
					{
						statements.push_back(result.unwrap());
					}
				}

				if manage_scope
				{
					sub_context.symbols_table.pop_scope();
				}

				parser_context.errors.append(&mut sub_context.errors);
				parser_context.symbols_table = sub_context.symbols_table;

				let statement = Statement::new_compound(statements);

				return Some(statement);
			},

			token::TokenType::Identifier =>
			{
				let t = t.as_token::<IdentifierToken>().unwrap().clone();

				if t.name() == "function"
				{
					if parser_context.symbols_table.scope() != 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"function declaration or definition is not allowed here"
						);
					}

					let t_name = next_token!(parser_context, t, "an identifier token");

					let func_name = expect_token_type!(
						parser_context,
						t_name,
						IdentifierToken,
						"expected identifier token after `function`"
					).name();

					let t_sym = next_token!(parser_context, t_name, "a symbol token");

					let symbol = expect_token_type!(
						parser_context,
						t_sym,
						SymbolToken,
						"expected a symbol token after function identifier in signature"
					).sym();

					if symbol != Symbol::LeftParen
					{
						parser_error!(
							parser_context,
							t_sym.info(),
							"expected symbol `{:?}` to begin parameter list in function signature, got `{:?}`",
							Symbol::LeftParen,
							symbol
						);
					}

					let mut sub_tokens = VecDeque::new();

					while let Some(next_token) = parser_context.tokens.pop_front()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							let sub_sym_token = next_token.as_token::<SymbolToken>().unwrap();

							if sub_sym_token.sym() == Symbol::RightParen
							{
								break;
							}
						}

						sub_tokens.push_back(next_token);
					}

					let mut parameters = VecDeque::new();

					parser_context.symbols_table.push_scope();

					while let Some(sub_token) = sub_tokens.pop_front()
					{
						let param_id = expect_token_type!(
							parser_context,
							sub_token,
							IdentifierToken,
							"expected identifier token"
						).name();

						let type_token_opt = sub_tokens.pop_front();
						let type_token;

						if type_token_opt.is_none()
						{
							record_error(
								parser_context,
								"tokens should not end here, expected a type token",
								&sub_token.info());
							
							parser_context.tokens.clear();

							return None;
						}
						else
						{
							type_token = type_token_opt.unwrap();
						}

						let param_vtype = expect_token_type!(
							parser_context,
							type_token,
							TypeToken,
							"expected type token after param identifier `{}`",
							param_id
						).vtype();

						let maybe_comma_token = sub_tokens.pop_front();

						if maybe_comma_token.is_some()
						{
							let comma_token = expect_token_type!(
								parser_context,
								maybe_comma_token.as_ref().unwrap(),
								SymbolToken,
								"expected symbol token after param type, got `{:?}`",
								maybe_comma_token.as_ref().unwrap().get_type()
							);

							let sym = comma_token.sym();

							if sym != Symbol::Comma
							{
								parser_error!(
									parser_context,
									comma_token.info(),
									"expected symbol `{:?}` to end param entry, got `{:?}`",
									Symbol::Comma,
									sym
								);
							}
						}

						parser_context.symbols_table.define(&param_id, param_vtype.clone());

						let param = Parameter::new(
							parser_context.symbols_table.get_id(&param_id).unwrap(),
							param_vtype.clone()
						);

						parameters.push_back(param);
					}

					let t_type_token = next_token!(parser_context, t_name, "a type token");

					let vtype = expect_token_type!(
						parser_context,
						t_type_token,
						TypeToken,
						"expected type token to end function signature with identifier `{}`",
						func_name
					).vtype();

					parser_context.symbols_table.define_function(&func_name, vtype.clone(), parameters.clone());

					let next_token = next_token!(parser_context, t_name, "a symbol token");
					let sym = expect_token_type!(
						parser_context,
						next_token,
						SymbolToken,
						"expected a symbol token after function signature"
					).sym();
					
					let func_sign = FunctionSignature::new(func_name, vtype.clone(), parameters);

					match sym
					{
						Symbol::Semicolon =>
						{
							parser_context.symbols_table.pop_scope();

							let func_declare_statement = Statement::new_function_declare(func_sign);

							return Some(func_declare_statement);
						},

						Symbol::LeftBrace =>
						{
							parser_context.tokens.push_front(next_token); // reinsert token
							let result = parse_statement(parser_context, false);

							let func_define_statement = Statement::new_function_define(
								func_sign,
								result
									.unwrap()
									.as_statement::<CompoundStatement>()
									.expect("Expected compound statement")
									.clone()
							);
							
							parser_context.symbols_table.pop_scope();
		
							return Some(func_define_statement);
						},

						_ =>
						{
							parser_error!(
								parser_context,
								next_token.info(),
								"expected symbol `{:?}` or `{:?}` after function signature, got `{:?}`",
								Symbol::Semicolon,
								Symbol::LeftBrace,
								sym
							);
						}
					}
				}
				else if t.name() == "return"
				{
					if parser_context.symbols_table.scope() == 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"`return` statement is not allowed here"
						);
					}

					let mut expr_tokens = VecDeque::new();

					loop
					{
						let Some(next_token) = parser_context.tokens.front() else
						{
							break;
						};

						if next_token.get_type() == TokenType::Symbol
						{
							let sym_token_opt = next_token.as_token::<SymbolToken>();

							if let Some(sym_token) = sym_token_opt
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									parser_context.tokens.pop_front(); // consume ;
									break;
								}
							}
						}

						if let Some(token) = parser_context.tokens.pop_front()
						{
							expr_tokens.push_back(token);
						}
					}

					let expr;

					if expr_tokens.len() == 0
					{
						expr = None;
					}
					else
					{
						let mut expr_context = ParserContext
						{
							source: parser_context.source,
							tokens: expr_tokens,
							symbols_table: parser_context.symbols_table.clone(),
							errors: vec![]
						};

						expr = parse_expression(&mut expr_context);

						parser_context.errors.append(&mut expr_context.errors);

						if expr.is_none()
						{
							parser_error!(
								parser_context,
								t.info(),
								"no expression parsed for `return` statement"
							);
						}
					}

					return Some(Statement::new_function_return(expr));
				}
				else if t.name() == "let"
				{
					if parser_context.symbols_table.scope() == 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"`let` statement is not allowed here"
						);
					}

					// Variable declaration: let <name> <type> = <expr>;
					let t_name = next_token!(parser_context, t, "an identifier token");
					let i_name = expect_token_type!(
						parser_context,
						t_name,
						IdentifierToken,
						"expected identifier token after `let`"
					).name();
					
					let t_type_token = next_token!(parser_context, t_name, "a type token");
					let vtype = expect_token_type!(
						parser_context,
						t_type_token,
						TypeToken,
						"expected a type token after identifier `{}`",
						i_name
					).vtype();
					
					if vtype == VType::Void
					{
						parser_error!(
							parser_context,
							t_type_token.info(),
							"variable `{}` has incomplete type `void`",
							i_name
						);
					}
						
					let eq_token = next_token!(parser_context, t_type_token, "'=' after type token");
					let sym = expect_token_type!(
						parser_context,
						eq_token,
						SymbolToken,
						"expected symbol `=` after type"
					).sym();

					if sym != Symbol::Equal
					{
						parser_error!(
							parser_context,
							eq_token.info(),
							"expected symbol `{:?}` after type in `let` statement, got `{:?}`",
							Symbol::Equal,
							sym
						);
					}

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					while let Some(next_token) = parser_context.tokens.front()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							if let Some(sym_token) = next_token.as_token::<SymbolToken>()
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									parser_context.tokens.pop_front(); // consume ;
									break;
								}
							}
						}

						if let Some(tok) = parser_context.tokens.pop_front()
						{
							expr_tokens.push_back(tok);
						}
					}

					let mut expr_context = ParserContext
					{
						source: parser_context.source,
						tokens: expr_tokens,
						symbols_table: parser_context.symbols_table.clone(),
						errors: vec![]
					};

					let expr = parse_expression(&mut expr_context);

					parser_context.errors.append(&mut expr_context.errors);

					parser_context.symbols_table.define(&i_name, vtype.clone());

					if expr.is_none()
					{
						parser_error!(
							parser_context,
							t.info(),
							"no expression parsed for `let` statement"
						);
					}

					let statement = Statement::new_declare(vtype, parser_context.symbols_table.get_id(&i_name).unwrap(), expr.unwrap());

					return Some(statement);
				}
				else if t.name() == "set"
				{
					if parser_context.symbols_table.scope() == 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"`set` statement is not allowed here"
						);
					}

					// Variable assignment: set <name> = <expr>;
					let t_name = next_token!(parser_context, t, "an identifier token");
					let i_name = expect_token_type!(
						parser_context,
						t_name,
						IdentifierToken,
						"expected identifier token after `set`"
					).name();

					let id = parser_context.symbols_table.get_id(&i_name);

					if id.is_none()
					{
						parser_error!(
							parser_context,
							t_name.info(),
							"identifier `{}` not declared in the current scope.",
							i_name
						);
					}

					let id = id.unwrap();

					let eq_token = next_token!(parser_context, t_name, "'=' after identifier token");
					let sym = expect_token_type!(
						parser_context,
						eq_token,
						SymbolToken,
						"expected symbol `=` after identifier"
					).sym();

					if sym != Symbol::Equal
					{
						parser_error!(
							parser_context,
							eq_token.info(),
							"expected symbol `{:?}` after identifier in `set` statement, got `{:?}`",
							Symbol::Equal,
							sym
						);
					}

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					while let Some(next_token) = parser_context.tokens.front()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							if let Some(sym_token) = next_token.as_token::<SymbolToken>()
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									parser_context.tokens.pop_front(); // consume ;
									break;
								}
							}
						}

						if let Some(tok) = parser_context.tokens.pop_front()
						{
							expr_tokens.push_back(tok);
						}
					}

					let mut expr_context = ParserContext
					{
						source: parser_context.source,
						tokens: expr_tokens,
						symbols_table: parser_context.symbols_table.clone(),
						errors: vec![]
					};

					let expr = parse_expression(&mut expr_context);

					parser_context.errors.append(&mut expr_context.errors);

					if expr.is_none()
					{
						parser_error!(
							parser_context,
							t.info(),
							"no expression parsed for `set` statement"
						);
					}

					let statement = Statement::new_assign(id, expr.unwrap());

					return Some(statement);
				}
				else if t.name() == "print"
				{
					if parser_context.symbols_table.scope() == 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"`print` statement is not allowed here"
						);
					}

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					loop
					{
						let Some(next_token) = parser_context.tokens.front() else
						{
							break;
						};

						if next_token.get_type() == TokenType::Symbol
						{
							let sym_token_opt = next_token.as_token::<SymbolToken>();

							if let Some(sym_token) = sym_token_opt
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									parser_context.tokens.pop_front(); // consume ;
									break;
								}
							}
						}

						if let Some(token) = parser_context.tokens.pop_front()
						{
							expr_tokens.push_back(token);
						}
					}

					let mut expr_context = ParserContext
					{
						source: parser_context.source,
						tokens: expr_tokens,
						symbols_table: parser_context.symbols_table.clone(),
						errors: vec![]
					};

					let expr = parse_expression(&mut expr_context);

					parser_context.errors.append(&mut expr_context.errors);

					if expr.is_none()
					{
						parser_error!(
							parser_context,
							t.info(),
							"no expression parsed for `print` statement"
						);
					}

					return Some(Statement::new_print(expr.unwrap()));
				}
				else if t.name() == "express"
				{
					if parser_context.symbols_table.scope() == 1
					{
						parser_error!(
							parser_context,
							t.info(),
							"`express` statement is not allowed here"
						);
					}

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					loop
					{
						let Some(next_token) = parser_context.tokens.front() else
						{
							break;
						};

						if next_token.get_type() == TokenType::Symbol
						{
							let sym_token_opt = next_token.as_token::<SymbolToken>();

							if let Some(sym_token) = sym_token_opt
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									parser_context.tokens.pop_front(); // consume ;
									break;
								}
							}
						}

						if let Some(token) = parser_context.tokens.pop_front()
						{
							expr_tokens.push_back(token);
						}
					}

					let mut expr_context = ParserContext
					{
						source: parser_context.source,
						tokens: expr_tokens,
						symbols_table: parser_context.symbols_table.clone(),
						errors: vec![]
					};

					let expr = parse_expression(&mut expr_context);

					parser_context.errors.append(&mut expr_context.errors);

					if expr.is_none()
					{
						parser_error!(
							parser_context,
							t.info(),
							"no expression parsed for `express` statement"
						);
					}

					return Some(Statement::new_expression(expr.unwrap()));
				}
				else
				{
					parser_error!(
						parser_context,
						t.info(),
						"unexpected identifier `{}` when beginning a statement",
						t.name()
					);
				}
			}

			_ => 
			{
				parser_error!(
					parser_context,
					t.info(),
					"unexpected token `{:?}` when beginning a statement",
					t.get_type()
				);
			}
		}
	}

	panic!();
}

pub fn parse_root(source: String) -> (Root, Vec<ParserError>)
{
	let ref_source = source.as_str();

	let tokens = lex(ref_source.into());
	let mut root = Root::new();

	let mut parser_context = ParserContext
	{
		source: ref_source,
		tokens: tokens,
		symbols_table: SymbolsTable::new(),
		errors: Vec::new()
	};

	parser_context.symbols_table.push_scope();

	while !parser_context.tokens.is_empty()
	{
		let result = parse_statement(&mut parser_context, true);
		
		if result.is_some()
		{
			root.add(result.unwrap());
		}
	}

	(root, parser_context.errors)
}
