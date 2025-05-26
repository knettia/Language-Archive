pub mod token;
use token::*;

pub mod lexer;
use lexer::*;

pub mod symbols_table;
use symbols_table::*;

use std::collections::VecDeque;

use crate::ast::root::*;
use crate::ast::statement::*;
use crate::ast::literal::*;
use crate::ast::expression::*;

use crate::data::syms::*;
use crate::data::ops::*;
use crate::data::vtype::VType;

// Internal operator enum for shunting yard
fn parse_expression(symbols_table: &mut SymbolsTable, mut tokens: VecDeque<Token>) -> Expression
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

	while let Some(token) = tokens.pop_front()
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
						let mut sub_tokens: VecDeque<Token> = VecDeque::new();

						while let Some(next_token) = tokens.pop_front()
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
							panic!("Unclosed parenthesis in expression");
						}

						let inner_expr = parse_expression(symbols_table, sub_tokens);
						output_stack.push(inner_expr);
					}

					Symbol::RightParen =>
					{
						panic!("Unmatched right parenthesis");
					}

					_ => panic!("Unexpected symbol in expression: {:?}", sym_token.sym())
				}
			}

			TokenType::Identifier =>
			{
				let ident_token = token.as_token::<IdentifierToken>().unwrap();
				let name = ident_token.name();

				let id = symbols_table
					.get_id(&name)
					.unwrap_or_else(|| panic!("Undefined variable reference: `{}`", name));

				let vtype = symbols_table
					.lookup(&name)
					.unwrap_or_else(|| panic!("Undefined variable reference: `{}`", name));

				let var_ref_expr = Expression::new_variable(vtype.clone(), id);
				output_stack.push(var_ref_expr);
			}

			_ => panic!("Unexpected token in expression: {:?}", token.get_type())
		}
	}

	while let Some(op) = operator_stack.pop()
	{
		apply_operator(op, &mut output_stack);
	}

	assert_eq!(output_stack.len(), 1, "Expression stack must end with exactly one value");

	output_stack.pop().unwrap()
}


pub struct StatementReturn
{
	statement: Statement,
	remaining_tokens: VecDeque<Token>,
}

pub fn parse_statement(symbols_table: &mut SymbolsTable, stmt_tokens: VecDeque<Token>) -> StatementReturn
{
	let mut tokens = stmt_tokens.into_iter().peekable();

	while let Some(t) = tokens.next()
	{
		match t.get_type()
		{
			token::TokenType::Identifier =>
			{
				let t = t.as_token::<IdentifierToken>().unwrap().clone();

				if t.name() == "let"
				{
					// Variable declaration: let <name> <type> = <expr>;
					let t_name = tokens.next().expect("Expected identifier token after `let`");
					let t_name = t_name
						.as_token::<IdentifierToken>()
						.expect("Expected identifier token")
						.name();

					let t_type_token = tokens.next().expect(format!("Expected type after identifier `{}`", t_name).as_str());
					let vtype = t_type_token
						.as_token::<TypeToken>()
						.expect("Expected a type token")
						.vtype();

					let eq_token = tokens.next().expect("Expected '=' after type");
					let sym = eq_token
						.as_token::<SymbolToken>()
						.expect("Expected symbol token for '='")
						.sym();

					assert_eq!(sym, Symbol::Equal, "Expected '=' symbol after type");

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					while let Some(next_token) = tokens.peek()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							if let Some(sym_token) = next_token.as_token::<SymbolToken>()
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									tokens.next(); // consume ;
									break;
								}
							}
						}

						if let Some(tok) = tokens.next()
						{
							expr_tokens.push_back(tok);
						}
					}

					let expr = parse_expression(symbols_table, expr_tokens);

					symbols_table.define(&t_name, vtype.clone());

					let statement = Statement::new_declare(vtype, symbols_table.get_id(&t_name).unwrap(), expr);

					return StatementReturn
					{
						statement,
						remaining_tokens: tokens.collect()
					};
				}
				else if t.name() == "set"
				{
					// Variable assignment: set <name> = <expr>;
					let t_name = tokens.next().expect("Expected identifier after `set`");
					let t_name = t_name
						.as_token::<IdentifierToken>()
						.expect("Expected identifier token")
						.name();

					let id = symbols_table
						.get_id(&t_name)
						.unwrap_or_else(|| panic!("Symbol `{}` not found in current scope", t_name));

					let eq_token = tokens.next().expect("Expected '=' after identifier");
					let sym = eq_token
						.as_token::<SymbolToken>()
						.expect("Expected symbol token for '='")
						.sym();
					assert_eq!(sym, Symbol::Equal, "Expected '=' symbol");

					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					while let Some(next_token) = tokens.peek()
					{
						if next_token.get_type() == TokenType::Symbol
						{
							if let Some(sym_token) = next_token.as_token::<SymbolToken>()
							{
								if sym_token.sym() == Symbol::Semicolon
								{
									tokens.next(); // consume ;
									break;
								}
							}
						}

						if let Some(tok) = tokens.next()
						{
							expr_tokens.push_back(tok);
						}
					}

					let expr = parse_expression(symbols_table, expr_tokens);

					let statement = Statement::new_assign(id, expr);

					return StatementReturn
					{
						statement,
						remaining_tokens: tokens.collect()
					};
				}
				else if t.name() == "print"
				{
					let mut expr_tokens: VecDeque<Token> = VecDeque::new();

					loop
					{
						let Some(next_token) = tokens.peek() else
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
									tokens.next(); // consume ;
									break;
								}
							}
						}

						if let Some(token) = tokens.next()
						{
							expr_tokens.push_back(token);
						}
					}

					let expr = parse_expression(symbols_table, expr_tokens);

					return StatementReturn
					{
						statement: Statement::new_print(expr),
						remaining_tokens: tokens.collect()
					}
				}
				else
				{
					panic!("Unexpected identifier `{}`", t.name());
				}
			}

			_ => panic!("Unexpected starting statement token `{:?}`", t.get_type())
		}
	}

	panic!();
}

pub fn parse_root(source: String) -> Root
{
	let tokens = lex(source);
	let mut root = Root::new();

	let mut token_queue = tokens.into_iter().collect::<VecDeque<_>>();

	let mut symbols_table = SymbolsTable::new();

	symbols_table.push_scope();

	while !token_queue.is_empty()
	{
		let result = parse_statement(&mut symbols_table, token_queue.clone());

		root.add(result.statement);
		token_queue = result.remaining_tokens;
	}

	root
}
