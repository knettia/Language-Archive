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

pub fn parse_statement(symbols_table: &mut SymbolsTable, stmt_tokens: VecDeque<Token>, manage_scope: bool) -> StatementReturn
{
	let mut tokens = stmt_tokens.into_iter().peekable();

	while let Some(t) = tokens.next()
	{
		match t.get_type()
		{
			token::TokenType::Symbol =>
			{
				let t = t.as_token::<SymbolToken>().unwrap().clone();

				if t.sym() != Symbol::LeftBrace
				{
					panic!("Unexpected token `{:?}`", t.sym());
				}

				let mut depth = 1;
				let mut sub_tokens: VecDeque<Token> = VecDeque::new();

				while let Some(next_token) = tokens.next()
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
					panic!("Unclosed brace");
				}


				if manage_scope
				{
					symbols_table.push_scope();
				}

				let mut statements = VecDeque::new();

				while !sub_tokens.is_empty()
				{
					let result = parse_statement(symbols_table, sub_tokens.clone(), true);

					statements.push_back(result.statement);
					sub_tokens = result.remaining_tokens;
				}
				
				if manage_scope
				{
					symbols_table.pop_scope();
				}

				let statement = Statement::new_compound(statements);

				return StatementReturn
				{
					statement,
					remaining_tokens: tokens.collect()
				};
			},

			token::TokenType::Identifier =>
			{
				let t = t.as_token::<IdentifierToken>().unwrap().clone();

				if t.name() == "function"
				{
					if symbols_table.scope() != 1
					{
						panic!("Can only declare function in root scope");
					}

					let t_name = tokens.next().expect("Expected identifier token after `function`");
					let t_name = t_name
						.as_token::<IdentifierToken>()
						.expect("Expected identifier token after `function`")
						.name();

					let t_type_token = tokens.next().expect(format!("Expected type after function identifier `{}`", t_name).as_str());
					let vtype = t_type_token
						.as_token::<TypeToken>()
						.expect(format!("Expected type after function identifier `{}`", t_name).as_str())
						.vtype();

					let t_leftparen = tokens.next().expect("Expected `(` after type in function declaration");
					let t_leftparen = t_leftparen.as_token::<SymbolToken>().expect("Expected `(` after type in function declaration");
					if t_leftparen.sym() != Symbol::LeftParen
					{
						panic!("Expected `(` after function return type");
					}

					let mut sub_tokens = VecDeque::new();

					while let Some(next_token) = tokens.next()
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

					let mut iter_sub_token = sub_tokens.into_iter().peekable();

					let mut parameter_types = VecDeque::new();
					let mut parameters = VecDeque::new();

					symbols_table.push_scope();
					
					while let Some(sub_token) = iter_sub_token.next()
					{
						let param_id = sub_token.as_token::<IdentifierToken>()
							.expect("Expected identifier token")
							.name();

						let type_token = iter_sub_token.next().expect("Expected type after token");
						let param_vtype = type_token
							.as_token::<TypeToken>()
							.expect("Expected type token")
							.vtype();

						let comma_token = iter_sub_token.next();
						if comma_token.is_some()
						{
							let sym = comma_token.unwrap().as_token::<SymbolToken>().expect("Expected symbol token").sym();
							if sym != Symbol::Comma
							{
								panic!("Expected comma");
							}
						}
						
						symbols_table.define(&param_id, param_vtype.clone());

						let param = Parameter::new(symbols_table.get_id(&param_id).unwrap(), param_vtype.clone());
						parameters.push_back(param);
						parameter_types.push_back(param_vtype);
					}

					symbols_table.define_function(&t_name, vtype.clone(), parameter_types);

					let result = parse_statement(symbols_table, tokens.clone().collect(), false);

					let func_declare_statement = Statement::new_function_declare(
						t_name,
						parameters,
						vtype.clone(),
						result.statement
							.as_statement::<CompoundStatement>()
							.expect("Expected compoind statement")
							.clone()
						);
					
					symbols_table.pop_scope();

					return StatementReturn
					{
						statement: func_declare_statement,
						remaining_tokens: result.remaining_tokens.clone()
					};
				}
				else if t.name() == "return"
				{
					if symbols_table.scope() == 1
					{
						panic!("Cannot return in root scope");
					}

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
						statement: Statement::new_function_return(expr),
						remaining_tokens: tokens.collect()
					}
				}
				else if t.name() == "let"
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
					if symbols_table.scope() == 1
					{
						panic!("Cannot set in root scope");
					}

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
					if symbols_table.scope() == 1
					{
						panic!("Cannot print in root scope");
					}

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
		let result = parse_statement(&mut symbols_table, token_queue.clone(), true);

		root.add(result.statement);
		token_queue = result.remaining_tokens;
	}

	root
}
