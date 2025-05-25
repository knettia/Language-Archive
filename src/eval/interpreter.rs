
use crate::ast::expression::*;
use crate::ast::literal::*;
use crate::ast::root::Root;

use crate::ast::statement::*;
use crate::data::ops::ArithmeticOperation;
use crate::data::ops::BooleanOperation;
use crate::data::ops::ComparisonOperation;
use crate::data::vtype::VType;

use super::value::*;

pub struct Interpreter
{
	root: Root
}

impl Interpreter
{
	fn interpret_expression(&self, expression: Expression) -> Value
	{
		match expression.expression_type()
		{
			ExpressionType::Literal =>
			{
				let literal_expression = expression.as_expression::<LiteralExpression>().unwrap();
				
				match expression.virtual_type()
				{
					VType::Integer => return Value::new_integer(literal_expression.literal.as_literal::<IntegerLiteral>().unwrap().value),
					VType::Boolean => return Value::new_boolean(literal_expression.literal.as_literal::<BooleanLiteral>().unwrap().value),
				}
			}
			
			ExpressionType::Arithmetic =>
			{
				let expression = expression.as_expression::<ArithmeticExpression>().unwrap();

				let arithmetic_type = expression.virtual_type();

				if arithmetic_type == VType::Boolean
				{
					panic!("Boolean extressions cannot be arithmetic");
				}

				let left_value = self.interpret_expression(expression.left());
				let right_value = self.interpret_expression(expression.right());

				if left_value.virtual_type() != arithmetic_type || right_value.virtual_type() != arithmetic_type
				{
					panic!("Mismatched types in arithmetic expression, left: {:?}, right: {:?}", left_value.virtual_type(), right_value.virtual_type());
				}

				match arithmetic_type
				{
					VType::Integer =>
					{
						let left = left_value.as_value::<IntegerValue>().unwrap();
						let right = right_value.as_value::<IntegerValue>().unwrap();

						match expression.op()
						{
							ArithmeticOperation::Multiply => return Value::new_integer(left.value * right.value), 
							ArithmeticOperation::Divide => return Value::new_integer(left.value / right.value), 
							ArithmeticOperation::Add => return Value::new_integer(left.value + right.value), 
							ArithmeticOperation::Subtract => return Value::new_integer(left.value - right.value), 
						}
					}

					_ => unreachable!()
				}
			},

			ExpressionType::Comparison =>
			{
				let expression = expression.as_expression::<ComparisonExpression>().unwrap();

				let left_value = self.interpret_expression(expression.left());
				let right_value = self.interpret_expression(expression.right());

				let comparison_type = left_value.virtual_type();

				if right_value.virtual_type() != comparison_type
				{
					panic!("Mismatched types cannot be compared in comparison expression, left: {:?}, right: {:?}", left_value.virtual_type(), right_value.virtual_type());
				}

				match comparison_type
				{
					VType::Integer =>
					{
						let left = left_value.as_value::<IntegerValue>().unwrap();
						let right = right_value.as_value::<IntegerValue>().unwrap();

						match expression.op()
						{
							ComparisonOperation::IsEqual => return Value::new_boolean(left.value == right.value),
							ComparisonOperation::IsNotEqual => return Value::new_boolean(left.value != right.value),
							ComparisonOperation::IsGreater => return Value::new_boolean(left.value > right.value),
							ComparisonOperation::IsGreaterOrEqual => return Value::new_boolean(left.value >= right.value),
							ComparisonOperation::IsLess => return Value::new_boolean(left.value < right.value),
							ComparisonOperation::IsLessOrEqual => return Value::new_boolean(left.value <= right.value),
						}
					}

					VType::Boolean =>
					{
						let left = left_value.as_value::<BooleanValue>().unwrap();
						let right = right_value.as_value::<BooleanValue>().unwrap();
		
						match expression.op()
						{
							ComparisonOperation::IsEqual => return Value::new_boolean(left.value == right.value),
							ComparisonOperation::IsNotEqual => return Value::new_boolean(left.value != right.value),

							ComparisonOperation::IsGreater | ComparisonOperation::IsGreaterOrEqual |
							ComparisonOperation::IsLess | ComparisonOperation::IsLessOrEqual => panic!("Boolean types cannot be compared with the {:?}", expression.op())
						}
					}
				}
			}

			ExpressionType::Boolean =>
			{
				let expression = expression.as_expression::<BooleanExpression>().unwrap();

				let left_value = self.interpret_expression(expression.left());
				let right_value = self.interpret_expression(expression.right());

				let left = left_value.as_value::<BooleanValue>().unwrap();
				let right = right_value.as_value::<BooleanValue>().unwrap();

				match expression.op()
				{
					BooleanOperation::And => return Value::new_boolean(left.value && right.value),
					BooleanOperation::Or => return Value::new_boolean(left.value || right.value)
				}
			}
		}
	}

	pub fn new(root: Root) -> Self
	{
		Self { root }
	}

	pub fn interpret(&self)
	{
		for statement in self.root.statements.clone()
		{
			match statement.statement_type()
			{
				StatementType::Print =>
				{
					let statement = statement.as_statement::<PrintStatement>().unwrap();
					let expression = statement.expression();
					let value = self.interpret_expression(expression);

					match value.virtual_type()
					{
						VType::Integer =>
						{
							let value = value.as_value::<IntegerValue>().unwrap();
							println!("{}", value.value);
						}

						VType::Boolean =>
						{
							let value = value.as_value::<BooleanValue>().unwrap();
							println!("{}", value.value);
						}
					}
				}

				_ =>
				{
					unimplemented!();
				}
			}
		}
	}
}
