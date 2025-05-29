use dyn_clone::DynClone;
use std::any::Any;
use std::collections::VecDeque;

use super::literal::Literal;

use crate::data::ops::ComparisonOperation;
use crate::data::vtype::VType;

use crate::data::ops::ArithmeticOperation;
use crate::data::ops::BooleanOperation;

pub enum ExpressionType
{
	Literal,
	FunctionCall,
	Variable,
	Arithmetic,
	Comparison,
	Boolean
} 

pub trait ExpressionTrait: DynClone
{
	fn virtual_type(&self) -> VType;
	fn expression_type(&self) -> ExpressionType;

	fn as_any(&self) -> &dyn Any; // for downcasting
}

dyn_clone::clone_trait_object!(ExpressionTrait);

#[derive(Clone)]
pub struct LiteralExpression
{
	pub literal: Literal
}

impl ExpressionTrait for LiteralExpression
{
	fn virtual_type(&self) -> VType
	{
		self.literal.virtual_type().clone()
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::Literal
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl LiteralExpression
{
	fn new(literal: Literal) -> Self
	{
		Self { literal }
	}
}

#[derive(Clone)]
pub struct FunctionCallExpression
{
	pub vtype: VType,
	pub name: String,
	pub passed_arguments: VecDeque<Expression>
}

impl ExpressionTrait for FunctionCallExpression
{
	fn virtual_type(&self) -> VType
	{
		self.vtype.clone()
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::FunctionCall
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl FunctionCallExpression
{
	fn new(vtype: VType, name: String, passed_arguments: VecDeque<Expression>) -> Self
	{
		Self { vtype, name, passed_arguments }
	}

	pub fn name(&self) -> String
	{
		self.name.clone()
	}

	pub fn passed_arguments(&self) -> VecDeque<Expression>
	{
		self.passed_arguments.clone()
	}
}

#[derive(Clone)]
pub struct VariableExpression
{
	pub vtype: VType,
	pub identifier: u16
}

impl ExpressionTrait for VariableExpression
{
	fn virtual_type(&self) -> VType
	{
		self.vtype.clone()
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::Variable
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl VariableExpression
{
	fn new(vtype: VType, identifier: u16) -> Self
	{
		Self { vtype, identifier }
	}

	pub fn vtype(&self) -> VType
	{
		self.vtype.clone()
	}

	pub fn identifier(&self) -> u16
	{
		self.identifier
	}
}

#[derive(Clone)]
pub struct ArithmeticExpression
{
	vtype: VType,
	op: ArithmeticOperation,

	left: Expression,
	right: Expression
}

impl ExpressionTrait for ArithmeticExpression
{
	fn virtual_type(&self) -> VType
	{
		self.vtype.clone()
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::Arithmetic
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl ArithmeticExpression
{
	pub fn new(vtype: VType, op: ArithmeticOperation, left: Expression, right: Expression) -> Self
	{
		Self { vtype, op, left, right }
	}

	pub fn op(&self) -> ArithmeticOperation
	{
		self.op.clone()
	}

	pub fn left(&self) -> Expression
	{
		self.left.clone()
	}

	pub fn right(&self) -> Expression
	{
		self.right.clone()
	}
}

#[derive(Clone)]
pub struct ComparisonExpression
{
	op: ComparisonOperation,

	left: Expression,
	right: Expression
}

impl ExpressionTrait for ComparisonExpression
{
	fn virtual_type(&self) -> VType
	{
		VType::Boolean
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::Comparison
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl ComparisonExpression
{
	pub fn new(op: ComparisonOperation, left: Expression, right: Expression) -> Self
	{
		Self { op, left, right }
	}

	pub fn op(&self) -> ComparisonOperation
	{
		self.op.clone()
	}

	pub fn left(&self) -> Expression
	{
		self.left.clone()
	}

	pub fn right(&self) -> Expression
	{
		self.right.clone()
	}
}

#[derive(Clone)]
pub struct BooleanExpression
{
	op: BooleanOperation,

	left: Expression,
	right: Expression
}

impl ExpressionTrait for BooleanExpression
{
	fn virtual_type(&self) -> VType
	{
		VType::Boolean
	}

	fn expression_type(&self) -> ExpressionType
	{
		ExpressionType::Boolean
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl BooleanExpression
{
	pub fn new(op: BooleanOperation, left: Expression, right: Expression) -> Self
	{
		Self { op, left, right }
	}

	pub fn op(&self) -> BooleanOperation
	{
		self.op.clone()
	}

	pub fn left(&self) -> Expression
	{
		self.left.clone()
	}

	pub fn right(&self) -> Expression
	{
		self.right.clone()
	}
}

pub type ExpressionBox = Box<dyn ExpressionTrait>;

#[derive(Clone)]
pub struct Expression
{
	expression: ExpressionBox
}

impl Expression
{
	// Token functions:
	pub fn virtual_type(&self) -> VType
	{
		self.expression.virtual_type()
	}

	pub fn expression_type(&self) -> ExpressionType
	{
		self.expression.expression_type()
	}

	// New functions:
	pub fn new(expression: ExpressionBox) -> Self
	{
		Self { expression }
	}

	pub fn new_function_call(vtype: VType, name: String, passed_arguments: VecDeque<Expression>) -> Self
	{
		Self::new(Box::new(FunctionCallExpression::new(vtype, name, passed_arguments)))
	}

	pub fn new_literal(literal: Literal) -> Self
	{
		Self::new(Box::new(LiteralExpression::new(literal)))
	}
	
	pub fn new_variable(vtype: VType, identifier: u16) -> Self
	{
		Self::new(Box::new(VariableExpression::new(vtype, identifier)))
	}

	pub fn new_arithmetic(vtype: VType, op: ArithmeticOperation, left: Expression, right: Expression) -> Self
	{
		Self::new(Box::new(ArithmeticExpression::new(vtype, op, left, right)))
	}

	pub fn new_comparison(op: ComparisonOperation, left: Expression, right: Expression) -> Self
	{
		Self::new(Box::new(ComparisonExpression::new(op, left, right)))
	}

	pub fn new_boolean(op: BooleanOperation, left: Expression, right: Expression) -> Self
	{
		Self::new(Box::new(BooleanExpression::new(op, left, right)))
	}

	// As function:
	pub fn as_expression<T: 'static>(&self) -> Option<&T>
	{
		self.expression.as_any().downcast_ref::<T>()
	}
}
