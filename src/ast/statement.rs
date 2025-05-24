use dyn_clone::DynClone;
use std::any::Any;

use super::expression::Expression;

#[derive(Clone)]
pub enum StatementType
{
	Declare,
	Assign,
	Compound,
	Print
}

pub trait StatementTrait: DynClone
{
	fn statement_type(&self) -> StatementType;
	fn as_any(&self) -> &dyn Any; // for downcasting
}

dyn_clone::clone_trait_object!(StatementTrait);

#[derive(Clone)]
pub struct PrintStatement
{
	expression: Expression
}

impl StatementTrait for PrintStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::Print
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl PrintStatement
{
	pub fn new(expression: Expression) -> Self
	{
		Self { expression }
	}

	pub fn expression(&self) -> Expression
	{
		self.expression.clone()
	}
}

pub type StatementBox = Box<dyn StatementTrait>;

#[derive(Clone)]
pub struct Statement
{
	statement: StatementBox
}

impl Statement
{
	// Token functions:
	pub fn statement_type(&self) -> StatementType
	{
		self.statement.statement_type()
	}

	// New functions:
	pub fn new(statement: StatementBox) -> Self
	{
		Self { statement }
	}

	pub fn new_print(expression: Expression) -> Self
	{
		Self::new(Box::new(PrintStatement::new(expression)))
	}

	// As function:
	pub fn as_statement<T: 'static>(&self) -> Option<&T>
	{
		self.statement.as_any().downcast_ref::<T>()
	}
}
