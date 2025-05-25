use dyn_clone::DynClone;
use std::any::Any;

use crate::data::vtype::VType;

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
pub struct DeclareStatement
{
	vtype: VType,
	identifier: u16,
	expression: Expression
}

impl StatementTrait for DeclareStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::Declare
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl DeclareStatement
{
	pub fn new(vtype: VType, identifier: u16, expression: Expression) -> Self
	{
		Self { vtype, identifier, expression }
	}

	pub fn vtype(&self) -> VType
	{
		self.vtype.clone()
	}

	pub fn identifier(&self) -> u16
	{
		self.identifier
	}

	pub fn expression(&self) -> Expression
	{
		self.expression.clone()
	}
}

#[derive(Clone)]
pub struct AssignStatement
{
	identifier: u16,
	expression: Expression
}

impl StatementTrait for AssignStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::Assign
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl AssignStatement
{
	pub fn new(identifier: u16, expression: Expression) -> Self
	{
		Self { identifier, expression }
	}

	pub fn identifier(&self) -> u16
	{
		self.identifier
	}

	pub fn expression(&self) -> Expression
	{
		self.expression.clone()
	}
}

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

	pub fn new_declare(vtype: VType, identifier: u16, expression: Expression) -> Self
	{
		Self::new(Box::new(DeclareStatement::new(vtype, identifier, expression)))
	}

	pub fn new_assign(identifier: u16, expression: Expression) -> Self
	{
		Self::new(Box::new(AssignStatement::new(identifier, expression)))
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
