use dyn_clone::DynClone;
use std::any::Any;

use crate::data::vtype::VType;

pub trait LiteralTrait: DynClone
{
	fn virtual_type(&self) -> VType;
	fn as_any(&self) -> &dyn Any;
}

dyn_clone::clone_trait_object!(LiteralTrait);

#[derive(Clone)]
pub struct IntegerLiteral
{
	pub value: i32
}

impl LiteralTrait for IntegerLiteral
{
	fn virtual_type(&self) -> VType
	{
		VType::Integer
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl IntegerLiteral
{
	fn new(value: i32) -> Self
	{
		Self { value }
	}
}

#[derive(Clone)]
pub struct BooleanLiteral
{
	pub value: bool
}

impl LiteralTrait for BooleanLiteral
{
	fn virtual_type(&self) -> VType
	{
		VType::Boolean
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl BooleanLiteral
{
	fn new(value: bool) -> Self
	{
		Self { value }
	}
}

pub type LiteralBox = Box<dyn LiteralTrait>;

#[derive(Clone)]
pub struct Literal
{
	literal: LiteralBox
}

impl Literal
{
	// Token functions:
	pub fn virtual_type(&self) -> VType
	{
		self.literal.virtual_type()
	}

	// New functions:
	pub fn new(literal: LiteralBox) -> Literal
	{
		Literal { literal }
	}

	pub fn new_integer(value: i32) -> Literal
	{
		Literal::new(Box::new(IntegerLiteral::new(value)))
	}

	pub fn new_boolean(value: bool) -> Literal
	{
		Literal::new(Box::new(BooleanLiteral::new(value)))
	}

	// As function:
	pub fn as_literal<T: 'static>(&self) -> Option<&T>
	{
		self.literal.as_any().downcast_ref::<T>()
	}
}





