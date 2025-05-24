use dyn_clone::DynClone;
use std::any::Any;

use crate::data::vtype::VType;

pub trait ValueTrait: DynClone
{
	fn virtual_type(&self) -> VType;
	fn as_any(&self) -> &dyn Any;
}

dyn_clone::clone_trait_object!(ValueTrait);

#[derive(Clone)]
pub struct IntegerValue
{
	pub value: i32
}

impl ValueTrait for IntegerValue
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

impl IntegerValue
{
	fn new(value: i32) -> Self
	{
		Self { value }
	}
}

#[derive(Clone)]
pub struct BooleanValue
{
	pub value: bool
}

impl ValueTrait for BooleanValue
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

impl BooleanValue
{
	fn new(value: bool) -> Self
	{
		Self { value }
	}
}

pub type ValueBox = Box<dyn ValueTrait>;

#[derive(Clone)]
pub struct Value
{
	value: ValueBox
}

impl Value
{
	// Token functions:
	pub fn virtual_type(&self) -> VType
	{
		self.value.virtual_type()
	}

	// New functions:
	pub fn new(value: ValueBox) -> Value
	{
		Value { value }
	}

	pub fn new_integer(value: i32) -> Value
	{
		Value::new(Box::new(IntegerValue::new(value)))
	}

	pub fn new_boolean(value: bool) -> Value
	{
		Value::new(Box::new(BooleanValue::new(value)))
	}

	// As function:
	pub fn as_value<T: 'static>(&self) -> Option<&T>
	{
		self.value.as_any().downcast_ref::<T>()
	}
}





