use std::collections::VecDeque;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VType
{
	Void,
	Integer,
	Boolean
}

#[derive(Debug, Clone)]
pub struct Parameter
{
	id: u16,
	vtype: VType
}

impl Parameter
{
	pub fn new(id: u16, vtype: VType) -> Self
	{
		Self { id, vtype }
	}

	pub fn id(&self) -> u16
	{
		self.id
	}

	pub fn vtype(&self) -> VType
	{
		self.vtype.clone()
	}
}

#[derive(Debug, Clone)]
pub struct FunctionSignature
{
	name: String,
	return_type: VType,
	parameters: VecDeque<Parameter>
}

impl FunctionSignature
{
	pub fn new(name: String, return_type: VType, parameters: VecDeque<Parameter>) -> Self
	{
		Self { name, return_type, parameters }
	}

	pub fn name(&self) -> String
	{
		self.name.clone()
	}

	pub fn return_type(&self) -> VType
	{
		self.return_type.clone()
	}

	pub fn parameters(&self) -> VecDeque<Parameter>
	{
		self.parameters.clone()
	}
}
