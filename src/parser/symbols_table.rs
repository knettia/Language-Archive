use std::collections::{HashMap, VecDeque};

use crate::data::vtype::*;

#[derive(Debug, Clone)]
pub struct Scope
{
	name_to_id: HashMap<String, u16>,
	id_to_type: HashMap<u16, VType>
}

impl Scope
{
	pub fn new(parent: Option<&Scope>) -> Self
	{
		let name_to_id = if let Some(p) = parent
		{
			p.name_to_id.clone()
		}
		else
		{
			HashMap::new()
		};

		let id_to_type = if let Some(p) = parent
		{
			p.id_to_type.clone()
		}
		else
		{
			HashMap::new()
		};

		Self
		{
			name_to_id,
			id_to_type
		}
	}

	pub fn define(&mut self, name: &str, id: u16, vtype: VType)
	{
		self.name_to_id.insert(name.to_string(), id);
		self.id_to_type.insert(id, vtype);
	}

	pub fn lookup_id(&self, name: &str) -> Option<u16>
	{
		self.name_to_id.get(name).copied()
	}

	pub fn lookup_type(&self, id: u16) -> Option<&VType>
	{
		self.id_to_type.get(&id)
	}
}

#[derive(Debug, Clone)]
pub struct SymbolsTable
{
	functions: HashMap<String, FunctionSignature>,

	scopes: VecDeque<Scope>,
	next_id: u16
}

impl SymbolsTable
{
	pub fn new() -> Self
	{
		Self
		{
			functions: HashMap::new(),
			scopes: VecDeque::new(),
			next_id: 0
		}
	}

	pub fn define_function(&mut self, name: &str, return_type: VType, parameters: VecDeque<Parameter>)
	{
		let info = FunctionSignature::new(name.to_string(), return_type, parameters);

		if self.functions.contains_key(name)
		{
			panic!("Function {} already defined", name);
		}

		self.functions.insert(name.to_string(), info);
	}

	pub fn get_function(&self, name: &str) -> Option<&FunctionSignature>
	{
		self.functions.get(name)
	}

	pub fn push_scope(&mut self)
	{
		let parent = self.scopes.front();
		let new_scope = Scope::new(parent);
		self.scopes.push_front(new_scope);
	}

	pub fn pop_scope(&mut self)
	{
		self.scopes.pop_front();
	}

	pub fn define(&mut self, name: &str, vtype: VType)
	{
		let id: u16 = self.next_id;
		self.next_id += 1;

		if let Some(scope) = self.scopes.front_mut()
		{
			scope.define(name, id, vtype);
		}
	}

	pub fn scope(&self) -> usize
	{
		self.scopes.len()
	}

	pub fn lookup(&self, name: &str) -> Option<&VType>
	{
		let scope = self.scopes.front()?;

		let id = scope.lookup_id(name)?;
		scope.lookup_type(id)
	}

	pub fn get_id(&self, name: &str) -> Option<u16>
	{
		self.scopes.front()?.lookup_id(name)
	}
}
