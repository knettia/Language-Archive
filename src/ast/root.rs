use std::collections::VecDeque;

use super::statement::Statement;

#[derive(Clone)]
pub struct Root
{
	pub statements: VecDeque<Statement>
}

impl Root
{
	pub fn new() -> Self
	{
		Self { statements: VecDeque::new() }
	}

	pub fn add(&mut self, statement: Statement)
	{
		self.statements.push_back(statement.clone());
	}
}
