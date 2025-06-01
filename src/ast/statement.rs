use dyn_clone::DynClone;
use std::{any::Any, collections::VecDeque};

use crate::data::vtype::*;

use super::expression::Expression;

#[derive(Clone)]
pub enum StatementType
{
	FunctionDefine,
	FunctionDeclare,
	FunctionReturn,

	Expression,

	Compound,
	Declare,
	Assign,
	Print
}

pub trait StatementTrait: DynClone
{
	fn statement_type(&self) -> StatementType;
	fn as_any(&self) -> &dyn Any; // for downcasting
}

dyn_clone::clone_trait_object!(StatementTrait);

#[derive(Clone)]
pub struct FunctionDefineStatement
{
	signature: FunctionSignature,
	body: CompoundStatement
}

impl StatementTrait for FunctionDefineStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::FunctionDefine
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl FunctionDefineStatement
{
	pub fn new(signature: FunctionSignature, body: CompoundStatement) -> Self
	{
		Self { signature, body }
	}

	pub fn signature(&self) -> FunctionSignature
	{
		self.signature.clone()
	}

	pub fn body(&self) -> CompoundStatement
	{
		self.body.clone()
	}
}

#[derive(Clone)]
pub struct FunctionDeclareStatement
{
	signature: FunctionSignature
}

impl StatementTrait for FunctionDeclareStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::FunctionDeclare
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl FunctionDeclareStatement
{
	pub fn new(signature: FunctionSignature) -> Self
	{
		Self { signature }
	}

	pub fn signature(&self) -> FunctionSignature
	{
		self.signature.clone()
	}
}

#[derive(Clone)]
pub struct FunctionReturnStatement
{
	expression: Option<Expression>
}

impl StatementTrait for FunctionReturnStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::FunctionReturn
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl FunctionReturnStatement
{
	pub fn new(expression: Option<Expression>) -> Self
	{
		Self { expression }
	}

	pub fn expression(&self) -> Option<Expression>
	{
		self.expression.clone()
	}
}

#[derive(Clone)]
pub struct ExpressionStatement
{
	expression: Expression
}

impl StatementTrait for ExpressionStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::Expression
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl ExpressionStatement
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

#[derive(Clone)]
pub struct CompoundStatement
{
	statements: VecDeque<Statement>
}

impl StatementTrait for CompoundStatement
{
	fn statement_type(&self) -> StatementType
	{
		StatementType::Compound
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl CompoundStatement
{
	pub fn new(statements: VecDeque<Statement>) -> Self
	{
		Self { statements }
	}

	pub fn statements(&self) -> VecDeque<Statement>
	{
		self.statements.clone()
	}
}

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

	pub fn new_function_define(signature: FunctionSignature, body: CompoundStatement) -> Self
	{
		Self::new(Box::new(FunctionDefineStatement::new(signature, body)))
	}

	pub fn new_function_declare(signature: FunctionSignature) -> Self
	{
		Self::new(Box::new(FunctionDeclareStatement::new(signature)))
	}

	pub fn new_function_return(expression: Option<Expression>) -> Self
	{
		Self::new(Box::new(FunctionReturnStatement::new(expression)))
	}

	pub fn new_expression(expression: Expression) -> Self
	{
		Self::new(Box::new(ExpressionStatement::new(expression)))
	}

	pub fn new_compound(statements: VecDeque<Statement>) -> Self
	{
		Self::new(Box::new(CompoundStatement::new(statements)))
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
