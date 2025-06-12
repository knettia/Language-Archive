use dyn_clone::DynClone;
use std::any::Any;

use crate::data;
use crate::data::syms::Symbol;
use crate::data::vtype::VType;

#[derive(Clone)]
pub struct TokenInfo
{
	// Debug:
	pub line: usize,
	pub column_begin: usize,
	pub column_end: usize
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType
{
	Identifier,
	Type,
	Symbol,

	Arithmetic,
	Comparison,
	Boolean,

	BooleanLiteral,
	IntegerLiteral
}

pub trait TokenTrait: DynClone
{
	fn info(&self) -> TokenInfo;
	fn token_type(&self) -> TokenType;
	fn as_any(&self) -> &dyn Any; // for downcasting
}

dyn_clone::clone_trait_object!(TokenTrait);

#[derive(Clone)]
pub struct IdentifierToken
{
	info: TokenInfo,
	name: String 
}

impl TokenTrait for IdentifierToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Identifier
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl IdentifierToken
{
	pub fn name(&self) -> String
	{
		self.name.clone()
	}
}

#[derive(Clone)]
pub struct TypeToken
{
	info: TokenInfo,
	vtype: VType 
}

impl TokenTrait for TypeToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Type
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl TypeToken
{
	pub fn vtype(&self) -> VType
	{
		self.vtype.clone()
	}
}

#[derive(Clone)]
pub struct SymbolToken
{
	info: TokenInfo,
	sym: Symbol
}

impl TokenTrait for SymbolToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Symbol
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl SymbolToken
{
	pub fn sym(&self) -> Symbol
	{
		self.sym.clone()
	}
}

#[derive(Clone)]
pub struct ArithmeticToken
{
	info: TokenInfo,
	op: data::ops::ArithmeticOperation,
}

impl TokenTrait for ArithmeticToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Arithmetic
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl ArithmeticToken
{
	pub fn op(&self) -> data::ops::ArithmeticOperation
	{
		self.op.clone()
	}
}

#[derive(Clone)]
pub struct ComparisonToken
{
	info: TokenInfo,
	op: data::ops::ComparisonOperation
}

impl TokenTrait for ComparisonToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Comparison
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl ComparisonToken
{
	pub fn op(&self) -> data::ops::ComparisonOperation
	{
		self.op.clone()
	}
}

#[derive(Clone)]
pub struct BooleanToken
{
	info: TokenInfo,
	op: data::ops::BooleanOperation
}

impl TokenTrait for BooleanToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::Boolean
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl BooleanToken
{
	pub fn op(&self) -> data::ops::BooleanOperation
	{
		self.op.clone()
	}
}

#[derive(Clone)]
pub struct IntegerLiteralToken
{
	info: TokenInfo,
	value: i32 
}

impl TokenTrait for IntegerLiteralToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::IntegerLiteral
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl IntegerLiteralToken
{
	pub fn value(&self) -> i32
	{
		self.value
	}
}

#[derive(Clone)]
pub struct BooleanLiteralToken
{
	info: TokenInfo,
	value: bool 
}

impl TokenTrait for BooleanLiteralToken
{
	fn info(&self) -> TokenInfo
	{
		self.info.clone()
	}

	fn token_type(&self) -> TokenType
	{
		TokenType::BooleanLiteral
	}

	fn as_any(&self) -> &dyn Any
	{
		self
	}
}

impl BooleanLiteralToken
{
	pub fn value(&self) -> bool
	{
		self.value
	}
}

pub type TokenBox = Box<dyn TokenTrait>;

#[derive(Clone)]
pub struct Token
{
	token: TokenBox
}

impl Token
{
	// Token functions:
	pub fn info(&self) -> TokenInfo
	{
		self.token.info()
	}

	pub fn get_type(&self) -> TokenType
	{
		self.token.token_type()
	}

	// New functions:
	pub fn new(token: TokenBox) -> Token
	{
		Token { token }
	}

	pub fn new_identifier(info: TokenInfo, name: String) -> Token
	{
		Token::new(Box::new(IdentifierToken { info, name }))
	}

	pub fn new_type(info: TokenInfo, vtype: VType) -> Token
	{
		Token::new(Box::new(TypeToken { info, vtype }))
	}

	pub fn new_symbol(info: TokenInfo, sym: Symbol) -> Token
	{
		Token::new(Box::new(SymbolToken { info, sym }))
	}

	pub fn new_arithmetic(info: TokenInfo, op: data::ops::ArithmeticOperation) -> Token
	{
		Token::new(Box::new(ArithmeticToken { info, op }))
	}

	pub fn new_comparison(info: TokenInfo, op: data::ops::ComparisonOperation) -> Token
	{
		Token::new(Box::new(ComparisonToken { info, op }))
	}

	pub fn new_boolean(info: TokenInfo, op: data::ops::BooleanOperation) -> Token
	{
		Token::new(Box::new(BooleanToken { info, op }))
	}

	pub fn new_integer_literal(info: TokenInfo, value: i32) -> Token
	{
		Token::new(Box::new(IntegerLiteralToken { info, value }))
	}

	pub fn new_boolean_literal(info: TokenInfo, value: bool) -> Token
	{
		Token::new(Box::new(BooleanLiteralToken { info, value }))
	}

	// As function:
	pub fn as_token<T: 'static>(&self) -> Option<&T>
	{
		self.token.as_any().downcast_ref::<T>()
	}
}
