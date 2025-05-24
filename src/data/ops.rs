
#[derive(Clone, Debug)]
pub enum ArithmeticOperation
{
	Multiply,
	Divide,
	Add,
	Subtract
}

#[derive(Clone, Debug)]
pub enum ComparisonOperation
{
	IsEqual,
	IsNotEqual,
	IsGreater,
	IsGreaterOrEqual,
	IsLess,
	IsLessOrEqual
}

#[derive(Clone, Debug)]
pub enum BooleanOperation
{
	And,
	Or
}

// #[derive(Clone)]
// pub enum UnaBooleanOperation
// {
// 	And,
// 	Or
// }

