
use crate::data::vtype::VType;
use inkwell::values::BasicValueEnum;

pub struct MetaValue<'ctx>
{
	pub vtype: VType,
	pub llvm: Option<BasicValueEnum<'ctx>>
}
