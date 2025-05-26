use std::path::Path;
use std::str::FromStr;
use std::collections::HashMap;

use inkwell::values::{FunctionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::{
	context::Context,
	builder::Builder,
	module::Module,
	values::BasicValueEnum,
	types::BasicType
};

use crate::ast::root::*;

use crate::ast::statement::*;
use crate::ast::expression::*;
use crate::ast::literal::*;

use crate::data::vtype::VType;
use crate::data::ops::*;

pub struct MetaValue<'ctx>
{
	pub vtype: VType,
	pub llvm: BasicValueEnum<'ctx>
}

pub struct Generator<'ctx>
{
	context: &'ctx Context,
	builder: Builder<'ctx>,
	module: Module<'ctx>,
	variables: HashMap<u16, PointerValue<'ctx>>
}

impl<'ctx> Generator<'ctx>
{
	pub fn new(context: &'ctx Context, module_name: &str) -> Self
	{
		let module = context.create_module(module_name);
		let builder = context.create_builder();

		Self { context, builder, module, variables: HashMap::new() }
	}

	pub fn generate_expression(&self, expression: Expression) -> MetaValue
	{
		match expression.expression_type()
		{
			ExpressionType::Literal =>
			{
				let literal_expression = expression.as_expression::<LiteralExpression>().unwrap();

				match expression.virtual_type()
				{
					VType::Integer =>
					{
						let int_val = literal_expression.literal.as_literal::<IntegerLiteral>().unwrap().value;
						let llvm_int = self.context.i32_type().const_int(int_val as u64, true);
						return MetaValue { vtype: VType::Integer, llvm: llvm_int.into()};
					},

					VType::Boolean =>
					{
						let bool_val = literal_expression.literal.as_literal::<BooleanLiteral>().unwrap().value;
						let llvm_bool = self.context.bool_type().const_int(if bool_val { 1 } else { 0 }, false);
						return MetaValue { vtype: VType::Boolean, llvm: llvm_bool.into()};
					},
				}
			},

			ExpressionType::Variable =>
			{
				let variable_expr = expression.as_expression::<VariableExpression>().unwrap();
				let id = variable_expr.identifier();

				let vtype = variable_expr.vtype();

				let llvm_type = match vtype.clone()
				{
					VType::Integer => self.context.i32_type().as_basic_type_enum(),
					VType::Boolean => self.context.bool_type().as_basic_type_enum()
				};


				let Some(ptr) = self.variables.get(&id) else
				{
					panic!("Use of undeclared variable ID {}", id);
				};

				let loaded_value = self.builder.build_load(llvm_type, *ptr, "loadvar").unwrap();

				return MetaValue
				{
					vtype,
					llvm: loaded_value
				};
			}

			ExpressionType::Arithmetic =>
			{
				let arithmetic_expression = expression.as_expression::<ArithmeticExpression>().unwrap();

				let left = self.generate_expression(arithmetic_expression.left());
				let right = self.generate_expression(arithmetic_expression.right());

				let arithmetic_type = expression.virtual_type();

				if arithmetic_type == VType::Boolean
				{
					panic!("Boolean extressions cannot be arithmetic");
				}

				if left.vtype != arithmetic_type || right.vtype != arithmetic_type
				{
					panic!("Mismatched types in arithmetic expression, left: {:?}, right: {:?}", left.vtype, right.vtype);
				}

				match arithmetic_expression.op()
				{
					ArithmeticOperation::Add =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: self.builder.build_int_add(
								left.llvm.into_int_value(),
								right.llvm.into_int_value(),
								"addtmp"
							).unwrap().into()
						}
					}

					ArithmeticOperation::Subtract =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: self.builder.build_int_sub(
								left.llvm.into_int_value(),
								right.llvm.into_int_value(),
								"subtmp"
							).unwrap().into()
						}
					}

					ArithmeticOperation::Multiply =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: self.builder.build_int_mul(
								left.llvm.into_int_value(),
								right.llvm.into_int_value(),
								"multmp"
							).unwrap().into()
						}
					}

					ArithmeticOperation::Divide =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: self.builder.build_int_signed_div(
								left.llvm.into_int_value(),
								right.llvm.into_int_value(),
								"sigdivtmp"
							).unwrap().into()
						}
					}
				}
			},

			ExpressionType::Comparison =>
			{
				let expression = expression.as_expression::<ComparisonExpression>().unwrap();

				let left_value = self.generate_expression(expression.left());
				let right_value = self.generate_expression(expression.right());

				let comparison_type = left_value.vtype.clone();

				if right_value.vtype != comparison_type
				{
					panic!("Mismatched types cannot be compared in comparison expression, left: {:?}, right: {:?}", left_value.vtype, right_value.vtype);
				}

				match comparison_type
				{
					VType::Integer =>
					{
						let left = left_value.llvm.into_int_value();
						let right = right_value.llvm.into_int_value();

						return MetaValue
						{
							vtype: expression.virtual_type(),
							llvm: match expression.op()
							{
								ComparisonOperation::IsEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::EQ,
										left,
										right,
										"eqcmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsNotEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::NE,
										left,
										right,
										"necmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsGreater =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::SGT,
										left,
										right,
										"sgtcmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsGreaterOrEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::SGE,
										left,
										right,
										"sgecmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsLess =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::SLT,
										left,
										right,
										"sltcmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsLessOrEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::SLE,
										left,
										right,
										"slecmptmp"
									).unwrap().into()
								}
							    
							}
						};
					}

					// TODO: in the future, include metadata so we know which types are "boolean", even if under the hood they are still integers
					VType::Boolean =>
					{
						let left = left_value.llvm.into_int_value();
						let right = right_value.llvm.into_int_value();

						match expression.op()
						{
							ComparisonOperation::IsGreater | ComparisonOperation::IsGreaterOrEqual |
							ComparisonOperation::IsLess | ComparisonOperation::IsLessOrEqual => panic!("Boolean types cannot be compared with the {:?}", expression.op()),

							_ => { }
						}

						return MetaValue
						{
							vtype: expression.virtual_type(),
							llvm: match expression.op()
							{
								ComparisonOperation::IsEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::EQ,
										left,
										right,
										"eqcmptmp"
									).unwrap().into()
								}

								ComparisonOperation::IsNotEqual =>
								{
									self.builder.build_int_compare(
										inkwell::IntPredicate::NE,
										left,
										right,
										"necmptmp"
									).unwrap().into()
								}

								_ => panic!()
							}
						}
					}
				}
			}

			ExpressionType::Boolean =>
			{
				let expression = expression.as_expression::<BooleanExpression>().unwrap();

				let boolean_type = VType::Boolean;

				let left = self.generate_expression(expression.left());
				let right = self.generate_expression(expression.right());

				if left.vtype != boolean_type || right.vtype != boolean_type
				{
					panic!("Mismatched types in boolean expression, left: {:?}, right: {:?}", left.vtype, right.vtype);
				}

				return MetaValue
				{
					vtype: boolean_type,

					llvm: match expression.op()
					{
						BooleanOperation::And => self.builder.build_and(
							left.llvm.into_int_value(),
							right.llvm.into_int_value(),
							"boolandtmp"
						).unwrap().into(),

						BooleanOperation::Or => self.builder.build_or(
							left.llvm.into_int_value(),
							right.llvm.into_int_value(),
							"boolortmp"
						).unwrap().into(),
					}
				}
			}
		}
	}

	fn get_printf_function(&self) -> FunctionValue<'ctx>
	{
		let ptr_type = self.context.ptr_type(AddressSpace::from(0));

		let printf_type = self.context.i32_type().fn_type(&[ptr_type.into()], true);

		match self.module.get_function("printf")
		{
			Some(func) => func,
			None => self.module.add_function("printf", printf_type, None),
		}
	}

	fn create_global_format_str(&self, name: &str, format: &str) -> PointerValue<'ctx>
	{
		let c_string = std::ffi::CString::new(format).unwrap();
		let str_constant = self.context.const_string(c_string.as_bytes_with_nul(), false);

		let global = self.module.add_global(str_constant.get_type(), None, name);
		global.set_initializer(&str_constant);
		global.set_constant(true);

		global.as_pointer_value()
	}
	
	pub fn generate_printf(&self, value: MetaValue)
	{
		let printf_fn = self.get_printf_function();

		match value.vtype
		{
			VType::Integer =>
			{
				let format_str = self.create_global_format_str("fmt_int", "%d\n");

				let mut args = vec![format_str.into()];
				args.push(value.llvm.into_int_value().into());

				self.builder.build_call(printf_fn, &args, "printf_call").unwrap();
			},

			VType::Boolean =>
			{
				let format_str = self.create_global_format_str("fmt_bool", "%s\n");

				let true_str = self.create_global_format_str("str_true", "true");
				let false_str = self.create_global_format_str("str_false", "false");

				let bool_val = value.llvm.into_int_value();

				let selected_str = self.builder.build_select(
					bool_val,
					true_str,
					false_str,
					"bool_str"
				);

				let args = vec![format_str.into(), selected_str.unwrap().into()];
				self.builder.build_call(printf_fn, &args, "printf_call").unwrap();
			},

		}
	}

	pub fn generate_statement(&mut self, statement: Statement)
	{
		match statement.statement_type()
		{
			StatementType::Print =>
			{
				let print_stmt: &PrintStatement = statement.as_statement::<PrintStatement>().unwrap();
				let value = self.generate_expression(print_stmt.expression());

				self.generate_printf(value);
			},

			StatementType::Declare =>
			{
				let declare_stmt = statement.as_statement::<DeclareStatement>().unwrap();

				let id = declare_stmt.identifier();
				let vtype = declare_stmt.vtype();
				let expr = declare_stmt.expression();

				let value = self.generate_expression(expr);

				if value.vtype != vtype
				{
					panic!("Type mismatch in declaration: declared {:?}, got {:?}", vtype, value.vtype);
				}

				let ptr = match vtype
				{
					VType::Integer => self.builder.build_alloca(self.context.i32_type(), "intvar").unwrap(),
					VType::Boolean => self.builder.build_alloca(self.context.bool_type(), "boolvar").unwrap(),
				};

				self.builder.build_store(ptr, value.llvm).unwrap();
				self.variables.insert(id, ptr);
			}


			StatementType::Assign =>
			{
				let assign_stmt = statement.as_statement::<AssignStatement>().unwrap();

				let id = assign_stmt.identifier();
				let expr = assign_stmt.expression();

				let value = self.generate_expression(expr);

				let Some(ptr) = self.variables.get(&id) else
				{
					panic!("Assignment to undeclared variable ID {}", id);
				};

				self.builder.build_store(*ptr, value.llvm).unwrap();
			}


			_ => unimplemented!("Codegen for this statement type is not implemented."),
		}
	}

	pub fn write_to_file(&self, path: &Path)
	{
		self.module.print_to_file(path).expect(format!("Failed to write IR {}", String::from_str(path.to_str().unwrap()).unwrap()).as_str());
	}

	pub fn generate(&mut self, root: &Root)
	{
		let i32_type = self.context.i32_type();
		let fn_type = i32_type.fn_type(&[], false);
		let function = self.module.add_function("main", fn_type, None);
		let basic_block = self.context.append_basic_block(function, "entry");

		self.builder.position_at_end(basic_block);

		for stmt in root.statements.clone()
		{
			self.generate_statement(stmt);
		}

		self.builder.build_return(Some(&self.context.i32_type().const_int(0, true))).unwrap();
	}
}
