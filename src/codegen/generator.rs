use std::path::Path;
use std::str::FromStr;
use std::collections::HashMap;

use inkwell::
{
	AddressSpace,
	context::Context,
	builder::Builder,
	module::Module,

	values::
	{
		FunctionValue,
		PointerValue
	},

	types::
	{
		BasicType,
		FunctionType
	}
};

use faren_canon::ast::
{
	root::*,
	statement::*,
	expression::*,
	literal::*,
};

use faren_canon::data::
{
	vtype::*,
	ops::*
};

use super::meta_value::MetaValue;

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
			ExpressionType::FunctionCall =>
			{
				let function_call_expression = expression.as_expression::<FunctionCallExpression>().unwrap();

				let function_name = function_call_expression.name();

				let llvm_function = self.module.get_function(&function_name)
					.unwrap_or_else(|| panic!("Function `{}` not found in module", function_name));

				let passed_arguments = function_call_expression.passed_arguments();

				let mut llvm_args = Vec::new();

				for arg_expr in passed_arguments
				{
					let meta_arg = self.generate_expression(arg_expr);

					let llvm_value = match meta_arg.vtype
					{
						VType::Void => panic!(),

						VType::Integer | VType::Boolean =>
						{
							meta_arg.llvm.unwrap().into_int_value().into()
						}
					};

					llvm_args.push(llvm_value);
				}

				let call_site = self.builder.build_call(
					llvm_function,
					&llvm_args,
					"calltmp"
				).unwrap();

				let return_type = function_call_expression.vtype();

				let return_value = match return_type
				{
					VType::Void =>
					{
						MetaValue
						{
							vtype: VType::Void,
							llvm: None
						}
					},

					VType::Integer =>
					{
						let value = call_site.try_as_basic_value().left()
							.expect("Expected integer return from function");
						MetaValue
						{
							vtype: VType::Integer,
							llvm: Some(value)
						}
					}

					VType::Boolean =>
					{
						let value = call_site.try_as_basic_value().left()
							.expect("Expected boolean return from function");
						MetaValue
						{
							vtype: VType::Boolean,
							llvm: Some(value)
						}
					}
				};

				return return_value;
			}

			ExpressionType::Literal =>
			{
				let literal_expression = expression.as_expression::<LiteralExpression>().unwrap();

				match expression.virtual_type()
				{
					VType::Void => panic!(),

					VType::Integer =>
					{
						let int_val = literal_expression.literal().as_literal::<IntegerLiteral>().unwrap().value;
						let llvm_int = self.context.i32_type().const_int(int_val as u64, true);
						return MetaValue { vtype: VType::Integer, llvm: Some(llvm_int.into()) };
					},

					VType::Boolean =>
					{
						let bool_val = literal_expression.literal().as_literal::<BooleanLiteral>().unwrap().value;
						let llvm_bool = self.context.bool_type().const_int(if bool_val { 1 } else { 0 }, false);
						return MetaValue { vtype: VType::Boolean, llvm: Some(llvm_bool.into()) };
					}
				}
			},

			ExpressionType::Variable =>
			{
				let variable_expr = expression.as_expression::<VariableExpression>().unwrap();
				let id = variable_expr.identifier();

				let vtype = variable_expr.vtype();

				let llvm_type = match vtype.clone()
				{
					VType::Void => panic!(),
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
					llvm: Some(loaded_value)
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
							llvm: Some(self.builder.build_int_add(
								left.llvm.unwrap().into_int_value(),
								right.llvm.unwrap().into_int_value(),
								"addtmp"
							).unwrap().into())
						}
					}

					ArithmeticOperation::Subtract =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: Some(self.builder.build_int_sub(
								left.llvm.unwrap().into_int_value(),
								right.llvm.unwrap().into_int_value(),
								"subtmp"
							).unwrap().into())
						}
					}

					ArithmeticOperation::Multiply =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: Some(self.builder.build_int_mul(
								left.llvm.unwrap().into_int_value(),
								right.llvm.unwrap().into_int_value(),
								"multmp"
							).unwrap().into())
						}
					}

					ArithmeticOperation::Divide =>
					{
						return MetaValue
						{
							vtype: arithmetic_type,
							llvm: Some(self.builder.build_int_signed_div(
								left.llvm.unwrap().into_int_value(),
								right.llvm.unwrap().into_int_value(),
								"sigdivtmp"
							).unwrap().into())
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
					VType::Void => panic!(),

					VType::Integer =>
					{
						let left = left_value.llvm.unwrap().into_int_value();
						let right = right_value.llvm.unwrap().into_int_value();

						return MetaValue
						{
							vtype: expression.vtype(),
							llvm: match expression.op()
							{
								ComparisonOperation::IsEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::EQ,
										left,
										right,
										"eqcmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsNotEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::NE,
										left,
										right,
										"necmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsGreater =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::SGT,
										left,
										right,
										"sgtcmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsGreaterOrEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::SGE,
										left,
										right,
										"sgecmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsLess =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::SLT,
										left,
										right,
										"sltcmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsLessOrEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::SLE,
										left,
										right,
										"slecmptmp"
									).unwrap().into())
								}
							    
							}
						};
					}

					VType::Boolean =>
					{
						let left = left_value.llvm.unwrap().into_int_value();
						let right = right_value.llvm.unwrap().into_int_value();

						match expression.op()
						{
							ComparisonOperation::IsGreater | ComparisonOperation::IsGreaterOrEqual |
							ComparisonOperation::IsLess | ComparisonOperation::IsLessOrEqual => panic!("Boolean types cannot be compared with the {:?}", expression.op()),

							_ => { }
						}

						return MetaValue
						{
							vtype: expression.vtype(),
							llvm: match expression.op()
							{
								ComparisonOperation::IsEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::EQ,
										left,
										right,
										"eqcmptmp"
									).unwrap().into())
								}

								ComparisonOperation::IsNotEqual =>
								{
									Some(self.builder.build_int_compare(
										inkwell::IntPredicate::NE,
										left,
										right,
										"necmptmp"
									).unwrap().into())
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
						BooleanOperation::And => Some(self.builder.build_and(
							left.llvm.unwrap().into_int_value(),
							right.llvm.unwrap().into_int_value(),
							"boolandtmp"
						).unwrap().into()),

						BooleanOperation::Or => Some(self.builder.build_or(
							left.llvm.unwrap().into_int_value(),
							right.llvm.unwrap().into_int_value(),
							"boolortmp"
						).unwrap().into()),
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
			VType::Void => panic!(),

			VType::Integer =>
			{
				let format_str = self.create_global_format_str("fmt_int", "%d\n");

				let mut args = vec![format_str.into()];
				args.push(value.llvm.unwrap().into_int_value().into());

				self.builder.build_call(printf_fn, &args, "printf_call").unwrap();
			},

			VType::Boolean =>
			{
				let format_str = self.create_global_format_str("fmt_bool", "%s\n");

				let true_str = self.create_global_format_str("str_true", "true");
				let false_str = self.create_global_format_str("str_false", "false");

				let bool_val = value.llvm.unwrap().into_int_value();

				let selected_str = self.builder.build_select(
					bool_val,
					true_str,
					false_str,
					"bool_str"
				);

				let args = vec![format_str.into(), selected_str.unwrap().into()];
				self.builder.build_call(printf_fn, &args, "printf_call").unwrap();
			}
		}
	}

	pub fn get_function_type(&self, signature: &FunctionSignature) -> FunctionType<'ctx>
	{
		let vtype = signature.return_type();
		let parameters = signature.parameters();

		let llvm_params;

		if parameters.len() > 0
		{
			llvm_params = parameters
				.iter()
				.map(|vt|
				{
					match vt.vtype()
					{
						VType::Void => panic!(),
						VType::Integer => self.context.i32_type().as_basic_type_enum().into(),
						VType::Boolean => self.context.bool_type().as_basic_type_enum().into()
					}
				})
				.collect();
		}
		else
		{
			llvm_params = vec![];
		}

		match vtype.clone()
		{
			VType::Void =>
			{
				self.context.void_type().fn_type(&llvm_params[..], false)
			},

			VType::Integer =>
			{
				self.context.i32_type().fn_type(&llvm_params[..], false)
			},

			VType::Boolean =>
			{
				self.context.bool_type().fn_type(&llvm_params[..], false)
			}
		}
	}

	pub fn generate_statement(&mut self, statement: Statement)
	{
		match statement.statement_type()
		{
			StatementType::FunctionDefine =>
			{
				let func_stmt = statement.as_statement::<FunctionDefineStatement>().unwrap();
				let func_sign = func_stmt.signature();

				let func_type = self.get_function_type(&func_sign);
				let func_val = self.module.add_function(&func_sign.name(), func_type, None);

				
				let entry_block = self.context.append_basic_block(func_val, "entry");
				self.builder.position_at_end(entry_block);
				
				let old_variables = std::mem::take(&mut self.variables);
				
				let func_params = func_sign.parameters();

				if func_params.len() > 0
				{
					for (i, param) in func_params.iter().enumerate()
					{
						let param_value = func_val.get_nth_param(i as u32).unwrap();

						let ptr = match param.vtype()
						{
							VType::Void => panic!(),
							VType::Integer => self.builder.build_alloca(self.context.i32_type(), "arg_int").unwrap(),
							VType::Boolean => self.builder.build_alloca(self.context.bool_type(), "arg_bool").unwrap()
						};

						self.builder.build_store(ptr, param_value).unwrap();
						self.variables.insert(param.id(), ptr);
					}
				}

				self.generate_statement(Statement::new(Box::new(func_stmt.body())));

				self.variables = old_variables;
			}

			StatementType::FunctionDeclare =>
			{
				let func_stmt = statement.as_statement::<FunctionDeclareStatement>().unwrap();
				let func_sign = func_stmt.signature();

				let func_type = self.get_function_type(&func_sign);

				self.module.add_function(&func_sign.name(), func_type, None);
			}

			StatementType::FunctionReturn =>
			{
				let return_stmt = statement.as_statement::<FunctionReturnStatement>().unwrap();

				let return_expr = return_stmt.expression();

				if return_expr.is_none() // return void
				{
					self.builder.build_return(None).unwrap();
				}
				else
				{
					let value = self.generate_expression(return_expr.unwrap());
				    
					let llvm_val = value.llvm.unwrap();
					self.builder.build_return(Some(&llvm_val)).unwrap();
				}
			}

			StatementType::Expression =>
			{
				let expr_stmt = statement.as_statement::<ExpressionStatement>().unwrap();

				let expr = expr_stmt.expression();

				let value = self.generate_expression(expr);
				let _ = value; // discard value
			}

			StatementType::Compound =>
			{
				let compound_stmt = statement.as_statement::<CompoundStatement>().unwrap();

				for stmt in compound_stmt.statements()
				{
					self.generate_statement(stmt);
				}
			},

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
					VType::Void => panic!(),
					VType::Integer => self.builder.build_alloca(self.context.i32_type(), "intvar").unwrap(),
					VType::Boolean => self.builder.build_alloca(self.context.bool_type(), "boolvar").unwrap()
				};

				self.builder.build_store(ptr, value.llvm.unwrap()).unwrap();
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

				self.builder.build_store(*ptr, value.llvm.unwrap()).unwrap();
			}

			// _ => unimplemented!("Codegen for this statement type is not implemented."),
		}
	}

	pub fn write_to_file(&self, path: &Path)
	{
		self.module.print_to_file(path).expect(format!("Failed to write IR {}", String::from_str(path.to_str().unwrap()).unwrap()).as_str());
	}

	pub fn generate(&mut self, root: &Root)
	{
		// let i32_type = self.context.i32_type();
		// let fn_type = i32_type.fn_type(&[], false);
		// let function = self.module.add_function("main", fn_type, None);
		// let basic_block = self.context.append_basic_block(function, "entry");

		// self.builder.position_at_end(basic_block);

		for stmt in root.statements.clone()
		{
			self.generate_statement(stmt);
		}

		// self.builder.build_return(Some(&self.context.i32_type().const_int(0, true))).unwrap();
	}
}
