mod data;
mod parser;
mod ast;

mod eval;
use eval::interpreter::*;

mod codegen;
use codegen::generator::*;

use std::env;
use std::fs;
use std::path::Path;
use std::process;

// Exits the process with a usage manual
fn exit_with_usage(proc: &str)
{
	eprintln!(" > Usage:");
	eprintln!(" | {} interpret <source-file>", proc);
	eprintln!(" | {} compile <source-file> -o <output.o>", proc);
	eprintln!(" | {} compile <object-file> -x <output.out>", proc);
	process::exit(1);
}

// Reads file path and returns its contents, exits on error
fn read_file_or_exit(path: &str) -> String
{
	fs::read_to_string(path)
		.unwrap_or_else(|err|
		{
			eprintln!(" > Error reading file `{}`: {}", path, err);
			process::exit(1);
		})
}


// TODO: Right now the CLI is hard-coded, in the future
//       we want an actual CLI handler, which for example
//       --exec and --object flags rely on a --compile flag
//       and can be ordered however:
//       {} compile -o output.out -s source.script
//       some as:
//       {} compile -s source.script -o output.out
fn main() -> ()
{
	let args: Vec<String> = env::args().collect();

	if args.len() == 1 
	{
		exit_with_usage(&args[0]);
	}

	match args[1].as_str()
	{
		"interpret" =>
		{
			let file_path = &args[2];
			let contents = read_file_or_exit(file_path);

			let root = parser::parse_root(contents);

			let interpreter = Interpreter::new();
			interpreter.interpret(&root);
		},

		"compile" =>
		{
			let file_path = &args[2];
			
			if args.len() >= 5 && args[3] == "-o"
			{
				let mut leave_artifacts = false;

				if args.len() == 6
				{
					if args[5] == "--artifacts"
					{
						leave_artifacts = true;
					}
					else
					{
						exit_with_usage(&args[0]);
					}
				}

				let object_path = &args[4];
				let object_name = Path::new(file_path).file_name().unwrap();
				let contents = read_file_or_exit(file_path);

				let root = parser::parse_root(contents);
				let context = inkwell::context::Context::create();

				let generator = Generator::new(&context, "main_module");

				let ir_path = env::temp_dir().join(format!("{}{}", object_name.to_str().unwrap(), "_temp.ir"));
				let asm_path = env::temp_dir().join(format!("{}{}", object_name.to_str().unwrap(), "_temp.s"));

				generator.generate(&root);
				generator.write_to_file(ir_path.as_path());

				// Compile IR to ASM
				let llc_status = process::Command::new("llc")
					.arg("-opaque-pointers")
					.arg(ir_path.to_str().unwrap())
					.arg("-o")
					.arg(asm_path.to_str().unwrap())
					.status()
					.unwrap_or_else(|err|
					{
						eprintln!(" > Error invoking `llc`: {}", err);
						process::exit(1);
					});
				
				if !llc_status.success()
				{
					eprintln!(" > `llc` failed to compile IR file `{}` to assembly file `{}`", ir_path.to_str().unwrap(), asm_path.to_str().unwrap());
					process::exit(1);
				}

				// Assemble ASM into Object
				let as_status = process::Command::new("as")
					.arg(asm_path.to_str().unwrap())
					.arg("-o")
					.arg(object_path.clone())
					.status()
					.unwrap_or_else(|err|
					{
						eprintln!(" > Error invoking `as`: {}", err);
						process::exit(1);
					});

				if !as_status.success()
				{
					eprintln!(" > `as` failed to assemble assembly file `{}` to object file `{}`", asm_path.to_str().unwrap(), file_path.clone());
					process::exit(1);
				}

				// Clean-up
				if leave_artifacts
				{
					fs::copy(&ir_path, format!("{}.ir", object_name.to_str().unwrap())).unwrap();
					fs::copy(&asm_path, format!("{}.s", object_name.to_str().unwrap())).unwrap();
				}

				fs::remove_file(&ir_path).unwrap();
				fs::remove_file(&asm_path).unwrap();
			}
			else if args.len() == 5 && args[3] == "-x"
			{
				let binary_path = &args[4];

				let status = process::Command::new("clang")
					.arg(file_path)
					.arg("-o")
					.arg(binary_path)
					.status()
					.unwrap_or_else(|err|
					{
						eprintln!(" > Error invoking clang: {}", err);
						process::exit(1);
					});

				if !status.success()
				{
					eprintln!(" > Clang failed to compile to binary {}", binary_path);
					process::exit(1);
				}
			}
			else
			{
				exit_with_usage(&args[0]);
			}
		},

		_ =>
		{
			eprintln!(" > Unknown command: {}", args[1]);
			exit_with_usage(&args[0]);
		}
	}
}
