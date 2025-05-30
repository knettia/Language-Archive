use clap::Parser;
use std::path::Path;
use std::{env, fs, process};

mod ast;
mod codegen;
mod data;
mod parser;

use codegen::generator::*;

use ansi_term::Colour;

fn record_warning(msg: &str)
{
	let label = Colour::Yellow.bold().paint("warning:");
	eprintln!("farenc: {} {}", label, msg);
}

fn record_error(msg: &str)
{
	let label = Colour::Red.bold().paint("error:");
	eprintln!("farenc: {} {}", label, msg);
}

fn exit_error(msg: &str) -> !
{
	record_error(msg);
	process::exit(1);
}

#[derive(Parser, Clone)]
#[command(author, version, about = "The Faren compiler frontend.")]
struct Cli
{
	/// Input file (.faren or .o)
	#[arg(short = 's', long, required = true)]
	sources: Vec<String>,

	/// Output file (.o or executable)
	#[arg(short = 'o', long)]
	output: String,

	/// Link Archive Static library to an output file
	#[arg(long)]
	link_archive: Option<Vec<String>>,

	/// Dump LLVM IR to specified file
	#[arg(long)]
	dump_ir: Option<String>,

	/// Dump assembly to specified file
	#[arg(long)]
	dump_asm: Option<String>,
}

enum OutputType
{
	Relocatable,
	Executable,
	Archive
}

fn main() -> ()
{
	let cli = Cli::parse();

	let mut object_files = Vec::new();

	let output_ext = Path::new(&cli.output)
		.extension()
		.and_then(|e| e.to_str())
		.unwrap_or("");

	let output_type = match output_ext
	{
		"o" => OutputType::Relocatable,
		"" | "out" => OutputType::Executable,
		"a" => OutputType::Archive,
		_ => exit_error(&format!("unknown output extension: `.{}`", output_ext))
	};

	for source in &cli.sources
	{
		let input_ext = Path::new(&source)
			.extension()
			.and_then(|e| e.to_str())
			.unwrap_or("");
		
		match input_ext
		{
			"faren" | "frn" | "fn" =>
			{
				let object_name = Path::new(source)
					.file_stem()
					.unwrap()
					.to_str()
					.unwrap();

				let object_path = env::temp_dir().join(format!("faren-unnamed-{}.o", object_name));
				let mut tmp_cli = cli.clone();

				tmp_cli.sources = vec![source.clone()];
				tmp_cli.output = object_path.to_str().unwrap().to_string();

				compile_to_object(source.clone(), object_path.to_str().unwrap().to_string(), cli.dump_ir.clone(), cli.dump_asm.clone());
				object_files.push(tmp_cli.output.clone());
			},

			"o" =>
			{
				object_files.push(source.clone());
			},

			ext =>
			{
				exit_error(&format!("unknown input extension: `.{}`", ext));
			}
		}
	}

	link_objects(object_files, cli.output, output_type, cli.link_archive);
}

fn link_objects(object_paths: Vec<String>, output_path: String, output_type: OutputType, linked_archives: Option<Vec<String>>)
{
	match output_type
	{
		OutputType::Relocatable =>
		{
			let mut command = process::Command::new("ld");
			command.arg("-r");

			for obj in &object_paths
			{
				command.arg(obj);
			}

			command.arg("-o").arg(&output_path);

			let status = command.status().unwrap_or_else(|err|
			{
				exit_error(&format!("invoking `ld -r` failed: {}", err));
			});

			if !status.success()
			{
				exit_error("`ld -r` failed.");
			}
		},

		OutputType::Executable =>
		{
			let mut command = process::Command::new("clang");

			for obj in &object_paths
			{
				command.arg(obj);
			}

			if let Some(archives) = &linked_archives
			{
				command.arg("-Wl,--start-group");

				for archive in archives
				{
					command.arg(archive);
				}

				command.arg("-Wl,--end-group");
			}

			command.arg("-o").arg(&output_path);

			let status = command.status().unwrap_or_else(|err|
			{
				exit_error(&format!("invoking `clang` failed: {}", err));
			});

			if !status.success()
			{
				exit_error("`clang` failed.");
			}
		}


		OutputType::Archive =>
		{
			let mut command = process::Command::new("ar");

			let mut args = vec!["rcs".to_string(), output_path.clone()];
			args.extend(object_paths.clone());

			command.args(args);

			let status = command.status().unwrap_or_else(|err|
			{
				exit_error(&format!("invoking `ar` failed: {}", err));
			});

			if !status.success()
			{
				exit_error("`ar` failed.");
			}
		}
	}
}

fn read_file_or_exit(path: &str) -> String
{
	fs::read_to_string(path)
		.unwrap_or_else(|err|
		{
			exit_error(&format!("failed to read file `{}`: {}", path, err));
		})
}

fn compile_to_object(source: String, output: String, dump_ir: Option<String>, dump_asm: Option<String>)
{
	let object_name = Path::new(&source)
		.file_stem()
		.unwrap()
		.to_str()
		.unwrap();

	let context = inkwell::context::Context::create();
	let contents = read_file_or_exit(&source);
	let root = parser::parse_root(contents);

	let mut generator = Generator::new(&context, "main_module");
	generator.generate(&root);

	let ir_path = env::temp_dir().join(format!("faren-unnamed-{}.ll", object_name));
	let asm_path = env::temp_dir().join(format!("faren-unnamed-{}.S", object_name));

	generator.write_to_file(ir_path.as_path());

	let llc_status = process::Command::new("llc")
		.arg("-opaque-pointers")
		.arg(ir_path.to_str().unwrap())
		.arg("-o")
		.arg(asm_path.to_str().unwrap())
		.status()
		.unwrap_or_else(|err|
		{
			exit_error(&format!("invoking `llc` failed: {}", err));
		});

	if !llc_status.success()
	{
		exit_error("`llc` failed.");
	}

	let as_status = process::Command::new("as")
		.arg(asm_path.to_str().unwrap())
		.arg("-o")
		.arg(&output)
		.status()
		.unwrap_or_else(|err|
		{
			exit_error(&format!("invoking `as` failed: {}", err));
		});

	if !as_status.success()
	{
		exit_error("`as` failed.");
	}

	if let Some(ir_out) = &dump_ir
	{
		let ext = Path::new(&ir_out)
			.extension()
			.and_then(|e| e.to_str())
			.unwrap_or("");

		if ext != "ll"
		{
			record_warning("IR artifact extension is not `.ll`");
		}

		let err = fs::copy(&ir_path, ir_out.clone());

		if err.is_err()
		{
			record_error(&format!("emitting IR artifact failed: {}", err.unwrap_err()));
		}
	}
	
	if let Some(asm_out) = &dump_asm
	{
		let ext = Path::new(&asm_out)
			.extension()
			.and_then(|e| e.to_str())
			.unwrap_or("");

		if ext != "S"
		{
			record_warning("ASM artifact extension is not `.S`");
		}

		let err = fs::copy(&asm_path, asm_out.clone());

		if err.is_err()
		{
			record_error(&format!("emitting ASM artifact failed: {}", err.unwrap_err()));
		}
	}

	fs::remove_file(&ir_path).unwrap();
	fs::remove_file(&asm_path).unwrap();
}
