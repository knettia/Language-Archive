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

#[derive(Parser)]
#[command(author, version, about = "The Faren compiler frontend.")]
struct Cli
{
	/// Input file (.faren or .o)
	#[arg(short = 's', long)]
	source: String,

	/// Output file (.o or executable)
	#[arg(short = 'o', long)]
	output: String,

	/// Dump LLVM IR to specified file
	#[arg(long)]
	dump_ir: Option<String>,

	/// Dump assembly to specified file
	#[arg(long)]
	dump_asm: Option<String>,
}

fn main() -> ()
{
	let cli = Cli::parse();

	let input_ext = Path::new(&cli.source)
		.extension()
		.and_then(|e| e.to_str())
		.unwrap_or("");

	let output_ext = Path::new(&cli.output)
		.extension()
		.and_then(|e| e.to_str())
		.unwrap_or("");

	match (input_ext, output_ext)
	{
		("faren", "o") | ("faren", "out") =>
		{
			compile_to_object(&cli);
		},

		("faren", _) =>
		{
			let object_name = Path::new(&cli.source)
				.file_stem()
				.unwrap()
				.to_str()
				.unwrap();

			let tmp_obj = env::temp_dir().join(format!("faren-unnamed-tmp-{}.o", object_name));

			let mut tmp_cli = Cli::parse();
			tmp_cli.output = tmp_obj.to_str().unwrap().to_string();
			compile_to_object(&tmp_cli);

			link_object(&tmp_cli.output, &cli.output);

			fs::remove_file(&tmp_obj).unwrap();
		},

		("o", _) | ("out", _) =>
		{
			link_object(&cli.source, &cli.output);
		},

		_ =>
		{
			if input_ext != "faren" && (input_ext != "o" && input_ext != "out")
			{
				exit_error(&format!("unknown source file type `.{}`", input_ext))
			}

			if (output_ext != "o" && output_ext != "out") && output_ext != ""
			{
				exit_error(&format!("unknown output file type `.{}`", output_ext))
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

fn compile_to_object(cli: &Cli)
{
	let object_name = Path::new(&cli.source)
		.file_stem()
		.unwrap()
		.to_str()
		.unwrap();

	let context = inkwell::context::Context::create();
	let contents = read_file_or_exit(&cli.source);
	let root = parser::parse_root(contents);

	let mut generator = Generator::new(&context, "main_module");
	generator.generate(&root);

	let ir_path = env::temp_dir().join(format!("faren-unnamed-tmp-{}.ll", object_name));
	let asm_path = env::temp_dir().join(format!("faren-unnamed-tmp-{}.s", object_name));

	generator.write_to_file(ir_path.as_path());

	if let Some(ir_out) = &cli.dump_ir
	{
		fs::copy(&ir_path, ir_out).unwrap();
	}

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

	if let Some(asm_out) = &cli.dump_asm
	{
		fs::copy(&asm_path, asm_out).unwrap();
	}

	let as_status = process::Command::new("as")
		.arg(asm_path.to_str().unwrap())
		.arg("-o")
		.arg(&cli.output)
		.status()
		.unwrap_or_else(|err|
		{
			exit_error(&format!("invoking `as` failed: {}", err));
		});

	if !as_status.success()
	{
		exit_error("`as` failed.");
	}

	if cli.dump_ir.is_some()
	{
		let path = cli.dump_ir.clone().unwrap();

		let ext = Path::new(&path)
			.extension()
			.and_then(|e| e.to_str())
			.unwrap_or("");

		if ext != "ll"
		{
			record_warning("IR artifact extension is not `.ll`");
		}

		let err = fs::copy(&ir_path, path.clone());

		if err.is_err()
		{
			record_error(&format!("emitting IR artifact failed: {}", err.unwrap_err()));
		}
	}
	
	if cli.dump_asm.is_some()
	{
		let path = cli.dump_asm.clone().unwrap();

		let ext = Path::new(&path)
			.extension()
			.and_then(|e| e.to_str())
			.unwrap_or("");

		if ext != "S"
		{
			record_warning("ASM artifact extension is not `.S`");
		}

		let err = fs::copy(&asm_path, path.clone());

		if err.is_err()
		{
			record_error(&format!("emitting ASM artifact failed: {}", err.unwrap_err()));
		}
	}

	fs::remove_file(&ir_path).unwrap();
	fs::remove_file(&asm_path).unwrap();
}

fn link_object(obj_path: &str, output_path: &str)
{
	let status = process::Command::new("clang")
		.arg(obj_path)
		.arg("-o")
		.arg(output_path)
		.status()
		.unwrap_or_else(|err|
		{
			exit_error(&format!("invoking `clang` failed: {}", err));
		});


	if !status.success()
	{
		exit_error("`clang` failed.");
	}
}
