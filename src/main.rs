mod data;

mod parser;
mod ast;

mod eval;

use std::env;
use std::fs;
use std::process;

fn main() -> ()
{
	let args: Vec<String> = env::args().collect();

	if args.len() < 2
	{
		eprintln!("Usage: {} <source_file>", args[0]);
		process::exit(1);
	}

	let file_path = &args[1];

	let contents = fs::read_to_string(file_path)
		.unwrap_or_else(|err|
		{
			eprintln!("Error reading file `{}`: {}", file_path, err);
			process::exit(1);
		});

	let root = parser::parse_root(contents);

	let interpreter = eval::interpreter::Interpreter::new(root);
	interpreter.interpret();
}
