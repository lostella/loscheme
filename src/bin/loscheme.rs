use loscheme::run::{run, run_standard};
use loscheme::treewalk::{Environment, Value};
use loscheme::utils::read_code;
use std::env;
use std::io::{self, prelude::*};
use std::process::ExitCode;

const REPL_PROMPT: &str = "λscm>";

fn repl_loop() -> ExitCode {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::replenv().child();

    loop {
        print!("{REPL_PROMPT} ");
        stdout.flush().unwrap();

        let mut input = String::new();
        let read_res = stdin.lock().read_line(&mut input);
        match read_res {
            Ok(0) => {
                // EOF (Ctrl-D)
                println!(); // Print a newline for clean exit
                break;
            }
            Ok(_) => {
                let input = input.trim();
                if input == ":quit" {
                    break;
                }
                if input.is_empty() {
                    continue;
                }
                match run(input, &mut env) {
                    Ok(Value::Unspecified) => (),
                    Ok(v) => println!("{v}"),
                    Err(err) => eprintln!("{err}"),
                }
            }
            Err(err) => {
                eprintln!("Error reading from stdin: {err}");
                return ExitCode::from(1);
            }
        }
    }
    ExitCode::SUCCESS
}

fn script_loop(filename: &str) -> ExitCode {
    let code = match read_code(filename) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            return ExitCode::from(1);
        }
    };

    if let Err(err) = run_standard(&code) {
        eprintln!("Error executing script '{}': {}", filename, err);
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let program_name = args.get(0).map_or("loscheme", |s| s.as_str());

    if args.len() > 2 {
        eprintln!("Error: expected one file argument at most, got {}", args.len() - 1);
        eprintln!("Usage: {} [script_file]", program_name);
        return ExitCode::from(1);
    }

    if let Some(arg) = args.get(1) {
        match arg.as_str() {
            "-h" | "--help" => {
                println!("A Scheme interpreter written in Rust.");
                println!();
                println!("Usage: {} [script_file]", program_name);
                println!();
                println!("If no script file is provided, it starts a REPL.");
                println!();
                println!("Options:");
                println!("  -h, --help     Print this help message and exit");
                println!("  -v, --version  Print version information and exit");
                ExitCode::SUCCESS
            }
            "-v" | "--version" => {
                println!("loscheme {}", env!("CARGO_PKG_VERSION"));
                ExitCode::SUCCESS
            }
            filename => {
                if filename.starts_with('-') {
                    eprintln!("Error: unknown option '{}'", filename);
                    eprintln!("Usage: {} [script_file]", program_name);
                    return ExitCode::from(1);
                }
                script_loop(filename)
            }
        }
    } else {
        repl_loop()
    }
}
