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
                continue;
            }
            Ok(_) => {
                if input.split_once(":quit").is_some() {
                    break;
                }
                match run(&input, &mut env) {
                    Ok(Value::Unspecified) => (),
                    Ok(v) => println!("{v}"),
                    Err(err) => eprintln!("{err}"),
                }
            }
            Err(err) => {
                eprintln!("{err}");
            }
        }
    }
    ExitCode::SUCCESS
}

fn script_loop(filename: &str) -> ExitCode {
    let maybe_code = read_code(filename);
    let output = match maybe_code {
        Ok(code) => run_standard(&code),
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::from(1);
        }
    };
    if let Err(err) = output {
        eprintln!("{err}");
        return ExitCode::from(1);
    }
    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    match args.len() - 1 {
        0 => repl_loop(),
        1 => script_loop(&args[1]),
        x => {
            eprintln!("Error: need one argument at most, got {x}");
            ExitCode::from(1)
        }
    }
}
