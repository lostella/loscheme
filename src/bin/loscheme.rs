use loscheme::run::{run, run_standard};
use loscheme::treewalk::Environment;
use std::env;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

const REPL_PROMPT: &str = "λscm>";

fn read_code_file(filename: &str) -> String {
    let file = File::open(filename).expect("Unable to open file");
    let reader = BufReader::new(file);
    let mut code = String::new();

    for res in reader.lines() {
        let line = res.expect("Unable to read line");
        match line.find(';') {
            Some(idx) => code.push_str(&line[..idx]),
            None => code.push_str(&line),
        }
        code.push('\n');
    }

    code
}

fn repl_loop() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::standard().child();

    loop {
        print!("{REPL_PROMPT} ");
        stdout.flush().unwrap();

        let mut input = String::new();
        let read_res = stdin.lock().read_line(&mut input);
        match read_res {
            Ok(0) => {
                println!();
                continue;
            }
            Ok(_) => {
                if input.split_once(":quit").is_some() {
                    break;
                }
                let to_be_run = if let Some((_, filename)) = input.trim().split_once(":load ") {
                    read_code_file(filename)
                } else {
                    input
                };
                match run(&to_be_run, &mut env) {
                    Ok(v) => println!("{v}"),
                    Err(err) => eprintln!("{err}"),
                }
            }
            Err(err) => {
                eprintln!("{err}");
            }
        }
    }
}

fn script_loop(filename: &str) {
    let code = read_code_file(filename);
    let output = run_standard(&code);
    if let Err(err) = output {
        eprintln!("{err}")
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() - 1 {
        0 => repl_loop(),
        1 => script_loop(&args[1]),
        x => eprintln!("Error: need one argument at most, got {x}"),
    }
}
