use loscheme::treewalk::{parse, Environment, Expr};
use std::env;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

const REPL_PROMPT: &str = "λscm>";

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::standard().child();

    loop {
        print!("{} ", REPL_PROMPT);
        stdout.flush().unwrap();

        let mut input = String::new();
        let read_res = stdin.lock().read_line(&mut input);
        match read_res {
            Ok(0) => {
                println!();
                continue;
            }
            Ok(_) => {
                input = input.trim().to_string();
                match parse(&input) {
                    Ok(exprs) => {
                        for expr in exprs {
                            let eval_res = env.evaluate(&expr);
                            match eval_res {
                                Ok(Expr::Unspecified) => (),
                                Ok(v) => println!("{}", v),
                                Err(err) => eprintln!("Error (eval): {}", err),
                            };
                        }
                    }
                    Err(err) => eprintln!("Error (parsing): {}", err),
                }
            }
            Err(err) => {
                eprintln!("Error (I/O): {}", err);
            }
        }
    }
}

fn run(filename: &str) {
    let mut env = Environment::standard().child();

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

    match parse(&code) {
        Ok(exprs) => {
            for expr in exprs {
                let res = env.evaluate(&expr);
                if let Err(err) = res {
                    eprintln!("Error (eval): {}", err)
                }
            }
        }
        Err(err) => eprintln!("Error (parsing): {}", err),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() - 1 {
        0 => repl(),
        1 => run(&args[1]),
        x => eprintln!("Error: need one argument at most, got {}", x),
    }
}
