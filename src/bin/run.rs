use loscheme::{parse_code, Environment};
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Error: need one argument, got {}", args.len() - 1);
    }

    let mut env = Environment::standard().child();

    let file = File::open(&args[1]).expect("Unable to open file");
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

    match parse_code(&code) {
        Ok(exprs) => {
            for expr in exprs {
                let res = env.evaluate(expr);
                if let Err(err) = res {
                    eprintln!("Error (eval): {}", err)
                }
            }
        }
        Err(err) => eprintln!("Error (parsing): {}", err),
    }
}
