use loscheme::{parse_code, Environment};
use std::io::{self, BufRead, Write};

const INPUT_PROMPT: &str = "λ~>";

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::standard().child();

    loop {
        print!("{} ", INPUT_PROMPT);
        stdout.flush().unwrap();

        let mut input = String::new();

        match stdin.lock().read_line(&mut input) {
            Ok(0) => {
                continue;
            }
            Ok(_) => {
                input = input.trim().to_string();
                match parse_code(&input) {
                    Ok(exprs) => {
                        for expr in exprs {
                            let res = env.evaluate(&expr);
                            match res {
                                Ok(Some(v)) => println!("{}", v),
                                Ok(None) => (),
                                Err(err) => println!("Error (eval): {}", err),
                            };
                        }
                    }
                    Err(err) => println!("Error (parsing): {}", err),
                }
            }
            Err(err) => {
                println!("Error (I/O): {}", err);
            }
        }
    }
}
