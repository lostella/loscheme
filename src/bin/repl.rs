use loscheme::{parse_code, Environment, Expr};
use std::io::{self, BufRead, Write};

const INPUT_PROMPT: &str = "λscm>";

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::standard().child();

    loop {
        print!("{} ", INPUT_PROMPT);
        stdout.flush().unwrap();

        let mut input = String::new();
        let read_res = stdin.lock().read_line(&mut input);
        match read_res {
            Ok(0) => {
                continue;
            }
            Ok(_) => {
                input = input.trim().to_string();
                match parse_code(&input) {
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
