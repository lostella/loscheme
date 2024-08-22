use loscheme::{parse_code, Environment};
use std::io::{self, BufRead, Write};

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::standard().child();

    loop {
        print!("λ~> ");
        stdout.flush().unwrap();

        let mut input = String::new();

        match stdin.lock().read_line(&mut input) {
            Ok(0) => {
                continue;
            }
            Ok(_) => {
                input = input.trim().to_string();
                let expr = &parse_code(&input).unwrap()[0];
                let res = env.evaluate(expr);
                println!("=> {:?}", res);
            }
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }

    println!("Exiting...");
}
