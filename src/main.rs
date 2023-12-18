use loscheme::{parse_str, tokenize, Environment};
use std::io::{stdin, stdout, Write};

fn main() {
    let _environment = Environment::new();
    let mut user_input = String::new();

    loop {
        print!("loscheme> ");
        let _ = stdout().flush();

        user_input.clear();
        stdin()
            .read_line(&mut user_input)
            .expect("Did not enter a correct string");

        let res = parse_str(&user_input);

        match res {
            Ok(exprs) => {
                // TODO evaluate all expressions instead
                println!("{:?}", exprs);
            }
            Err(s) => println!("{}", s),
        }
    }
}
