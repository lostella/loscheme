use loscheme::{parse_tokens, tokenize, Environment};
use std::io::{stdin, stdout, Write};

fn main() {
    let mut environment = Environment::new();
    let mut user_input = String::new();

    loop {
        print!("loscheme> ");
        let _ = stdout().flush();

        user_input.clear();
        stdin()
            .read_line(&mut user_input)
            .expect("Did not enter a correct string");

        let mut tokens = tokenize(&user_input);
        let res = parse_tokens(&mut tokens);

        match res {
            Ok(exprs) => {
                // TODO evaluate all expressions instead
                println!("{:?}", exprs);
            },
            Err(s) => println!("{}", s),
        }
    }
}
