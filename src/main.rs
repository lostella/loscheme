use loscheme::{eval, standard_env};
use std::io::{self, Write};

fn main() {
    let mut env = standard_env();

    loop {
        print!("loscheme> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let result = eval(&input, &mut env);

        println!("Result: {:?}", result);
    }
}
