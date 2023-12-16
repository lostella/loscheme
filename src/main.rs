use loscheme::{tokenize, Expr};
use std::io::{stdin, stdout, Write};

fn main() {
    let mut s = String::new();

    loop {
        print!("loscheme> ");
        let _ = stdout().flush();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");

        let res = Expr::from_tokens(&mut tokenize(&s));

        match res {
            Ok(expr) => println!("{:?}", expr),
            Err(s) => println!("{}", s),
        }
    }
}
