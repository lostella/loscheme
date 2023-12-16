use loscheme::{parse_tokens, tokenize};
use std::io::{stdin, stdout, Write};

fn main() {
    let mut s = String::new();

    loop {
        print!("loscheme> ");
        let _ = stdout().flush();

        s.clear();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");

        let mut tokens = tokenize(&s);
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
