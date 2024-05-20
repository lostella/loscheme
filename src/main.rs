use std::io::{self, Write};
use loscheme::{standard_env, parse, eval};


fn main() {
    let mut env = standard_env();
    
    loop {
        print!("loscheme> ");
        io::stdout().flush().unwrap();
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read line");
        
        let expr = parse(&input);
        let result = eval(expr, &mut env);
        
        println!("Result: {:?}", result);
    }
}
