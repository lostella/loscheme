use crate::treewalk::{parse, Environment};

pub fn run(code: &str) {
    let mut env = Environment::standard().child();

    match parse(code) {
        Ok(exprs) => {
            for expr in exprs {
                let res = env.evaluate(&expr);
                if let Err(err) = res {
                    eprintln!("Error (eval): {}", err)
                }
            }
        }
        Err(err) => eprintln!("Error (parsing): {}", err),
    }
}
