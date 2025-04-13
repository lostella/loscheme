use crate::errors::MyError;
use crate::parser::{parse, Expr};
use crate::treewalk::Environment;

pub fn run(code: &str, env: &mut Environment) -> Result<Expr, MyError> {
    let mut val: Expr = Expr::Unspecified;

    match parse(code) {
        Ok(exprs) => {
            for expr in exprs {
                match env.evaluate(&expr) {
                    Err(err) => return Err(MyError::RuntimeError(err.to_string())),
                    Ok(expr) => val = expr,
                }
            }
            Ok(val)
        }
        Err(err) => Err(MyError::ParseError(err.to_string())),
    }
}

pub fn run_standard(code: &str) -> Result<Expr, MyError> {
    let mut env = Environment::standard().child();
    run(code, &mut env)
}
