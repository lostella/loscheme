use crate::errors::MyError;
use crate::parser::parse;
use crate::treewalk::{Environment, Value};

pub fn run(code: &str, env: &mut Environment) -> Result<Value, MyError> {
    let mut val: Value = Value::Unspecified;

    match parse(code) {
        Ok(exprs) => {
            for expr in exprs {
                match env.evaluate(&expr.into()) {
                    Err(err) => return Err(MyError::RuntimeError(err.to_string())),
                    Ok(res) => val = res,
                }
            }
            Ok(val)
        }
        Err(err) => Err(MyError::ParseError(err.to_string())),
    }
}

pub fn run_standard(code: &str) -> Result<Value, MyError> {
    let mut env = Environment::standard().child();
    run(code, &mut env)
}
