use crate::errors::MyError;
use crate::parser::parse;
use crate::treewalk::{Environment, Value, ValueRef};

pub fn run(code: &str, env: &mut Environment) -> Result<ValueRef, MyError> {
    let mut val: ValueRef = Value::Unspecified.into();

    match parse(code) {
        Ok(exprs) => {
            for expr in exprs {
                match env.evaluate(ValueRef::from(Value::from(expr))) {
                    Err(err) => return Err(MyError::RuntimeError(err.to_string())),
                    Ok(res) => val = res,
                }
            }
            Ok(val)
        }
        Err(err) => Err(MyError::ParseError(err.to_string())),
    }
}

pub fn run_standard(code: &str) -> Result<ValueRef, MyError> {
    let mut env = Environment::standard().child();
    run(code, &mut env)
}
