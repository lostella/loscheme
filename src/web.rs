use crate::run::run;
use crate::treewalk::Environment;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_standard(code: &str) -> String {
    let mut env = Environment::standard().child();
    let res = run(code, &mut env);
    match res {
        Ok(v) => format!("{}", v),
        Err(err) => format!("{}", err.to_string()),
    }
}
