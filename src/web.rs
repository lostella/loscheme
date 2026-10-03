use crate::run::run;
use crate::treewalk::{Environment, Value};
use crate::utils::take_output;
use wasm_bindgen::prelude::*;

/// Result of running a program: what it printed, and either its final value or an error.
#[wasm_bindgen]
pub struct RunResult {
    output: String,
    value: Option<String>,
    error: Option<String>,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }

    /// Printable final value, absent if it is unspecified or the run failed.
    #[wasm_bindgen(getter)]
    pub fn value(&self) -> Option<String> {
        self.value.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn error(&self) -> Option<String> {
        self.error.clone()
    }
}

#[wasm_bindgen]
pub fn run_standard(code: &str) -> RunResult {
    take_output();
    let mut env = Environment::standard().child();
    let res = run(code, &mut env);
    let output = take_output();
    match res {
        Ok(Value::Unspecified) => RunResult {
            output,
            value: None,
            error: None,
        },
        Ok(v) => RunResult {
            output,
            value: Some(v.to_string()),
            error: None,
        },
        Err(err) => RunResult {
            output,
            value: None,
            error: Some(err.to_string()),
        },
    }
}
