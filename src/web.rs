use crate::treewalk::{parse, Environment};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct WebRepl {
    env: Environment,
}

#[wasm_bindgen]
impl WebRepl {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            env: Environment::standard().child(),
        }
    }

    #[wasm_bindgen]
    pub fn evaluate(&mut self, input: String) -> String {
        let mut output = "".to_string();
        match parse(&input.trim().to_string()) {
            Ok(exprs) => {
                for expr in exprs {
                    let eval_res = self.env.evaluate(&expr);
                    match eval_res {
                        Ok(v) => output = format!("{}", v),
                        Err(err) => output = format!("{}", err.to_string()),
                    };
                }
            }
            Err(err) => output = format!("{}", err.to_string()),
        }
        output
    }
}
