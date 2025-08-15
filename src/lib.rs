pub mod char;
pub mod errors;
pub mod parser;
pub mod rationals;
pub mod run;
pub mod rv32;
pub mod treewalk;
pub mod utils;

#[cfg(all(target_arch = "wasm32", feature = "web"))]
pub mod web;
