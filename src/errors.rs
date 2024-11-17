use std::fmt;

#[derive(Debug, PartialEq)]
pub enum MyError {
    IOError(String),
    ParseError(String),
    RuntimeError(String),
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MyError::IOError(s) => write!(f, "Error (I/O): {}", s),
            MyError::ParseError(s) => write!(f, "Error (parsing): {}", s),
            MyError::RuntimeError(s) => write!(f, "Error (runtime): {}", s),
        }
    }
}
