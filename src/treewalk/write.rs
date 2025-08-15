use super::{BuiltInFnType, MaybeValue, Value};

pub const EXPORTED_BINDINGS: [(&str, BuiltInFnType); 2] =
    [("newline", builtin_newline), ("write", builtin_write)];

fn builtin_newline(values: Vec<Value>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Write takes no arguments".to_string());
    }
    println!();
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_write(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    print!("{}", values[0]);
    Ok(MaybeValue::Just(Value::Unspecified))
}
