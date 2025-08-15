use super::{BuiltInFnType, MaybeValue, Value};

pub const EXPORTED_BINDINGS: [(&str, BuiltInFnType); 2] =
    [("display", builtin_write), ("write", builtin_write)];

fn builtin_write(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    print!("{}", values[0]);
    Ok(MaybeValue::Just(Value::Unspecified))
}
