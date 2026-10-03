use super::{BuiltInFnType, MaybeValue, Value};

pub const EXPORTED_BINDINGS: [(&str, BuiltInFnType); 2] =
    [("display", builtin_write), ("write", builtin_write)];

fn builtin_write(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    crate::utils::emit(&values[0].to_string());
    Ok(MaybeValue::Just(Value::Unspecified))
}
