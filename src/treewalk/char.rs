use super::{BuiltInFnType, MaybeValue, Value};

pub const EXPORTED_BINDINGS: [(&str, BuiltInFnType); 1] =
    [("char-whitespace?", builtin_ischarwhitespace)];

fn builtin_ischarwhitespace(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Char-whitespace? needs exactly one argument".to_string());
    }
    let Some(Value::Char(c)) = values.first() else {
        return Err("Char-whitespace? takes a char as argument".to_string());
    };
    Ok(MaybeValue::Just(Value::Bool(c.is_whitespace())))
}
