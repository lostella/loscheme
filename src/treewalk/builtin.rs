use super::{BuiltInFnType, Callable, MaybeValue, Value};
use crate::parser::{parse_expression, Tokenizer};
use std::cell::RefCell;
use std::io::{self, BufRead};
use std::rc::Rc;

pub const BUILTIN_BINDINGS: [(&str, BuiltInFnType); 44] = [
    ("+", builtin_add),
    ("-", builtin_sub),
    ("*", builtin_mul),
    ("/", builtin_div),
    ("abs", builtin_abs),
    ("<", builtin_lt),
    (">", builtin_gt),
    ("<=", builtin_leq),
    (">=", builtin_geq),
    ("=", builtin_isnumeq),
    ("not", builtin_not),
    ("list", builtin_list),
    ("apply", builtin_apply),
    ("length", builtin_length),
    ("append", builtin_append),
    ("pair?", builtin_ispair),
    ("list?", builtin_islist),
    ("null?", builtin_isnull),
    ("number?", builtin_isnumber),
    ("symbol?", builtin_issymbol),
    ("string?", builtin_isstring),
    ("boolean?", builtin_isboolean),
    ("procedure?", builtin_isprocedure),
    ("even?", builtin_iseven),
    ("odd?", builtin_isodd),
    ("positive?", builtin_ispositive),
    ("negative?", builtin_isnegative),
    ("zero?", builtin_iszero),
    ("cons", builtin_cons),
    ("car", builtin_car),
    ("cdr", builtin_cdr),
    ("set-car!", builtin_setcar),
    ("set-cdr!", builtin_setcdr),
    ("filter", builtin_filter),
    ("map", builtin_map),
    ("reverse", builtin_reverse),
    ("read", builtin_read),
    ("write", builtin_write),
    ("newline", builtin_newline),
    ("quotient", builtin_quotient),
    ("remainder", builtin_remainder),
    ("modulo", builtin_modulo),
    ("list-ref", builtin_listref),
    ("list-tail", builtin_listtail),
];

fn builtin_add(values: Vec<Value>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(0), |acc, x| acc.add(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_mul(values: Vec<Value>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(1), |acc, x| acc.mul(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_sub(values: Vec<Value>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let Some(v) = values_iter.next() else {
        return Ok(MaybeValue::Just(Value::Integer(0)));
    };
    let mut res = v.clone();
    for v in values_iter {
        res = res.sub(&v)?
    }
    Ok(MaybeValue::Just(res))
}

fn builtin_abs(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Abs needs exactly one argument".to_string());
    }
    let res = match values.first() {
        Some(value) => match value {
            Value::Integer(n) => Value::Integer(if *n >= 0 { *n } else { -*n }),
            Value::Float(n) => Value::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("Abs needs a number argument".to_string()),
        },
        _ => return Err("Abs needs exactly one argument".to_string()),
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_div(values: Vec<Value>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let Some(v) = values_iter.next() else {
        return Ok(MaybeValue::Just(Value::Integer(1)));
    };
    let mut res = v.clone();
    for v in values_iter {
        res = res.div(&v)?
    }
    Ok(MaybeValue::Just(res))
}

fn builtin_quotient(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(MaybeValue::Just(Value::Integer(a / b))),
            _ => Err("Quotient needs integer arguments".to_string()),
        },
        _ => Err("Quotient needs exactly two arguments".to_string()),
    }
}

fn builtin_remainder(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => Ok(MaybeValue::Just(Value::Integer(a % b))),
            _ => Err("Remainder needs integer arguments".to_string()),
        },
        _ => Err("Remainder needs exactly two arguments".to_string()),
    }
}

fn builtin_modulo(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => {
                let r = a % b;
                let m = if a.signum() != b.signum() { r + b } else { r };
                Ok(MaybeValue::Just(Value::Integer(m)))
            }
            _ => Err("Modulo needs integer arguments".to_string()),
        },
        _ => Err("Modulo needs exactly two arguments".to_string()),
    }
}

type CmpFnType = fn(&Value, &Value) -> Result<Value, String>;

fn builtin_cmp(values: Vec<Value>, method: CmpFnType) -> Result<MaybeValue, String> {
    for (a, b) in values.iter().zip(values.iter().skip(1)) {
        match method(a, b) {
            Ok(Value::Bool(false)) => return Ok(MaybeValue::Just(Value::Bool(false))),
            Err(s) => return Err(s),
            _ => (),
        }
    }
    Ok(MaybeValue::Just(Value::Bool(true)))
}

fn builtin_lt(values: Vec<Value>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::lt)
}

fn builtin_gt(values: Vec<Value>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::gt)
}

fn builtin_leq(values: Vec<Value>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::leq)
}

fn builtin_geq(values: Vec<Value>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::geq)
}

fn builtin_isnumeq(values: Vec<Value>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::iseq)
}

fn builtin_not(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Not needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(match values[0] {
        Value::Bool(b) => !b,
        _ => false,
    })))
}

fn builtin_list(values: Vec<Value>) -> Result<MaybeValue, String> {
    Ok(MaybeValue::Just(Value::from_slice(&values)))
}

fn builtin_apply(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Apply needs exactly two arguments".to_string());
    }
    match &values[0] {
        Value::Procedure(proc) => Ok(MaybeValue::TailCall(
            proc.clone(),
            values[1].clone().into_vec()?,
        )),
        _ => Err("Apply needs a procedure and a list as arguments".to_string()),
    }
}

fn builtin_length(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument".to_string());
    }
    match values[0].clone().into_vec() {
        Ok(v) => Ok(MaybeValue::Just(Value::Integer(v.len() as i64))),
        _ => Err("Cannot compute length (is it a list?)".to_string()),
    }
}

fn builtin_append(values: Vec<Value>) -> Result<MaybeValue, String> {
    let mut all = Vec::new();
    for value in values {
        match value {
            Value::Pair(_) => all.extend(value.into_vec()?),
            Value::Null => (),
            _ => return Err("Append needs lists as arguments".to_string()),
        }
    }
    Ok(MaybeValue::Just(Value::from_slice(&all)))
}

fn builtin_ispair(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Pair(_)
    ))))
}

fn builtin_islist(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument".to_string());
    }
    let mut cur = values[0].clone();
    loop {
        match cur {
            Value::Null => return Ok(MaybeValue::Just(Value::Bool(true))),
            Value::Pair(p) => {
                cur = p.borrow().1.clone();
            }
            _ => return Ok(MaybeValue::Just(Value::Bool(false))),
        }
    }
}

fn builtin_isnull(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Null
    ))))
}

fn builtin_isnumber(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Float(_) | Value::Integer(_)
    ))))
}

fn builtin_issymbol(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Symbol(_)
    ))))
}

fn builtin_isstring(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("String? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Str(_)
    ))))
}

fn builtin_isboolean(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Boolean? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Bool(_)
    ))))
}

fn builtin_isprocedure(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Procedure? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Procedure(_)
    ))))
}

fn builtin_iseven(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Even? needs exactly one argument".to_string());
    }
    match values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 == 0))),
        _ => Err("Even? needs an integer argument".to_string()),
    }
}

fn builtin_isodd(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Odd? needs exactly one argument".to_string());
    }
    match values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 != 0))),
        _ => Err("Odd? needs an integer argument".to_string()),
    }
}

fn builtin_ispositive(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Positive? needs exactly one argument".to_string());
    }
    match values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v > 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(v > 0 as f64))),
        _ => Err("Positive? needs a number argument".to_string()),
    }
}

fn builtin_isnegative(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Negative? needs exactly one argument".to_string());
    }
    match values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v < 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(v < 0 as f64))),
        _ => Err("Negative? needs a number argument".to_string()),
    }
}

fn builtin_iszero(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Zero? needs exactly one argument".to_string());
    }
    match values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v == 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(v == 0 as f64))),
        _ => Err("Zero? needs a number argument".to_string()),
    }
}

fn builtin_cons(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Cons needs exactly two arguments".to_string());
    }
    Ok(MaybeValue::Just(Value::Pair(Rc::new(RefCell::new((
        values[0].clone(),
        values[1].clone(),
    ))))))
}

fn builtin_car(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Pair(p) => Ok(MaybeValue::Just(p.borrow().0.clone())),
        _ => Err("Car needs a pair as argument".to_string()),
    }
}

fn builtin_cdr(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Pair(p) => Ok(MaybeValue::Just(p.borrow().1.clone())),
        _ => Err("Cdr needs a pair as argument".to_string()),
    }
}

fn builtin_setcar(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Set-car needs exactly two arguments".to_string());
    }
    match &values[0] {
        Value::Pair(p) => {
            let mut borrowed = p.borrow_mut();
            borrowed.0 = values[1].clone();
            Ok(MaybeValue::Just(Value::Unspecified))
        }
        _ => Err("Set-car needs a pair as first argument".to_string()),
    }
}

fn builtin_setcdr(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Set-cdr needs exactly two arguments".to_string());
    }
    match &values[0] {
        Value::Pair(p) => {
            let mut borrowed = p.borrow_mut();
            borrowed.1 = values[1].clone();
            Ok(MaybeValue::Just(Value::Unspecified))
        }
        _ => Err("Set-cdr needs a pair as first argument".to_string()),
    }
}

fn builtin_filter(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Filter needs exactly two arguments".to_string());
    }
    match &values[0] {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in values[1].clone().into_vec()? {
                if proc.call(vec![x.clone()])?.materialize()? == Value::Bool(true) {
                    v.push(x)
                }
            }
            Ok(MaybeValue::Just(Value::from_slice(&v)))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_map(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Map needs exactly two arguments".to_string());
    }
    match &values[0] {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in values[1].clone().into_vec()? {
                v.push(proc.call(vec![x])?.materialize()?)
            }
            Ok(MaybeValue::Just(Value::from_slice(&v)))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_reverse(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Reverse needs exactly one argument".to_string());
    }
    let mut res = values[0].clone().into_vec()?;
    res.reverse();
    Ok(MaybeValue::Just(Value::from_slice(&res)))
}

fn builtin_read(values: Vec<Value>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Read takes no arguments".to_string());
    }
    let mut input = String::new();
    let _ = io::stdin().lock().read_line(&mut input);
    loop {
        let mut tokens = Tokenizer::new(&input).peekable();
        if let Ok(expr) = parse_expression(&mut tokens) {
            return Ok(MaybeValue::Just(Value::from(expr)));
        }
        let _ = io::stdin().lock().read_line(&mut input);
    }
}

fn builtin_write(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    print!("{}", values[0]);
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_newline(values: Vec<Value>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Write takes no arguments".to_string());
    }
    println!();
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_listtail(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("List-tail needs exactly two arguments".to_string());
    }
    let Value::Integer(mut k) = values[1] else {
        return Err("List-tail needs integer as second argument".to_string());
    };
    let mut cur = values[0].clone();
    loop {
        if k == 0 {
            return Ok(MaybeValue::Just(cur.clone()));
        }
        match cur {
            Value::Null => {
                return Err("Exceeded list length".to_string());
            }
            Value::Pair(p) => {
                k -= 1;
                cur = p.borrow().1.clone();
            }
            _ => {
                return Err("Not a proper list".to_string());
            }
        }
    }
}

fn builtin_listref(values: Vec<Value>) -> Result<MaybeValue, String> {
    let MaybeValue::Just(pair) = builtin_listtail(values)? else {
        return Err("List-tail did not return a value?".into());
    };
    if let Value::Pair(p) = pair {
        Ok(MaybeValue::Just(p.borrow().0.clone()))
    } else {
        Err("List-tail did not return a pair?".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_add() {
        let values = vec![Value::Integer(10), Value::Float(42.0)];

        assert_eq!(
            builtin_add(values),
            Ok(MaybeValue::Just(Value::Float(52.0)))
        );

        let values = vec![Value::Float(42.0), Value::Integer(13)];

        assert_eq!(
            builtin_add(values),
            Ok(MaybeValue::Just(Value::Float(55.0)))
        );

        let values = vec![
            Value::Float(42.0),
            Value::Integer(13),
            Value::Str("hey, hey".to_string().into()),
        ];

        assert_eq!(builtin_add(values), Err("Cannot add types".to_string()));
    }
}
