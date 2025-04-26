use super::{BuiltInFnType, Callable, MaybeValue, Value, ValueRef};
use crate::parser::{parse_expression, Tokenizer};
use std::io::{self, BufRead};

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
    ("=", builtin_iseq),
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

fn builtin_add(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(0), |acc, x| acc.add(&x.borrow()))?;
    Ok(MaybeValue::Just(res.into()))
}

fn builtin_mul(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(1), |acc, x| acc.mul(&x.borrow()))?;
    Ok(MaybeValue::Just(res.into()))
}

fn builtin_sub(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let Some(v) = values_iter.next() else {
        return Ok(MaybeValue::Just(Value::Integer(0).into()));
    };
    let mut res = (*v.borrow()).clone();
    for v in values_iter {
        res = res.sub(&v.borrow())?
    }
    Ok(MaybeValue::Just(res.into()))
}

fn builtin_abs(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Abs needs exactly one argument".to_string());
    }
    let res = match values.first() {
        Some(value) => match &*value.borrow() {
            Value::Integer(n) => Value::Integer(if *n >= 0 { *n } else { -*n }),
            Value::Float(n) => Value::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("Abs needs a number argument".to_string()),
        },
        _ => return Err("Abs needs exactly one argument".to_string()),
    };
    Ok(MaybeValue::Just(res.into()))
}

fn builtin_div(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let Some(v) = values_iter.next() else {
        return Ok(MaybeValue::Just(Value::Integer(1).into()));
    };
    let mut res = (*v.borrow()).clone();
    for v in values_iter {
        res = res.div(&v.borrow())?
    }
    Ok(MaybeValue::Just(res.into()))
}

fn builtin_quotient(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (&*a.borrow(), &*b.borrow()) {
            (Value::Integer(a), Value::Integer(b)) => {
                Ok(MaybeValue::Just(Value::Integer(a / b).into()))
            }
            _ => Err("Quotient needs integer arguments".to_string()),
        },
        _ => Err("Quotient needs exactly two arguments".to_string()),
    }
}

fn builtin_remainder(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (&*a.borrow(), &*b.borrow()) {
            (Value::Integer(a), Value::Integer(b)) => {
                Ok(MaybeValue::Just(Value::Integer(a % b).into()))
            }
            _ => Err("Remainder needs integer arguments".to_string()),
        },
        _ => Err("Remainder needs exactly two arguments".to_string()),
    }
}

fn builtin_modulo(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [a, b] => match (&*a.borrow(), &*b.borrow()) {
            (Value::Integer(a), Value::Integer(b)) => {
                let r = a % b;
                let m = if a.signum() != b.signum() { r + b } else { r };
                Ok(MaybeValue::Just(Value::Integer(m).into()))
            }
            _ => Err("Modulo needs integer arguments".to_string()),
        },
        _ => Err("Modulo needs exactly two arguments".to_string()),
    }
}

type CmpFnType = fn(&Value, &Value) -> Result<Value, String>;

fn builtin_cmp(values: Vec<ValueRef>, method: CmpFnType) -> Result<MaybeValue, String> {
    for (a, b) in values.iter().zip(values.iter().skip(1)) {
        match method(&a.borrow(), &b.borrow()) {
            Ok(Value::Bool(false)) => return Ok(MaybeValue::Just(Value::Bool(false).into())),
            Err(s) => return Err(s),
            _ => (),
        }
    }
    Ok(MaybeValue::Just(Value::Bool(true).into()))
}

fn builtin_lt(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::lt)
}

fn builtin_gt(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::gt)
}

fn builtin_leq(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::leq)
}

fn builtin_geq(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::geq)
}

fn builtin_iseq(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Value::iseq)
}

fn builtin_not(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Not needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(match &*values[0].borrow() {
            Value::Bool(b) => !b,
            _ => false,
        })
        .into(),
    ))
}

fn builtin_list(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    Ok(MaybeValue::Just(Value::from_slice_ref(&values).into()))
}

fn builtin_apply(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Apply needs exactly two arguments".to_string());
    }
    match &*values[0].borrow() {
        Value::Procedure(proc) => Ok(MaybeValue::TailCall(
            proc.clone(),
            values[1].borrow().borrow_vec()?,
        )),
        _ => Err("Apply needs a procedure and a list as arguments".to_string()),
    }
}

fn builtin_length(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument".to_string());
    }
    match values[0].borrow().borrow_vec() {
        Ok(v) => Ok(MaybeValue::Just(Value::Integer(v.len() as i64).into())),
        _ => Err("Cannot compute length (is it a list?)".to_string()),
    }
}

fn builtin_append(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let mut all = Vec::new();
    for value in values {
        let borrowed = &*value.borrow();
        match borrowed {
            Value::Pair { .. } => all.extend(borrowed.borrow_vec()?),
            Value::Null => (),
            _ => return Err("Append needs lists as arguments".to_string()),
        }
    }
    Ok(MaybeValue::Just(Value::from_slice_ref(&all).into()))
}

fn builtin_ispair(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Pair { .. })).into(),
    ))
}

fn builtin_islist(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument".to_string());
    }
    let mut res = true;
    let mut cur = values[0].clone();
    loop {
        let borrowed = cur.borrow();
        match &*borrowed {
            Value::Null => break,
            Value::Pair { car: _, cdr } => {
                let next = cdr.clone();
                drop(borrowed);
                cur = next;
            }
            _ => {
                res = false;
                break;
            }
        }
    }
    Ok(MaybeValue::Just(Value::Bool(res).into()))
}

fn builtin_isnull(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Null)).into(),
    ))
}

fn builtin_isnumber(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(
            &*values[0].borrow(),
            Value::Float(_) | Value::Integer(_)
        ))
        .into(),
    ))
}

fn builtin_issymbol(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Symbol(_))).into(),
    ))
}

fn builtin_isstring(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("String? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Str(_))).into(),
    ))
}

fn builtin_isboolean(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Boolean? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Bool(_))).into(),
    ))
}

fn builtin_isprocedure(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Procedure? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Bool(matches!(&*values[0].borrow(), Value::Procedure(_))).into(),
    ))
}

fn builtin_iseven(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Even? needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 == 0).into())),
        _ => Err("Even? needs an integer argument".to_string()),
    }
}

fn builtin_isodd(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Odd? needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 != 0).into())),
        _ => Err("Odd? needs an integer argument".to_string()),
    }
}

fn builtin_ispositive(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Positive? needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v > 0).into())),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v > 0 as f64).into())),
        _ => Err("Positive? needs a number argument".to_string()),
    }
}

fn builtin_isnegative(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Negative? needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v < 0).into())),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v < 0 as f64).into())),
        _ => Err("Negative? needs a number argument".to_string()),
    }
}

fn builtin_iszero(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Zero? needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v == 0).into())),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v == 0 as f64).into())),
        _ => Err("Zero? needs a number argument".to_string()),
    }
}

fn builtin_cons(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Cons needs exactly two arguments".to_string());
    }
    Ok(MaybeValue::Just(
        Value::Pair {
            car: values[0].clone(),
            cdr: values[1].clone(),
        }
        .into(),
    ))
}

fn builtin_car(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Pair { car, cdr: _ } => Ok(MaybeValue::Just(car.clone())),
        _ => Err("Car needs a pair as argument".to_string()),
    }
}

fn builtin_cdr(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument".to_string());
    }
    match &*values[0].borrow() {
        Value::Pair { car: _, cdr } => Ok(MaybeValue::Just(cdr.clone())),
        _ => Err("Cdr needs a pair as argument".to_string()),
    }
}

fn builtin_setcar(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Set-car needs exactly two arguments".to_string());
    }
    let first = &mut *values[0].borrow_mut();
    match first {
        Value::Pair { car, cdr: _ } => {
            *car = values[1].clone();
            Ok(MaybeValue::Just(Value::Unspecified.into()))
        }
        _ => Err("Set-car needs a pair as first argument".to_string()),
    }
}

fn builtin_setcdr(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Set-cdr needs exactly two arguments".to_string());
    }
    let first = &mut *values[0].borrow_mut();
    match first {
        Value::Pair { car: _, cdr } => {
            *cdr = values[1].clone();
            Ok(MaybeValue::Just(Value::Unspecified.into()))
        }
        _ => Err("Set-cdr needs a pair as first argument".to_string()),
    }
}

fn builtin_filter(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Filter needs exactly two arguments".to_string());
    }
    match &*values[0].borrow() {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in values[1].borrow().borrow_vec()? {
                if proc.call(vec![x.clone()])?.materialize()? == Value::Bool(true).into() {
                    v.push(x)
                }
            }
            Ok(MaybeValue::Just(Value::from_slice_ref(&v).into()))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_map(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Map needs exactly two arguments".to_string());
    }
    match &*values[0].borrow() {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in values[1].borrow().borrow_vec()? {
                v.push(proc.call(vec![x])?.materialize()?)
            }
            Ok(MaybeValue::Just(Value::from_slice_ref(&v).into()))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_reverse(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Reverse needs exactly one argument".to_string());
    }
    let res = &mut *values[0].borrow().borrow_vec()?;
    res.reverse();
    Ok(MaybeValue::Just(Value::from_slice_ref(res).into()))
}

fn builtin_read(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Read takes no arguments".to_string());
    }
    let mut input = String::new();
    let _ = io::stdin().lock().read_line(&mut input);
    loop {
        let mut tokens = Tokenizer::new(&input).peekable();
        if let Ok(expr) = parse_expression(&mut tokens) {
            return Ok(MaybeValue::Just(Value::from(expr).into()));
        }
        let _ = io::stdin().lock().read_line(&mut input);
    }
}

fn builtin_write(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    print!("{}", &*values[0].borrow());
    Ok(MaybeValue::Just(Value::Unspecified.into()))
}

fn builtin_newline(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Write takes no arguments".to_string());
    }
    println!();
    Ok(MaybeValue::Just(Value::Unspecified.into()))
}

fn builtin_listtail(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("List-tail needs exactly two arguments".to_string());
    }
    let Value::Integer(mut k) = &*values[1].borrow() else {
        return Err("List-tail needs integer as second argument".to_string());
    };
    let mut cur = values[0].clone();
    loop {
        if k == 0 {
            return Ok(MaybeValue::Just(cur.clone()));
        }
        let borrowed = cur.borrow();
        match &*borrowed {
            Value::Null => {
                return Err("Exceeded list length".to_string());
            }
            Value::Pair { car: _, cdr } => {
                k -= 1;
                let next = cdr.clone();
                drop(borrowed);
                cur = next;
            }
            _ => {
                return Err("Not a proper list".to_string());
            }
        }
    }
}

fn builtin_listref(values: Vec<ValueRef>) -> Result<MaybeValue, String> {
    let MaybeValue::Just(pair) = builtin_listtail(values)? else {
        return Err("List-tail did not return a value?".into());
    };
    let Value::Pair { car, cdr: _ } = &*pair.borrow() else {
        return Err("List-tail did not return a pair?".into());
    };
    Ok(MaybeValue::Just(car.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_add() {
        let values = vec![Value::Integer(10).into(), Value::Float(42.0).into()];

        assert_eq!(
            builtin_add(values),
            Ok(MaybeValue::Just(Value::Float(52.0).into()))
        );

        let values = vec![Value::Float(42.0).into(), Value::Integer(13).into()];

        assert_eq!(
            builtin_add(values),
            Ok(MaybeValue::Just(Value::Float(55.0).into()))
        );

        let values = vec![
            Value::Float(42.0).into(),
            Value::Integer(13).into(),
            Value::Str("hey, hey".to_string()).into(),
        ];

        assert_eq!(builtin_add(values), Err("Cannot add types".to_string()));
    }
}
