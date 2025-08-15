use super::{make_rational, BuiltInFnType, Callable, MaybeValue, Value};
use crate::parser::{parse_expression, Tokenizer};
use std::cell::RefCell;
use std::io::{self, BufRead};
use std::mem::take;
use std::rc::Rc;

pub const BUILTIN_BINDINGS: [(&str, BuiltInFnType); 94] = [
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
    ("eqv?", builtin_iseqv),
    ("eq?", builtin_iseqv),
    ("equal?", builtin_isequal),
    ("pair?", builtin_ispair),
    ("list?", builtin_islist),
    ("null?", builtin_isnull),
    ("number?", builtin_isnumber),
    ("integer?", builtin_isinteger),
    ("symbol?", builtin_issymbol),
    ("string?", builtin_isstring),
    ("char?", builtin_ischar),
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
    ("caar", builtin_caar),
    ("cadr", builtin_cadr),
    ("cdar", builtin_cdar),
    ("cddr", builtin_cddr),
    ("caaar", builtin_caaar),
    ("caadr", builtin_caadr),
    ("cadar", builtin_cadar),
    ("caddr", builtin_caddr),
    ("cdaar", builtin_cdaar),
    ("cdadr", builtin_cdadr),
    ("cddar", builtin_cddar),
    ("cdddr", builtin_cdddr),
    ("caaaar", builtin_caaaar),
    ("caaadr", builtin_caaadr),
    ("caadar", builtin_caadar),
    ("caaddr", builtin_caaddr),
    ("cadaar", builtin_cadaar),
    ("cadadr", builtin_cadadr),
    ("caddar", builtin_caddar),
    ("cadddr", builtin_cadddr),
    ("cdaaar", builtin_cdaaar),
    ("cdaadr", builtin_cdaadr),
    ("cdadar", builtin_cdadar),
    ("cdaddr", builtin_cdaddr),
    ("cddaar", builtin_cddaar),
    ("cddadr", builtin_cddadr),
    ("cdddar", builtin_cdddar),
    ("cddddr", builtin_cddddr),
    ("set-car!", builtin_setcar),
    ("set-cdr!", builtin_setcdr),
    ("filter", builtin_filter),
    ("map", builtin_map),
    ("reverse", builtin_reverse),
    ("read", builtin_read),
    ("newline", builtin_newline),
    ("write-char", builtin_writechar),
    ("write-string", builtin_writestring),
    ("quotient", builtin_quotient),
    ("remainder", builtin_remainder),
    ("modulo", builtin_modulo),
    ("make-list", builtin_makelist),
    ("list-ref", builtin_listref),
    ("list-tail", builtin_listtail),
    ("vector", builtin_vector),
    ("vector?", builtin_isvector),
    ("vector-length", builtin_vectorlength),
    ("make-vector", builtin_makevector),
    ("vector-ref", builtin_vectorref),
    ("vector-set!", builtin_vectorset),
    ("list->vector", builtin_listvector),
    ("vector->list", builtin_vectorlist),
    ("sqrt", builtin_sqrt),
    ("sin", builtin_sin),
    ("cos", builtin_cos),
    ("tan", builtin_tan),
    ("asin", builtin_asin),
    ("acos", builtin_acos),
    ("atan", builtin_atan),
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
    let Some((first, rest)) = values.split_first() else {
        return Err("- needs at least one argument".to_string());
    };
    if rest.is_empty() {
        return Ok(MaybeValue::Just(Value::Integer(0).sub(first)?));
    }
    let mut res = first.clone();
    for v in rest {
        res = res.sub(v)?;
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
    let Some((first, rest)) = values.split_first() else {
        return Err("/ needs at least one argument".to_string());
    };
    if rest.is_empty() {
        return Ok(MaybeValue::Just(Value::Integer(1).div(first)?));
    }
    let mut res = first.clone();
    for v in rest {
        res = res.div(v)?;
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
                let m = if a.signum() == b.signum() { r } else { r + b };
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
    let res = match values.first() {
        Some(Value::Bool(b)) => !b,
        None => return Err("Unreachable reached".to_string()),
        _ => false,
    };
    Ok(MaybeValue::Just(Value::Bool(res)))
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

pub fn eqv(first: &Value, second: &Value) -> bool {
    match (first, second) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(na, da), Value::Rational(nb, db)) => {
            make_rational(*na, *da) == make_rational(*nb, *db)
        }
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => Rc::ptr_eq(a, b),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Procedure(a), Value::Procedure(b)) => Rc::ptr_eq(a, b),
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        _ => false,
    }
}

fn builtin_iseqv(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Eqv? needs exactly two arguments".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(eqv(&values[0], &values[1]))))
}

fn builtin_isequal(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Equal? needs exactly two arguments".to_string());
    }
    let res = match (&values[0], &values[1]) {
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Pair(_), Value::Pair(_)) => {
            values[0].clone().into_vec() == values[1].clone().into_vec()
        }
        _ => return builtin_iseqv(values),
    };
    Ok(MaybeValue::Just(Value::Bool(res)))
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

fn builtin_isinteger(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Integer? needs exactly one argument".to_string());
    }
    let res = match values[0] {
        Value::Integer(_) => true,
        Value::Rational(_, d) => d == 1,
        Value::Float(f) => f.fract() == 0.0,
        _ => false,
    };
    Ok(MaybeValue::Just(Value::Bool(res)))
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

fn builtin_ischar(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Char? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Char(_)
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

fn builtin_car(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.into())
}

fn builtin_cdr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.into())
}

fn builtin_caar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.into())
}

fn builtin_cadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.into())
}

fn builtin_cdar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.into())
}

fn builtin_cddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.into())
}

fn builtin_caaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.car()?.into())
}

fn builtin_caadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.car()?.into())
}

fn builtin_cadar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cadar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.car()?.into())
}

fn builtin_caddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.car()?.into())
}

fn builtin_cdaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.cdr()?.into())
}

fn builtin_cdadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.cdr()?.into())
}

fn builtin_cddar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cddar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.cdr()?.into())
}

fn builtin_cdddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.cdr()?.into())
}

fn builtin_caaaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caaaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.car()?.car()?.into())
}

fn builtin_caaadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caaadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.car()?.car()?.into())
}

fn builtin_caadar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caadar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.car()?.car()?.into())
}

fn builtin_caaddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caaddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.car()?.car()?.into())
}

fn builtin_cadaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cadaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.cdr()?.car()?.into())
}

fn builtin_cadadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cadadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.cdr()?.car()?.into())
}

fn builtin_caddar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Caddar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.cdr()?.car()?.into())
}

fn builtin_cadddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cadddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.cdr()?.car()?.into())
}

fn builtin_cdaaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdaaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.car()?.cdr()?.into())
}

fn builtin_cdaadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdaadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.car()?.cdr()?.into())
}

fn builtin_cdadar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdadar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.car()?.cdr()?.into())
}

fn builtin_cdaddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdaddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.car()?.cdr()?.into())
}

fn builtin_cddaar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cddaar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.car()?.cdr()?.cdr()?.into())
}

fn builtin_cddadr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cddadr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.car()?.cdr()?.cdr()?.into())
}

fn builtin_cdddar(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdddar needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).car()?.cdr()?.cdr()?.cdr()?.into())
}

fn builtin_cddddr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cddddr needs exactly one argument".to_string());
    }
    Ok(take(&mut values[0]).cdr()?.cdr()?.cdr()?.cdr()?.into())
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

fn builtin_newline(values: Vec<Value>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Newline takes no arguments".to_string());
    }
    println!();
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_writechar(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() > 1 {
        return Err("Write-char takes one argument".to_string());
    }
    let Some(Value::Char(c)) = values.first() else {
        return Err("Write-char takes a char as argument".to_string());
    };
    print!("{c}");
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_writestring(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() > 1 {
        return Err("Write-string takes one argument".to_string());
    }
    let Some(Value::Str(s)) = values.first() else {
        return Err("Write-string takes a string as argument".to_string());
    };
    print!("{s}");
    Ok(MaybeValue::Just(Value::Unspecified))
}

fn builtin_makelist(values: Vec<Value>) -> Result<MaybeValue, String> {
    let fill = if values.len() == 1 {
        Value::Integer(0)
    } else if values.len() == 2 {
        values[1].clone()
    } else {
        return Err("Make-list expects one or two arguments".to_string());
    };
    let Value::Integer(qty) = values[0] else {
        return Err("Make-list expects an integer as first argument".to_string());
    };
    let data = vec![fill; qty as usize];
    Ok(MaybeValue::Just(Value::from_slice(&data)))
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

fn builtin_vector(values: Vec<Value>) -> Result<MaybeValue, String> {
    Ok(MaybeValue::Just(Value::Vector(Rc::new(values.into()))))
}

fn builtin_isvector(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Vector? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        values[0],
        Value::Vector(_)
    ))))
}

fn builtin_vectorlength(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Vector-length needs exactly one argument".to_string());
    }
    let Value::Vector(v) = &values[0] else {
        return Err("Vector-length expects a vector as argument".to_string());
    };
    Ok(MaybeValue::Just(Value::Integer(v.borrow().len() as i64)))
}

fn builtin_makevector(values: Vec<Value>) -> Result<MaybeValue, String> {
    let fill = if values.len() == 1 {
        Value::Integer(0)
    } else if values.len() == 2 {
        values[1].clone()
    } else {
        return Err("Make-vector expects one or two arguments".to_string());
    };
    let Value::Integer(qty) = values[0] else {
        return Err("Make-vector expects an integer as first argument".to_string());
    };
    Ok(MaybeValue::Just(Value::Vector(Rc::new(
        vec![fill; qty as usize].into(),
    ))))
}

fn builtin_vectorref(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Vector-ref needs exactly two argument".to_string());
    }
    let Value::Vector(v) = &values[0] else {
        return Err("Vector-ref expects a vector as first argument".to_string());
    };
    let Value::Integer(k) = &values[1] else {
        return Err("Vector-ref expects an integer as second argument".to_string());
    };
    Ok(MaybeValue::Just(v.borrow()[*k as usize].clone()))
}

fn builtin_vectorset(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 3 {
        return Err("Vector-set! needs exactly three argument".to_string());
    }
    let (fst, snd) = values.split_at_mut(1);
    let Value::Integer(k) = &snd[0] else {
        return Err("Vector-set! expects an integer as second argument".to_string());
    };
    match &mut fst[0] {
        Value::Vector(rc) => {
            rc.borrow_mut()[*k as usize] = snd[1].clone();
            Ok(MaybeValue::Just(Value::Unspecified))
        }
        _ => Err("Vector-set! expects a vector as first argument".to_string()),
    }
}

fn builtin_listvector(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("List->vector needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Vector(Rc::new(
        values[0].clone().into_vec()?.into(),
    ))))
}

fn builtin_vectorlist(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Vector->list needs exactly one argument".to_string());
    }
    let Value::Vector(rc) = &values[0] else {
        return Err("Vector->list needs a vector as argument".to_string());
    };
    Ok(MaybeValue::Just(Value::from_slice(&rc.borrow())))
}

fn builtin_sqrt(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Sqrt expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).sqrt()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.sqrt()))),
        _ => Err("Unsupported type for sqrt".to_string()),
    }
}

fn builtin_sin(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Sin expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).sin()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.sin()))),
        _ => Err("Unsupported type for sin".to_string()),
    }
}

fn builtin_cos(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Cos expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).cos()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.cos()))),
        _ => Err("Unsupported type for cos".to_string()),
    }
}

fn builtin_tan(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Tan expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).tan()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.tan()))),
        _ => Err("Unsupported type for tan".to_string()),
    }
}

fn builtin_asin(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Asin expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).asin()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.asin()))),
        _ => Err("Unsupported type for asin".to_string()),
    }
}

fn builtin_acos(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Acos expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).acos()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.acos()))),
        _ => Err("Unsupported type for acos".to_string()),
    }
}

fn builtin_atan(values: Vec<Value>) -> Result<MaybeValue, String> {
    let [x] = values.as_slice() else {
        return Err("Atan expects one argument".to_string());
    };
    match x {
        Value::Integer(n) => Ok(MaybeValue::Just(Value::Float((*n as f64).atan()))),
        Value::Float(f) => Ok(MaybeValue::Just(Value::Float(f.atan()))),
        _ => Err("Unsupported type for atan".to_string()),
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
