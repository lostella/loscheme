use crate::parser::{parse_expression, Expr, Keyword, Tokenizer};
use crate::rationals::{lcm, simplify};
use internment::Intern;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use std::io::{self, BufRead};
use std::iter::zip;
use std::mem::take;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Cons {
    pub car: Value,
    pub cdr: Value,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Str(String),
    Bool(bool),
    Keyword(Keyword),
    Symbol(Intern<String>),
    Cons(Rc<Cons>),
    Procedure(Procedure),
    #[default]
    Unspecified,
}

impl From<Expr> for Value {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Null => Value::Null,
            Expr::Bool(x) => Value::Bool(x),
            Expr::Integer(x) => Value::Integer(x),
            Expr::Float(x) => Value::Float(x),
            Expr::Rational(x, y) => Value::Rational(x, y),
            Expr::Str(x) => Value::Str(x),
            Expr::Keyword(x) => Value::Keyword(x),
            Expr::Symbol(x) => Value::Symbol(x),
            Expr::Cons(x) => Value::Cons(Rc::new(Cons {
                car: Value::from(x.car.clone()),
                cdr: Value::from(x.cdr.clone()),
            })),
        }
    }
}

fn make_rational(num: i64, denom: i64) -> Value {
    let (num1, denom1) = simplify(num, denom);
    if denom1 == 1 {
        Value::Integer(num1)
    } else {
        Value::Rational(num1, denom1)
    }
}

impl Value {
    pub fn from_vec(mut v: Vec<Value>) -> Self {
        match v.len() {
            0 => Value::Null,
            _ => Value::Cons(Rc::new(Cons {
                car: take(&mut v[0]),
                cdr: Value::from_vec(v.split_off(1)),
            })),
        }
    }

    pub fn from_slice(v: &[Value]) -> Self {
        match v.len() {
            0 => Value::Null,
            _ => Value::Cons(Rc::new(Cons {
                car: v[0].clone(),
                cdr: Value::from_slice(&v[1..v.len()]),
            })),
        }
    }

    pub fn into_vec(self) -> Result<Vec<Value>, String> {
        let mut res = Vec::new();
        let mut cur = self;
        loop {
            match cur {
                Value::Cons(pair) => {
                    res.push(pair.car.clone());
                    cur = pair.cdr.clone();
                }
                Value::Null => return Ok(res),
                _ => return Err("Not a proper list".to_string()),
            }
        }
    }

    pub fn borrow_vec(&self) -> Result<Vec<&Value>, String> {
        let mut cur = self;
        let mut res = Vec::new();
        loop {
            match cur {
                Value::Null => break,
                Value::Cons(pair) => {
                    res.push(&pair.car);
                    cur = &pair.cdr;
                }
                _ => return Err("Not a proper list".to_string()),
            }
        }
        Ok(res)
    }

    pub fn add(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Float((*num as f64) / (*denom as f64) + b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Float(a + (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => {
                Ok(make_rational(num + (b * denom), *denom))
            }
            (Value::Integer(a), Value::Rational(num, denom)) => {
                Ok(make_rational((a * denom) + num, *denom))
            }
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                let denom = lcm(*denom1, *denom2);
                Ok(make_rational(
                    num1 * denom / denom1 + num2 * denom / denom2,
                    denom,
                ))
            }
            _ => Err("Cannot add types".to_string()),
        }
    }

    pub fn mul(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a * *b as f64)),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Float((*num as f64) / (*denom as f64) * b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Float(a * (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => {
                Ok(make_rational(*num * *b, *denom))
            }
            (Value::Integer(a), Value::Rational(num, denom)) => {
                Ok(make_rational(*a * *num, *denom))
            }
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(make_rational(num1 * num2, denom1 * denom2))
            }
            _ => Err("Cannot multiply types".to_string()),
        }
    }

    pub fn sub(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a - *b as f64)),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Float((*num as f64) / (*denom as f64) - b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Float(a - (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => {
                Ok(make_rational(num - (b * denom), *denom))
            }
            (Value::Integer(a), Value::Rational(num, denom)) => {
                Ok(make_rational((a * denom) - num, *denom))
            }
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                let denom = lcm(*denom1, *denom2);
                Ok(make_rational(
                    num1 * denom / denom1 - num2 * denom / denom2,
                    denom,
                ))
            }
            _ => Err("Cannot subtract types".to_string()),
        }
    }

    pub fn div(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Float(*a as f64 / *b as f64)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a / *b as f64)),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Float((*num as f64) / (*denom as f64) / b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Float(a / (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => {
                Ok(make_rational(*num, *denom * *b))
            }
            (Value::Integer(a), Value::Rational(num, denom)) => {
                Ok(make_rational(*num, *a * *denom))
            }
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(make_rational(num1 * denom2, denom1 * num2))
            }
            _ => Err("Cannot divide types".to_string()),
        }
    }

    pub fn lt(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) < *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a < (*b as f64))),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Bool((*num as f64) / (*denom as f64) < *b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(*a < (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => Ok(Value::Bool(*num < denom * b)),
            (Value::Integer(a), Value::Rational(num, denom)) => Ok(Value::Bool(*num < denom * a)),
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(Value::Bool(num1 * denom2 < num2 * denom1))
            }
            _ => Err("Cannot compare types".to_string()),
        }
    }

    pub fn gt(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) > *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a > (*b as f64))),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Bool((*num as f64) / (*denom as f64) > *b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(*a > (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => Ok(Value::Bool(*num > denom * b)),
            (Value::Integer(a), Value::Rational(num, denom)) => Ok(Value::Bool(*num > denom * a)),
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(Value::Bool(num1 * denom2 > num2 * denom1))
            }
            _ => Err("Cannot compare types".to_string()),
        }
    }

    pub fn leq(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) <= *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a <= (*b as f64))),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Bool((*num as f64) / (*denom as f64) <= *b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(*a <= (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => Ok(Value::Bool(*num <= denom * b)),
            (Value::Integer(a), Value::Rational(num, denom)) => Ok(Value::Bool(*num <= denom * a)),
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(Value::Bool(num1 * denom2 <= num2 * denom1))
            }
            _ => Err("Cannot compare types".to_string()),
        }
    }

    pub fn geq(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) >= *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a >= (*b as f64))),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Bool((*num as f64) / (*denom as f64) >= *b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(*a >= (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => Ok(Value::Bool(*num >= denom * b)),
            (Value::Integer(a), Value::Rational(num, denom)) => Ok(Value::Bool(*num >= denom * a)),
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(Value::Bool(num1 * denom2 >= num2 * denom1))
            }
            _ => Err("Cannot compare types".to_string()),
        }
    }

    pub fn iseq(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a == b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool(*a as f64 == *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a == *b as f64)),
            (Value::Rational(num, denom), Value::Float(b)) => {
                Ok(Value::Bool((*num as f64) / (*denom as f64) == *b))
            }
            (Value::Float(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(*a == (*num as f64) / (*denom as f64)))
            }
            (Value::Rational(num, denom), Value::Integer(b)) => {
                Ok(Value::Bool(num == b && *denom == 1))
            }
            (Value::Integer(a), Value::Rational(num, denom)) => {
                Ok(Value::Bool(num == a && *denom == 1))
            }
            (Value::Rational(num1, denom1), Value::Rational(num2, denom2)) => {
                Ok(Value::Bool(num1 == num2 && denom1 == denom2))
            }
            _ => Err("Cannot compare types".to_string()),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "()"),
            Value::Bool(v) => write!(f, "{}", if *v { "#t" } else { "#f" }),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Rational(n, d) => write!(f, "{}/{}", n, d),
            Value::Str(v) => write!(f, "\"{}\"", v),
            Value::Keyword(k) => write!(f, "{}", k),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Cons(p) => {
                write!(f, "(")?;
                let mut cur = p;
                loop {
                    write!(f, "{}", cur.car)?;
                    match &cur.cdr {
                        Value::Null => break,
                        Value::Cons(pp) => cur = pp,
                        _ => {
                            write!(f, " . {}", cur.cdr)?;
                            break;
                        }
                    }
                    write!(f, " ")?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Procedure(proc) => write!(f, "{}", proc),
            Value::Unspecified => Ok(()),
        }
    }
}

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
    let res = match values_iter.next() {
        None => Value::Integer(0),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.sub(&x))?,
    };
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
    let res = match values_iter.next() {
        None => Value::Integer(1),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.div(&x))?,
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_quotient(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Value::Integer(a), Value::Integer(b)] => Ok(MaybeValue::Just(Value::Integer(a / b))),
        _ => Err("Quotient needs exactly two arguments".to_string()),
    }
}

fn builtin_remainder(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Value::Integer(a), Value::Integer(b)] => Ok(MaybeValue::Just(Value::Integer(a % b))),
        _ => Err("Remainder needs exactly two arguments".to_string()),
    }
}

fn builtin_modulo(values: Vec<Value>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Value::Integer(a), Value::Integer(b)] => {
            let r = a % b;
            Ok(MaybeValue::Just(Value::Integer(
                if a.signum() != b.signum() { r + b } else { r },
            )))
        }
        _ => Err("Modulo needs exactly two integers".to_string()),
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

fn builtin_iseq(values: Vec<Value>) -> Result<MaybeValue, String> {
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
    Ok(MaybeValue::Just(Value::from_vec(values)))
}

fn builtin_apply(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Apply needs exactly two arguments".to_string());
    }
    let first = take(&mut values[0]);
    let second = take(&mut values[1]);
    match first {
        Value::Procedure(proc) => Ok(MaybeValue::TailCall(proc, second.into_vec()?)),
        _ => Err("Apply needs a procedure and a list as arguments".to_string()),
    }
}

fn builtin_length(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument".to_string());
    }
    match take(&mut values[0]).into_vec() {
        Ok(v) => Ok(MaybeValue::Just(Value::Integer(v.len() as i64))),
        _ => Err("Cannot compute length (is it a list?)".to_string()),
    }
}

fn builtin_append(values: Vec<Value>) -> Result<MaybeValue, String> {
    let mut all = Vec::new();
    for value in values {
        match value {
            Value::Cons(_) => all.extend(value.into_vec()?),
            Value::Null => (),
            _ => return Err("Append needs lists as arguments".to_string()),
        }
    }
    Ok(MaybeValue::Just(Value::from_vec(all)))
}

fn builtin_ispair(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Cons(_)
    ))))
}

fn builtin_islist(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Value::Cons(pair) => match builtin_islist(vec![pair.cdr.clone()])? {
            MaybeValue::Just(Value::Bool(b)) => Ok(MaybeValue::Just(Value::Bool(b))),
            _ => Err("Unexpected non-boolean".to_string()),
        },
        Value::Null => Ok(MaybeValue::Just(Value::Bool(true))),
        _ => Ok(MaybeValue::Just(Value::Bool(false))),
    }
}

fn builtin_isnull(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Null
    ))))
}

fn builtin_isnumber(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Float(_) | Value::Integer(_)
    ))))
}

fn builtin_issymbol(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Symbol(_)
    ))))
}

fn builtin_isstring(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("String? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Str(_)
    ))))
}

fn builtin_isboolean(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Boolean? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Bool(_)
    ))))
}

fn builtin_isprocedure(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Procedure? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Value::Bool(matches!(
        &values[0],
        Value::Procedure(_)
    ))))
}

fn builtin_iseven(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Even? needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 == 0))),
        _ => Err("Even? needs an integer argument".to_string()),
    }
}

fn builtin_isodd(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Odd? needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(v % 2 != 0))),
        _ => Err("Odd? needs an integer argument".to_string()),
    }
}

fn builtin_ispositive(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Positive? needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v > 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v > 0 as f64))),
        _ => Err("Positive? needs a number argument".to_string()),
    }
}

fn builtin_isnegative(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Negative? needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v < 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v < 0 as f64))),
        _ => Err("Negative? needs a number argument".to_string()),
    }
}

fn builtin_iszero(values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Zero? needs exactly one argument".to_string());
    }
    match &values[0] {
        Value::Integer(v) => Ok(MaybeValue::Just(Value::Bool(*v == 0))),
        Value::Float(v) => Ok(MaybeValue::Just(Value::Bool(*v == 0 as f64))),
        _ => Err("Zero? needs a number argument".to_string()),
    }
}

fn builtin_cons(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Cons needs exactly two arguments".to_string());
    }
    let car = take(&mut values[0]);
    let cdr = take(&mut values[1]);
    Ok(MaybeValue::Just(Value::Cons(Rc::new(Cons { car, cdr }))))
}

fn builtin_car(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Value::Cons(p) => Ok(MaybeValue::Just(p.car.clone())),
        _ => Err("Car needs a pair as argument".to_string()),
    }
}

fn builtin_cdr(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Value::Cons(p) => Ok(MaybeValue::Just(p.cdr.clone())),
        _ => Err("Cdr needs a pair as argument".to_string()),
    }
}

fn builtin_filter(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Filter needs exactly two arguments".to_string());
    }
    let pred = take(&mut values[0]);
    let orig = take(&mut values[1]).into_vec()?;
    match pred {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in orig {
                if proc.call(vec![x.clone()])?.materialize()? == Value::Bool(true) {
                    v.push(x)
                }
            }
            Ok(MaybeValue::Just(Value::from_vec(v)))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_map(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Map needs exactly two arguments".to_string());
    }
    let pred = take(&mut values[0]);
    let orig = take(&mut values[1]).into_vec()?;
    match pred {
        Value::Procedure(proc) => {
            let mut v = Vec::new();
            for x in orig {
                v.push(proc.call(vec![x])?.materialize()?)
            }
            Ok(MaybeValue::Just(Value::from_vec(v)))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_reverse(mut values: Vec<Value>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Reverse needs exactly one argument".to_string());
    }
    let mut res = take(&mut values[0]).into_vec()?;
    res.reverse();
    Ok(MaybeValue::Just(Value::from_vec(res)))
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
            return Ok(MaybeValue::Just(expr.into()));
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

#[derive(Debug, PartialEq, Clone)]
enum MaybeValue {
    Just(Value),
    TailCall(Procedure, Vec<Value>),
}

impl MaybeValue {
    fn materialize(self) -> Result<Value, String> {
        match self {
            Self::Just(expr) => Ok(expr),
            Self::TailCall(proc, args) => proc.call(args)?.materialize(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentNode {
    data: FxHashMap<Intern<String>, Value>,
    parent: Option<EnvironmentLink>,
}

type EnvironmentLink = Rc<RefCell<EnvironmentNode>>;

impl EnvironmentNode {
    #[inline(always)]
    pub fn set(&mut self, key: Intern<String>, value: Value) -> Option<Value> {
        self.data.insert(key, value)
    }

    #[inline(always)]
    pub fn get(&self, key: &Intern<String>) -> Option<Value> {
        match self.data.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(link) => link.borrow().get(key),
                None => None,
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    head: EnvironmentLink,
}

type BuiltInFnType = fn(Vec<Value>) -> Result<MaybeValue, String>;

impl Environment {
    pub fn empty() -> Environment {
        let node = EnvironmentNode {
            data: FxHashMap::default(),
            parent: None,
        };
        Environment {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn child(&self) -> Environment {
        let node = EnvironmentNode {
            data: FxHashMap::default(),
            parent: Some(self.head.clone()),
        };
        Environment {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn standard() -> Environment {
        let node = EnvironmentNode {
            data: FxHashMap::default(),
            parent: None,
        };
        let mut env = Environment {
            head: Rc::new(RefCell::new(node)),
        };
        // TODO add all built-in procedures in standard env
        let to_set = vec![
            ("+", builtin_add as BuiltInFnType),
            ("-", builtin_sub as BuiltInFnType),
            ("*", builtin_mul as BuiltInFnType),
            ("/", builtin_div as BuiltInFnType),
            ("abs", builtin_abs as BuiltInFnType),
            ("<", builtin_lt as BuiltInFnType),
            (">", builtin_gt as BuiltInFnType),
            ("<=", builtin_leq as BuiltInFnType),
            (">=", builtin_geq as BuiltInFnType),
            ("=", builtin_iseq as BuiltInFnType),
            ("not", builtin_not as BuiltInFnType),
            ("list", builtin_list as BuiltInFnType),
            ("apply", builtin_apply as BuiltInFnType),
            ("length", builtin_length as BuiltInFnType),
            ("append", builtin_append as BuiltInFnType),
            ("pair?", builtin_ispair as BuiltInFnType),
            ("list?", builtin_islist as BuiltInFnType),
            ("null?", builtin_isnull as BuiltInFnType),
            ("number?", builtin_isnumber as BuiltInFnType),
            ("symbol?", builtin_issymbol as BuiltInFnType),
            ("string?", builtin_isstring as BuiltInFnType),
            ("boolean?", builtin_isboolean as BuiltInFnType),
            ("procedure?", builtin_isprocedure as BuiltInFnType),
            ("even?", builtin_iseven as BuiltInFnType),
            ("odd?", builtin_isodd as BuiltInFnType),
            ("positive?", builtin_ispositive as BuiltInFnType),
            ("negative?", builtin_isnegative as BuiltInFnType),
            ("zero?", builtin_iszero as BuiltInFnType),
            ("cons", builtin_cons as BuiltInFnType),
            ("car", builtin_car as BuiltInFnType),
            ("cdr", builtin_cdr as BuiltInFnType),
            ("filter", builtin_filter as BuiltInFnType),
            ("map", builtin_map as BuiltInFnType),
            ("reverse", builtin_reverse as BuiltInFnType),
            ("read", builtin_read as BuiltInFnType),
            ("write", builtin_write as BuiltInFnType),
            ("newline", builtin_newline as BuiltInFnType),
            ("quotient", builtin_quotient as BuiltInFnType),
            ("remainder", builtin_remainder as BuiltInFnType),
            ("modulo", builtin_modulo as BuiltInFnType),
        ];
        for (s, f) in to_set {
            env.set(
                Intern::new(s.to_string()),
                Value::Procedure(Procedure::BuiltIn(BuiltInProcedure { func: f })),
            );
        }
        env
    }

    #[inline(always)]
    pub fn set(&mut self, key: Intern<String>, value: Value) -> Option<Value> {
        self.head.borrow_mut().set(key, value)
    }

    #[inline(always)]
    pub fn get(&self, key: &Intern<String>) -> Option<Value> {
        self.head.borrow().get(key)
    }

    pub fn evaluate(&mut self, expr: &Value) -> Result<Value, String> {
        self.maybe_evaluate(expr)?.materialize()
    }

    fn maybe_evaluate(&mut self, expr: &Value) -> Result<MaybeValue, String> {
        match expr {
            Value::Integer(_) => Ok(MaybeValue::Just(expr.clone())),
            Value::Float(_) => Ok(MaybeValue::Just(expr.clone())),
            Value::Rational(_, _) => Ok(MaybeValue::Just(expr.clone())),
            Value::Str(_) => Ok(MaybeValue::Just(expr.clone())),
            Value::Bool(_) => Ok(MaybeValue::Just(expr.clone())),
            Value::Cons(p) => self.maybe_evaluate_pair(p),
            Value::Symbol(s) => match self.get(s) {
                Some(value) => Ok(MaybeValue::Just(value)),
                None => Err(format!("Undefined symbol: {}", s)),
            },
            _ => Err("Cannot evaluate expression".to_string()),
        }
    }

    fn maybe_evaluate_pair(&mut self, pair: &Cons) -> Result<MaybeValue, String> {
        let args = pair.cdr.borrow_vec()?;

        match &pair.car {
            Value::Keyword(Keyword::Quote) => Ok(MaybeValue::Just(self.evaluate_quote(args)?)),
            Value::Keyword(Keyword::Quasiquote) => {
                Ok(MaybeValue::Just(self.evaluate_quasiquote(args)?))
            }
            Value::Keyword(Keyword::Lambda) => Ok(MaybeValue::Just(self.evaluate_lambda(args)?)),
            Value::Keyword(Keyword::Define) => Ok(MaybeValue::Just(self.evaluate_define(args)?)),
            Value::Keyword(Keyword::Set) => Ok(MaybeValue::Just(self.evaluate_set(args)?)),
            Value::Keyword(Keyword::If) => self.evaluate_if(args),
            Value::Keyword(Keyword::Cond) => self.evaluate_cond(args),
            Value::Keyword(Keyword::When) => self.evaluate_when(args),
            Value::Keyword(Keyword::Unless) => self.evaluate_unless(args),
            Value::Keyword(Keyword::Let) => self.evaluate_let(args),
            Value::Keyword(Keyword::Begin) => self.evaluate_begin(args),
            Value::Keyword(Keyword::And) => Ok(MaybeValue::Just(self.evaluate_and(args)?)),
            Value::Keyword(Keyword::Or) => Ok(MaybeValue::Just(self.evaluate_or(args)?)),
            car => match self.evaluate(car)? {
                Value::Procedure(proc) => {
                    let mut args_values = Vec::new();
                    for arg in args {
                        args_values.push(self.evaluate(arg)?)
                    }
                    Ok(MaybeValue::TailCall(proc, args_values))
                }
                stuff => Err(format!("Not a procedure call: {}", stuff)),
            },
        }
    }

    fn evaluate_quote(&mut self, args: Vec<&Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!(
                "Quote needs exactly one argument, got {}",
                args.len()
            ));
        }
        Ok(args[0].clone())
    }

    fn do_quasiquote(&mut self, expr: &Value) -> Result<Value, String> {
        if let Value::Cons(c) = expr {
            if let Value::Keyword(Keyword::Unquote) = c.car {
                let Value::Cons(to_eval) = &c.cdr else {
                    return Err("Unquote as part of improper list".to_string());
                };
                let Value::Null = to_eval.cdr else {
                    return Err("Unquote takes a single argument".to_string());
                };
                return self.evaluate(&to_eval.car);
            } else {
                return Ok(Value::Cons(Rc::new(Cons {
                    car: self.do_quasiquote(&c.car)?,
                    cdr: self.do_quasiquote(&c.cdr)?,
                })));
            }
        }
        Ok(expr.clone())
    }

    fn evaluate_quasiquote(&mut self, args: Vec<&Value>) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!(
                "Quasiquote needs exactly one argument, got {}",
                args.len()
            ));
        }
        self.do_quasiquote(args[0])
    }

    fn evaluate_lambda(&mut self, mut args: Vec<&Value>) -> Result<Value, String> {
        if args.is_empty() {
            return Err("Lambda needs at least one argument".to_string());
        }
        match args[0] {
            Value::Cons(_) => {
                let mut params = Vec::new();
                for expr in args[0].borrow_vec()? {
                    match expr {
                        Value::Symbol(s) => params.push(*s),
                        _ => return Err(format!("Not a symbol: {}", expr)),
                    }
                }
                let mut body = Vec::new();
                for expr in args.split_off(1) {
                    body.push(expr.clone())
                }
                let proc = UserDefinedProcedure {
                    params,
                    body,
                    env: self.clone(),
                };
                Ok(Value::Procedure(Procedure::UserDefined(proc)))
            }
            _ => Err("First argument to lambda must be a list of symbols".to_string()),
        }
    }

    fn evaluate_define(&mut self, mut args: Vec<&Value>) -> Result<Value, String> {
        if args.is_empty() {
            return Err("Define needs at least one argument".to_string());
        }
        match args[0] {
            Value::Symbol(key) => {
                let value = match args.len() {
                    1 => Value::Unspecified,
                    2 => self.evaluate(args[1])?,
                    _ => return Err("Define with a symbol gets at most two arguments".to_string()),
                };
                self.set(*key, value);
                Ok(Value::Unspecified)
            }
            Value::Cons(pair) => {
                let key = match &pair.car {
                    Value::Symbol(s) => Ok(s),
                    _ => Err("Not a symbol"),
                }?;
                let mut params = Vec::new();
                for expr in pair.cdr.borrow_vec()? {
                    match expr {
                        Value::Symbol(s) => params.push(*s),
                        _ => return Err("Not a symbol".to_string()),
                    }
                }
                let mut body = Vec::new();
                for expr in args.split_off(1) {
                    body.push(expr.clone())
                }
                let proc = UserDefinedProcedure {
                    params,
                    body,
                    env: self.clone(),
                };
                self.set(*key, Value::Procedure(Procedure::UserDefined(proc)));
                Ok(Value::Unspecified)
            }
            _ => Err("Define needs a symbol or a list as first argument".to_string()),
        }
    }

    fn evaluate_set(&mut self, args: Vec<&Value>) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("Set! needs exactly two arguments".to_string());
        }
        match args[0] {
            Value::Symbol(s) => {
                if self.get(s).is_none() {
                    return Err("Symbol is not bound".to_string());
                }
                let value = self.evaluate(args[1])?;
                self.set(*s, value);
                Ok(Value::Unspecified)
            }
            _ => Err("First argument to set! must be a symbol".to_string()),
        }
    }

    fn evaluate_if(&mut self, args: Vec<&Value>) -> Result<MaybeValue, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("If accepts two or three arguments".to_string());
        }
        match self.evaluate(args[0])? {
            Value::Bool(true) => self.maybe_evaluate(args[1]),
            Value::Bool(false) => {
                if args.len() == 2 {
                    Ok(MaybeValue::Just(Value::Unspecified))
                } else {
                    self.maybe_evaluate(args[2])
                }
            }
            _ => Err("First argument to if did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_cond(&mut self, args: Vec<&Value>) -> Result<MaybeValue, String> {
        for clause in args {
            match clause {
                Value::Cons(p) => {
                    let seq = p.cdr.borrow_vec()?;
                    if let Value::Symbol(s) = &p.car {
                        if **s == "else" {
                            return self.evaluate_begin(seq);
                        }
                    }
                    match self.evaluate(&p.car)? {
                        Value::Bool(true) => return self.evaluate_begin(seq),
                        Value::Bool(false) => continue,
                        _ => return Err("Clause did not evaluate to a boolean".to_string()),
                    }
                }
                _ => return Err("Not a list".to_string()),
            }
        }
        Ok(MaybeValue::Just(Value::Unspecified))
    }

    fn evaluate_when(&mut self, mut args: Vec<&Value>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("When needs at least one argument".to_string());
        }
        match self.evaluate(args[0])? {
            Value::Bool(true) => self.evaluate_begin(args.split_off(1)),
            Value::Bool(false) => Ok(MaybeValue::Just(Value::Unspecified)),
            _ => Err("First argument to when did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_unless(&mut self, mut args: Vec<&Value>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Unless needs at least one argument".to_string());
        }
        match self.evaluate(args[0])? {
            Value::Bool(true) => Ok(MaybeValue::Just(Value::Unspecified)),
            Value::Bool(false) => self.evaluate_begin(args.split_off(1)),
            _ => Err("First argument to unless did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_let(&mut self, mut args: Vec<&Value>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Let needs at least one argument".to_string());
        }
        let mut child = self.child();
        for expr in args[0].borrow_vec()? {
            match expr {
                Value::Cons(p) => match (&p.car, &p.cdr) {
                    (Value::Symbol(s), Value::Cons(cdr)) => {
                        if cdr.cdr != Value::Null {
                            return Err("Not a 2-list".to_string());
                        }
                        let value = child.evaluate(&cdr.car)?;
                        child.set(*s, value);
                    }
                    _ => return Err("Not a symbol".to_string()),
                },
                _ => return Err("Not a 2-list".to_string()),
            }
        }
        let mut out = MaybeValue::Just(Value::Unspecified);
        if let Some((last, rest)) = args.split_off(1).split_last() {
            for expr in rest {
                child.evaluate(expr)?;
            }
            out = child.maybe_evaluate(last)?;
        }
        Ok(out)
    }

    fn evaluate_begin(&mut self, args: Vec<&Value>) -> Result<MaybeValue, String> {
        let mut out = MaybeValue::Just(Value::Unspecified);
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                self.evaluate(expr)?;
            }
            out = self.maybe_evaluate(last)?;
        }
        Ok(out)
    }

    fn evaluate_and(&mut self, args: Vec<&Value>) -> Result<Value, String> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Value::Bool(true)) => continue,
                Ok(Value::Bool(false)) => return Ok(Value::Bool(false)),
                _ => return Err("Cannot \"and\" type".to_string()),
            }
        }
        Ok(Value::Bool(true))
    }

    fn evaluate_or(&mut self, args: Vec<&Value>) -> Result<Value, String> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Value::Bool(true)) => return Ok(Value::Bool(true)),
                Ok(Value::Bool(false)) => continue,
                _ => return Err("Cannot \"or\" type".to_string()),
            }
        }
        Ok(Value::Bool(false))
    }
}

trait Callable {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<Intern<String>>,
    body: Vec<Value>,
    env: Environment,
}

impl UserDefinedProcedure {
    #[inline(always)]
    fn call_except_tail(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        let params = &self.params;
        let body = &self.body;
        let mut env = self.env.child();
        if args.len() != params.len() {
            return Err("Incorrect number of arguments".to_string());
        }
        for (param, arg) in zip(params, args) {
            env.set(*param, arg);
        }
        let mut out = MaybeValue::Just(Value::Unspecified);
        if let Some((last, rest)) = body.split_last() {
            for expr in rest {
                env.evaluate(expr)?;
            }
            out = env.maybe_evaluate(last)?;
        }
        Ok(out)
    }
}

impl Callable for UserDefinedProcedure {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        let mut out = self.call_except_tail(args);
        loop {
            match out? {
                MaybeValue::Just(expr) => return Ok(MaybeValue::Just(expr)),
                MaybeValue::TailCall(Procedure::BuiltIn(proc), args) => return proc.call(args),
                MaybeValue::TailCall(Procedure::UserDefined(proc), args) => {
                    out = proc.call_except_tail(args)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltInProcedure {
    func: BuiltInFnType,
}

impl Callable for BuiltInProcedure {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        match self {
            Procedure::UserDefined(proc) => proc.call(args),
            Procedure::BuiltIn(proc) => proc.call(args),
        }
    }
}

impl fmt::Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Procedure::UserDefined(_) => write!(f, "#[user-defined procedure]"),
            Procedure::BuiltIn(_) => write!(f, "#[built-in procedure]"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn intern_str(s: &str) -> Intern<String> {
        Intern::new(s.to_string())
    }

    fn symbol_from_str(s: &str) -> Value {
        Value::Symbol(intern_str(s))
    }

    #[test]
    fn test_environment() {
        let mut base = Environment::empty();
        base.set(intern_str("a"), Value::Integer(42));

        let mut child = base.child();

        child.set(intern_str("a"), Value::Str("hello".to_string()));
        child.set(intern_str("b"), Value::Str("world".to_string()));

        assert_eq!(base.get(&intern_str("a")), Some(Value::Integer(42)));
        assert_eq!(base.get(&intern_str("b")), None);
        assert_eq!(
            child.get(&intern_str("a")),
            Some(Value::Str("hello".to_string()))
        );
        assert_eq!(
            child.get(&intern_str("b")),
            Some(Value::Str("world".to_string()))
        );
    }

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
            Value::Str("hey, hey".to_string()),
        ];

        assert_eq!(builtin_add(values), Err("Cannot add types".to_string()));
    }

    fn validate(steps: Vec<(&str, Value)>) {
        let mut env = Environment::standard().child();
        for (code, val) in steps {
            let expr = parse(code).unwrap().remove(0).into();
            assert_eq!(
                env.evaluate(&expr),
                Ok(val.clone()),
                "we are testing that {} gives {}",
                code,
                val
            );
        }
    }

    #[test]
    fn test_evaluate() {
        let steps = vec![
            ("13", Value::Integer(13)),
            ("-25", Value::Integer(-25)),
            ("42.42", Value::Float(42.42)),
            ("-12.34", Value::Float(-12.34)),
            ("5/4", Value::Rational(5, 4)),
            ("-7/3", Value::Rational(-7, 3)),
            ("6/4", Value::Rational(3, 2)),
            ("-6/4", Value::Rational(-3, 2)),
            ("15/3", Value::Integer(5)),
            ("-15/3", Value::Integer(-5)),
            ("#t", Value::Bool(true)),
            ("#f", Value::Bool(false)),
            ("\"hello, world!\"", Value::Str("hello, world!".to_string())),
            ("(define a 42)", Value::Unspecified),
            ("a", Value::Integer(42)),
            ("(+ 3 2)", Value::Integer(5)),
            ("(* 3 2)", Value::Integer(6)),
            ("(+ 3 2.0)", Value::Float(5.0)),
            ("(* 3.0 2)", Value::Float(6.0)),
            ("(- 10 2 3)", Value::Integer(5)),
            ("(/ 24 3 2)", Value::Float(4.0)),
            ("(+ 3/4 7/3)", Value::Rational(37, 12)),
            ("(- 3/4 7/3)", Value::Rational(-19, 12)),
            ("(* 3/4 7/3)", Value::Rational(7, 4)),
            ("(/ 3/4 7/3)", Value::Rational(9, 28)),
            ("(< 3/4 3/2)", Value::Bool(true)),
            ("(> 9/2 9/3)", Value::Bool(true)),
            ("(< 5/4 5/6)", Value::Bool(false)),
            ("(> 9/19 9/18)", Value::Bool(false)),
            ("(= 3/5 6/10)", Value::Bool(true)),
            ("(= 16/5 33/10)", Value::Bool(false)),
            ("(+ 3/4 2)", Value::Rational(11, 4)),
            ("(- 3/4 2)", Value::Rational(-5, 4)),
            ("(* 3/4 2)", Value::Rational(3, 2)),
            ("(/ 3/4 2)", Value::Rational(3, 8)),
            ("(< 3/4 1)", Value::Bool(true)),
            ("(> 9/2 4)", Value::Bool(true)),
            ("(< 5/4 1)", Value::Bool(false)),
            ("(> 9/19 4)", Value::Bool(false)),
            ("(= 16/4 4)", Value::Bool(true)),
            ("(= 16/5 4)", Value::Bool(false)),
            ("(< 3/4 0.8)", Value::Bool(true)),
            ("(> 9/2 3.4)", Value::Bool(true)),
            ("(< 5/4 1.2)", Value::Bool(false)),
            ("(> 9/19 0.5)", Value::Bool(false)),
            ("(= 16/4 4.0)", Value::Bool(true)),
            ("(= 16/5 3.9)", Value::Bool(false)),
            ("(abs -5)", Value::Integer(5)),
            ("(abs 5)", Value::Integer(5)),
            ("(abs -5.0)", Value::Float(5.0)),
            ("(abs 5.0)", Value::Float(5.0)),
            ("(< 1 2 3)", Value::Bool(true)),
            ("(< 1 3 2)", Value::Bool(false)),
            ("(<= 1 1 1)", Value::Bool(true)),
            ("(<= 1 0 1)", Value::Bool(false)),
            ("(> 3 2 1)", Value::Bool(true)),
            ("(> 1 3 2)", Value::Bool(false)),
            ("(>= 1 1 1)", Value::Bool(true)),
            ("(>= 1 1 2)", Value::Bool(false)),
            ("(= -1 -1 -1)", Value::Bool(true)),
            ("(= -1 -1 -2)", Value::Bool(false)),
            (
                "(cons 1 2)",
                Value::Cons(Rc::new(Cons {
                    car: Value::Integer(1),
                    cdr: Value::Integer(2),
                })),
            ),
            (
                "'(1 . 2)",
                Value::Cons(Rc::new(Cons {
                    car: Value::Integer(1),
                    cdr: Value::Integer(2),
                })),
            ),
            (
                "'(1 2 . 3)",
                Value::Cons(Rc::new(Cons {
                    car: Value::Integer(1),
                    cdr: Value::Cons(Rc::new(Cons {
                        car: Value::Integer(2),
                        cdr: Value::Integer(3),
                    })),
                })),
            ),
            (
                "'(1 . (2 . 3))",
                Value::Cons(Rc::new(Cons {
                    car: Value::Integer(1),
                    cdr: Value::Cons(Rc::new(Cons {
                        car: Value::Integer(2),
                        cdr: Value::Integer(3),
                    })),
                })),
            ),
            (
                "(list 1 2 3)",
                Value::from_slice(&[Value::Integer(1), Value::Integer(2), Value::Integer(3)]),
            ),
            (
                "'(1 2 3)",
                Value::from_slice(&[Value::Integer(1), Value::Integer(2), Value::Integer(3)]),
            ),
            (
                "'(1 . (2 . (3 . ())))",
                Value::from_slice(&[Value::Integer(1), Value::Integer(2), Value::Integer(3)]),
            ),
            ("(define a 13)", Value::Unspecified),
            ("(+ 8 a)", Value::Integer(21)),
            (
                "(define f (lambda (a b) (+ (* 3 a) b)))",
                Value::Unspecified,
            ),
            ("(f 7 a)", Value::Integer(34)),
            ("(f 7.0 a)", Value::Float(34.0)),
            ("(if (> 3 7) (- 3 7) (- 7 3))", Value::Integer(4)),
            ("(if (< 3 7) (- 3 7) (- 7 3))", Value::Integer(-4)),
            ("(begin (+ 4 7) (- 5 2) (* 7 3))", Value::Integer(21)),
            ("(let ((a 14) (b 7)) (+ a b) (- a b))", Value::Integer(7)),
            ("(length '())", Value::Integer(0)),
            ("(length '(4 5 6))", Value::Integer(3)),
            (
                "(append '(1 2) '(3) '() '(4))",
                Value::from_slice(&[
                    Value::Integer(1),
                    Value::Integer(2),
                    Value::Integer(3),
                    Value::Integer(4),
                ]),
            ),
            ("(set! a -1)", Value::Unspecified),
            ("a", Value::Integer(-1)),
        ];
        validate(steps);
    }

    #[test]
    fn test_and_or_not() {
        let steps = vec![
            ("(and)", Value::Bool(true)),
            ("(and #t #t #f)", Value::Bool(false)),
            ("(and #t #t #t)", Value::Bool(true)),
            ("(or)", Value::Bool(false)),
            ("(or #f #f #f)", Value::Bool(false)),
            ("(or #f #t #f)", Value::Bool(true)),
            ("(not #t)", Value::Bool(false)),
            ("(not #f)", Value::Bool(true)),
            ("(not 3)", Value::Bool(false)),
            ("(not (list 3))", Value::Bool(false)),
            ("(not '())", Value::Bool(false)),
            ("(not 'nil)", Value::Bool(false)),
        ];
        validate(steps);
    }

    #[test]
    fn test_quote() {
        let steps = vec![
            ("(quote ())", Value::from_slice(&[])),
            (
                "(quote (#t #f))",
                Value::from_slice(&[Value::Bool(true), Value::Bool(false)]),
            ),
            ("(quote 42.0)", Value::Float(42.0)),
            (
                "(quote (* 3 4))",
                Value::from_slice(&[symbol_from_str("*"), Value::Integer(3), Value::Integer(4)]),
            ),
            ("'()", Value::from_slice(&[])),
            (
                "'(#t #f)",
                Value::from_slice(&[Value::Bool(true), Value::Bool(false)]),
            ),
            ("'42.0", Value::Float(42.0)),
            (
                "'(* 3 4)",
                Value::from_slice(&[symbol_from_str("*"), Value::Integer(3), Value::Integer(4)]),
            ),
        ];
        validate(steps);
    }

    #[test]
    fn test_predicates() {
        let steps = vec![
            ("(list? '())", Value::Bool(true)),
            ("(list? '(1 2 3))", Value::Bool(true)),
            ("(list? (list 1 2 3))", Value::Bool(true)),
            ("(list? 42)", Value::Bool(false)),
            ("(list? (cons 17 18))", Value::Bool(false)),
            ("(pair? '())", Value::Bool(false)),
            ("(pair? '(1 2 3))", Value::Bool(true)),
            ("(pair? (list 1 2 3))", Value::Bool(true)),
            ("(pair? 42)", Value::Bool(false)),
            ("(pair? (cons 17 18))", Value::Bool(true)),
            ("(null? 0)", Value::Bool(false)),
            ("(null? #f)", Value::Bool(false)),
            ("(null? '())", Value::Bool(true)),
            ("(null? '(1))", Value::Bool(false)),
            ("(number? 42)", Value::Bool(true)),
            ("(number? 42.0)", Value::Bool(true)),
            ("(number? \"hello\")", Value::Bool(false)),
            ("(number? 'a)", Value::Bool(false)),
            ("(number? '())", Value::Bool(false)),
            ("(number? '(1 2 3))", Value::Bool(false)),
            ("(number? #t)", Value::Bool(false)),
            ("(symbol? 42)", Value::Bool(false)),
            ("(symbol? 42.0)", Value::Bool(false)),
            ("(symbol? \"hello\")", Value::Bool(false)),
            ("(symbol? 'a)", Value::Bool(true)),
            ("(symbol? '())", Value::Bool(false)),
            ("(symbol? '(1 2 3))", Value::Bool(false)),
            ("(symbol? #t)", Value::Bool(false)),
            ("(string? \"hello\")", Value::Bool(true)),
            ("(string? 3.14)", Value::Bool(false)),
            ("(string? '())", Value::Bool(false)),
            ("(boolean? #t)", Value::Bool(true)),
            ("(boolean? #f)", Value::Bool(true)),
            ("(boolean? \"hello\")", Value::Bool(false)),
            ("(boolean? 3.14)", Value::Bool(false)),
            ("(boolean? '())", Value::Bool(false)),
            ("(procedure? (lambda (x) (* 2 x)))", Value::Bool(true)),
            ("(procedure? #f)", Value::Bool(false)),
            ("(procedure? \"hello\")", Value::Bool(false)),
            ("(procedure? 3.14)", Value::Bool(false)),
            ("(procedure? '())", Value::Bool(false)),
            ("(even? 2)", Value::Bool(true)),
            ("(even? 3)", Value::Bool(false)),
            ("(even? -2)", Value::Bool(true)),
            ("(even? -3)", Value::Bool(false)),
            ("(odd? 2)", Value::Bool(false)),
            ("(odd? 3)", Value::Bool(true)),
            ("(odd? -2)", Value::Bool(false)),
            ("(odd? -3)", Value::Bool(true)),
            ("(positive? 2)", Value::Bool(true)),
            ("(positive? 2.0)", Value::Bool(true)),
            ("(positive? -2)", Value::Bool(false)),
            ("(positive? -2.0)", Value::Bool(false)),
            ("(negative? 2)", Value::Bool(false)),
            ("(negative? 2.0)", Value::Bool(false)),
            ("(negative? -2)", Value::Bool(true)),
            ("(negative? -2.0)", Value::Bool(true)),
            ("(zero? 0)", Value::Bool(true)),
            ("(zero? 0.0)", Value::Bool(true)),
            ("(zero? -0)", Value::Bool(true)),
            ("(zero? -0.0)", Value::Bool(true)),
            ("(zero? 1)", Value::Bool(false)),
            ("(zero? 0.0001)", Value::Bool(false)),
            ("(zero? -1)", Value::Bool(false)),
            ("(zero? -0.0001)", Value::Bool(false)),
        ];
        validate(steps);
    }

    #[test]
    fn test_apply() {
        let steps = vec![
            ("(apply + '(3 4))", Value::Integer(7)),
            ("(apply * (list -5 4))", Value::Integer(-20)),
        ];
        validate(steps);
    }

    #[test]
    fn test_multistep_function_1() {
        let steps = vec![
            (
                "(define f (lambda (x) (define a 3) (* a x)))",
                Value::Unspecified,
            ),
            ("(f 4)", Value::Integer(12)),
        ];
        validate(steps);
    }

    #[test]
    fn test_multistep_function_2() {
        let steps = vec![
            ("(define (f x) (define a 3) (* a x))", Value::Unspecified),
            ("(f 4)", Value::Integer(12)),
        ];
        validate(steps);
    }

    #[test]
    fn test_factorial() {
        let steps = vec![
            (
                "(define fact
                    (lambda (n) (
                        if (< n 2)
                        1
                        (* n (fact (- n 1))))))",
                Value::Unspecified,
            ),
            ("(fact 11)", Value::Integer(39916800)),
        ];
        validate(steps);
    }

    #[test]
    fn test_sqrt_newton_1() {
        let steps = vec![
            (
                "(define (sqrt x)
                    (define (square x) (* x x))
                    (define (average x y) (/ (+ x y) 2))
                    (define (good-enough? guess)
                        (< (abs (- (square guess) x)) 0.001))
                    (define (improve guess)
                        (average guess (/ x guess)))
                    (define (sqrt-iter guess)
                        (if (good-enough? guess)
                            guess
                            (sqrt-iter (improve guess))))
                    (sqrt-iter 1.0))",
                Value::Unspecified,
            ),
            ("(sqrt 2)", Value::Float(1.4142156862745097)),
        ];
        validate(steps);
    }

    #[test]
    fn test_sqrt_newton_2() {
        let steps = vec![
            ("(define (square x) (* x x))", Value::Unspecified),
            ("(define (average x y) (/ (+ x y) 2))", Value::Unspecified),
            (
                "(define (good-enough? guess x)
                    (< (abs (- (square guess) x)) 0.001))",
                Value::Unspecified,
            ),
            (
                "(define (improve guess x)
                    (average guess (/ x guess)))",
                Value::Unspecified,
            ),
            (
                "(define (sqrt-iter guess x)
                    (if (good-enough? guess x)
                        guess
                        (sqrt-iter (improve guess x) x)))",
                Value::Unspecified,
            ),
            (
                "(define (sqrt x)
                    (sqrt-iter 1.0 x))",
                Value::Unspecified,
            ),
            ("(sqrt 2)", Value::Float(1.4142156862745097)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_let_lambda() {
        let steps = vec![
            (
                "(define f (let ((a 3)) (lambda (x) (* a x))))",
                Value::Unspecified,
            ),
            ("(f 5)", Value::Integer(15)),
            ("(f -4)", Value::Integer(-12)),
            ("(define a 1)", Value::Unspecified),
            ("(f 5)", Value::Integer(15)),
            ("(f -4)", Value::Integer(-12)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_with_unbound() {
        let steps = vec![
            ("(define (f x) (+ a x))", Value::Unspecified),
            ("(define a 3)", Value::Unspecified),
            ("(f 5)", Value::Integer(8)),
            ("(define a -17)", Value::Unspecified),
            ("(f 3)", Value::Integer(-14)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_unspecified() {
        let steps = vec![
            ("(define (g) (define h))", Value::Unspecified),
            ("(define a (g))", Value::Unspecified),
            ("a", Value::Unspecified),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_function_using_closure() {
        let steps = vec![
            ("(define (f a) (lambda (b) (+ a b)))", Value::Unspecified),
            ("(define g1 (f 2))", Value::Unspecified),
            ("(g1 3)", Value::Integer(5)),
            ("(define (g2 c) (g1 c))", Value::Unspecified),
            ("(g2 4)", Value::Integer(6)),
            ("(define (g3 c) ((f 2) c))", Value::Unspecified),
            ("(g3 5)", Value::Integer(7)),
            ("(define g4 (lambda (c) (g1 c)))", Value::Unspecified),
            ("(g4 6)", Value::Integer(8)),
            ("(define g5 (lambda (c) ((f 2) c)))", Value::Unspecified),
            ("(g5 7)", Value::Integer(9)),
        ];
        validate(steps);
    }

    #[test]
    fn test_higher_order() {
        let steps = vec![
            (
                "(define (make-adder n) (lambda (x) (+ x n)))",
                Value::Unspecified,
            ),
            ("((make-adder 3) 7)", Value::Integer(10)),
        ];
        validate(steps);
    }

    #[test]
    fn test_nested_function() {
        let steps = vec![
            (
                "(define (outer-func x) (define (inner-func y) (+ x y)) (inner-func 10))",
                Value::Unspecified,
            ),
            ("(outer-func 5)", Value::Integer(15)),
        ];
        validate(steps);
    }

    #[test]
    fn test_function_scope() {
        let steps = vec![
            (
                "(define (outer x) (define y (+ x 1)) (lambda (z) (+ y z)))",
                Value::Unspecified,
            ),
            ("((outer 3) 2)", Value::Integer(6)),
        ];
        validate(steps);
    }

    #[test]
    fn test_cons_car_cdr() {
        let steps = vec![
            ("(cons 1 '())", Value::from_slice(&[Value::Integer(1)])),
            (
                "(cons 1 '(2 3))",
                Value::from_slice(&[Value::Integer(1), Value::Integer(2), Value::Integer(3)]),
            ),
            ("(car '(1 2 3))", Value::Integer(1)),
            (
                "(cdr '(1 2 3))",
                Value::from_slice(&[Value::Integer(2), Value::Integer(3)]),
            ),
            (
                "(cons 1 2)",
                Value::Cons(Rc::new(Cons {
                    car: Value::Integer(1),
                    cdr: Value::Integer(2),
                })),
            ),
        ];
        validate(steps);
    }

    #[test]
    fn test_cond() {
        let steps = vec![
            (
                "(define (f x) (cond ((< x 0) 'negative) ((> x 0) 'positive) (else 'zero)))",
                Value::Unspecified,
            ),
            ("(f -1)", symbol_from_str("negative")),
            ("(f 0)", symbol_from_str("zero")),
            ("(f 1)", symbol_from_str("positive")),
        ];
        validate(steps);
    }

    #[test]
    fn test_when_unless() {
        let steps = vec![
            ("(define a 42)", Value::Unspecified),
            ("(when (> 0 1) (set! a 43))", Value::Unspecified),
            ("a", Value::Integer(42)),
            ("(when (> 1 0) (set! a 44))", Value::Unspecified),
            ("a", Value::Integer(44)),
            ("(unless (> 0 1) (set! a 43))", Value::Unspecified),
            ("a", Value::Integer(43)),
            ("(unless (> 1 0) (set! a 42))", Value::Unspecified),
            ("a", Value::Integer(43)),
        ];
        validate(steps);
    }

    #[test]
    fn test_filter() {
        let steps = vec![
            (
                "(filter (lambda (x) (< x 3)) '(5 4 3 2 1))",
                Value::from_slice(&[Value::Integer(2), Value::Integer(1)]),
            ),
            (
                "(filter (lambda (x) (< x 0)) '(-5 4 -3 2 -1))",
                Value::from_slice(&[Value::Integer(-5), Value::Integer(-3), Value::Integer(-1)]),
            ),
        ];
        validate(steps);
    }

    #[test]
    fn test_quicksort() {
        let steps = vec![
            (
                "(define (quicksort lst)
                  (if (null? lst)
                      '()
                      (let ((pivot (car lst))
                            (rest (cdr lst)))
                        (append
                          (quicksort (filter (lambda (x) (< x pivot)) rest))
                          (list pivot)
                          (quicksort (filter (lambda (x) (>= x pivot)) rest))))))",
                Value::Unspecified,
            ),
            (
                "(quicksort '(34 7 23 32 5 62 32 2 1 6 45 78 99 3))",
                Value::from_vec(
                    vec![1, 2, 3, 5, 6, 7, 23, 32, 32, 34, 45, 62, 78, 99]
                        .into_iter()
                        .map(Value::Integer)
                        .collect(),
                ),
            ),
        ];
        validate(steps);
    }

    #[test]
    fn test_ackermann() {
        let steps = vec![
            (
                "(define (ackermann m n)
                  (cond
                    ((= m 0) (+ n 1))
                    ((= n 0) (ackermann (- m 1) 1))
                    (else (ackermann (- m 1) (ackermann m (- n 1))))))",
                Value::Unspecified,
            ),
            ("(ackermann 3 3)", Value::Integer(61)),
        ];
        validate(steps);
    }

    #[test]
    fn test_fibonacci_naive() {
        let steps = vec![
            (
                "(define (fib n)
                  (if (< n 2)
                      n
                      (+ (fib (- n 1)) (fib (- n 2)))))",
                Value::Unspecified,
            ),
            ("(fib 13)", Value::Integer(233)),
        ];
        validate(steps);
    }

    #[test]
    fn test_fibonacci_tailcall() {
        let steps = vec![
            (
                "(define (fib n)
                  (define (fib-tail-rec n a b)
                    (if (= n 0)
                        a
                        (fib-tail-rec (- n 1) b (+ a b))))
                  (fib-tail-rec n 0 1))",
                Value::Unspecified,
            ),
            ("(fib 20)", Value::Integer(6765)),
        ];
        validate(steps);
    }
}
