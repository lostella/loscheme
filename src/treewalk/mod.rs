use crate::char::char_to_external;
use crate::parser::{parse, Expr, Keyword};
use crate::rationals::{lcm, simplify};
use crate::utils::read_code;
use internment::Intern;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use std::iter::zip;
use std::rc::Rc;

mod builtin;
mod write;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Str(Rc<String>),
    Char(char),
    Bool(bool),
    Keyword(Keyword),
    Symbol(Intern<String>),
    Pair(Rc<RefCell<(Value, Value)>>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Procedure(Rc<Procedure>),
    #[default]
    Unspecified,
}

fn make_rational(num: i64, denom: i64) -> Value {
    let (num1, denom1) = simplify(num, denom);
    if denom1 == 1 {
        Value::Integer(num1)
    } else {
        Value::Rational(num1, denom1)
    }
}

impl From<Expr> for Value {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Bool(x) => Value::Bool(x),
            Expr::Integer(x) => Value::Integer(x),
            Expr::Float(x) => Value::Float(x),
            Expr::Rational(x, y) => make_rational(x, y),
            Expr::Str(x) => Value::Str(x.into()),
            Expr::Char(c) => Value::Char(c),
            Expr::Keyword(x) => Value::Keyword(x),
            Expr::Symbol(x) => Value::Symbol(x),
            Expr::List(v) => {
                let Some((first, rest)) = v.split_first() else {
                    return Value::Null;
                };
                match first {
                    Expr::Keyword(Keyword::Dot) => {
                        if let Some(last) = rest.first() {
                            Value::from(last.clone())
                        } else {
                            Value::Null
                        }
                    }
                    _ => Value::Pair(Rc::new(RefCell::new((
                        Value::from(first.clone()),
                        Value::from(Expr::List(rest.to_vec())),
                    )))),
                }
            }
            Expr::Vector(v) => Value::Vector(Rc::new(
                v.into_iter()
                    .map(std::convert::Into::into)
                    .collect::<Vec<Value>>()
                    .into(),
            )),
        }
    }
}

fn intern_str(s: &str) -> Intern<String> {
    Intern::new(s.to_string())
}

fn symbol_from_str(s: &str) -> Value {
    Value::Symbol(intern_str(s))
}

impl Value {
    pub fn from_slice(v: &[Value]) -> Self {
        match v.len() {
            0 => Value::Null,
            _ => Value::Pair(Rc::new(RefCell::new((
                v[0].clone(),
                Value::from_slice(&v[1..]),
            )))),
        }
    }

    pub fn into_vec(self) -> Result<Vec<Value>, String> {
        let mut res = Vec::new();
        let mut cur = self;
        loop {
            match cur {
                Value::Null => return Ok(res),
                Value::Pair(p) => {
                    let borrowed = p.borrow();
                    res.push(borrowed.0.clone());
                    cur = borrowed.1.clone();
                }
                _ => return Err(format!("Not a proper list {cur}")),
            }
        }
    }

    pub fn car(self) -> Result<Value, String> {
        match self {
            Value::Pair(p) => Ok(p.borrow().0.clone()),
            v => Err(format!("Car needs a pair as argument, got {v}")),
        }
    }

    pub fn cdr(self) -> Result<Value, String> {
        match self {
            Value::Pair(p) => Ok(p.borrow().1.clone()),
            v => Err(format!("Cdr needs a pair as argument, got {v}")),
        }
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
            (Value::Integer(a), Value::Integer(b)) => Ok(make_rational(*a, *b)),
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
            Value::Integer(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Str(v) => write!(f, "\"{v}\""),
            Value::Char(c) => write!(f, "#\\{}", char_to_external(*c)),
            Value::Keyword(k) => write!(f, "{k}"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Pair(_) => {
                write!(f, "(")?;
                let mut current = self.clone();
                let mut first = true;

                loop {
                    match current {
                        Value::Pair(pair_rc) => {
                            let pair = pair_rc.borrow();
                            if !first {
                                write!(f, " ")?;
                            }
                            write!(f, "{}", pair.0)?;
                            current = pair.1.clone();
                            first = false;
                        }
                        Value::Null => {
                            write!(f, ")")?;
                            break;
                        }
                        other => {
                            write!(f, " . {other})")?;
                            break;
                        }
                    }
                }
                Ok(())
            }
            Value::Vector(v) => {
                write!(f, "#(")?;
                let mut first = true;
                for el in v.borrow().iter() {
                    if !first {
                        write!(f, " ")?;
                    }
                    write!(f, "{el}")?;
                    first = false;
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Procedure(proc) => write!(f, "{proc}"),
            Value::Unspecified => Ok(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MaybeValue {
    Just(Value),
    TailCall(Rc<Procedure>, Vec<Value>),
}

impl MaybeValue {
    fn materialize(self) -> Result<Value, String> {
        match self {
            Self::Just(value) => Ok(value),
            Self::TailCall(proc, args) => proc.call(args)?.materialize(),
        }
    }
}

impl From<Value> for MaybeValue {
    fn from(value: Value) -> Self {
        Self::Just(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentNode {
    data: FxHashMap<Intern<String>, Rc<RefCell<Value>>>,
    parent: Option<Rc<RefCell<EnvironmentNode>>>,
}

impl EnvironmentNode {
    pub fn set(&mut self, key: Intern<String>, value: Value) {
        self.data.insert(key, Rc::new(RefCell::new(value)));
    }

    #[must_use]
    pub fn get(&self, key: &Intern<String>) -> Option<Rc<RefCell<Value>>> {
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
    head: Rc<RefCell<EnvironmentNode>>,
}

type ArgSpec = (Vec<Intern<String>>, Option<Intern<String>>);

impl Environment {
    #[must_use]
    pub fn empty() -> Self {
        let node = EnvironmentNode {
            data: FxHashMap::default(),
            parent: None,
        };
        Self {
            head: Rc::new(RefCell::new(node)),
        }
    }

    #[must_use]
    pub fn child(&self) -> Self {
        let node = EnvironmentNode {
            data: FxHashMap::default(),
            parent: Some(self.head.clone()),
        };
        Self {
            head: Rc::new(RefCell::new(node)),
        }
    }

    #[must_use]
    pub fn standard() -> Self {
        let mut env = Self::empty();
        env.import_bindings(&builtin::BUILTIN_BINDINGS);
        env
    }

    #[must_use]
    pub fn replenv() -> Self {
        let mut env = Self::empty();
        env.import_bindings(&builtin::BUILTIN_BINDINGS);
        env.import_bindings(&write::EXPORTED_BINDINGS);
        env
    }

    pub fn set(&mut self, key: Intern<String>, value: Value) {
        self.head.borrow_mut().set(key, value);
    }

    #[must_use]
    pub fn get(&self, key: &Intern<String>) -> Option<Rc<RefCell<Value>>> {
        self.head.borrow().get(key)
    }

    pub fn evaluate(&mut self, expr: Value) -> Result<Value, String> {
        self.maybe_evaluate(expr)?.materialize()
    }

    fn maybe_evaluate(&mut self, expr: Value) -> Result<MaybeValue, String> {
        match expr {
            Value::Integer(_)
            | Value::Float(_)
            | Value::Rational(_, _)
            | Value::Str(_)
            | Value::Char(_)
            | Value::Bool(_)
            | Value::Vector(_) => Ok(MaybeValue::Just(expr)),
            Value::Pair(rc) => self.maybe_evaluate_pair(&rc.borrow()),
            Value::Symbol(s) => match self.get(&s) {
                Some(rc) => Ok(MaybeValue::Just(rc.borrow().clone())),
                None => Err(format!("Undefined symbol: {s}")),
            },
            _ => Err("Cannot evaluate expression".to_string()),
        }
    }

    fn maybe_evaluate_pair(&mut self, pair: &(Value, Value)) -> Result<MaybeValue, String> {
        let args = pair.1.clone().into_vec()?;

        match pair.0 {
            Value::Keyword(Keyword::Include) => Ok(MaybeValue::Just(self.evaluate_include(&args)?)),
            Value::Keyword(Keyword::Import) => Ok(MaybeValue::Just(self.evaluate_import(&args)?)),
            Value::Keyword(Keyword::Quote) => Ok(MaybeValue::Just(self.evaluate_quote(&args)?)),
            Value::Keyword(Keyword::Quasiquote) => {
                Ok(MaybeValue::Just(self.evaluate_quasiquote(&args)?))
            }
            Value::Keyword(Keyword::Lambda) => Ok(MaybeValue::Just(self.evaluate_lambda(&args)?)),
            Value::Keyword(Keyword::Define) => Ok(MaybeValue::Just(self.evaluate_define(&args)?)),
            Value::Keyword(Keyword::Set) => Ok(MaybeValue::Just(self.evaluate_set(&args)?)),
            Value::Keyword(Keyword::If) => self.evaluate_if(&args),
            Value::Keyword(Keyword::Cond) => self.evaluate_cond(&args),
            Value::Keyword(Keyword::Case) => self.evaluate_case(&args),
            Value::Keyword(Keyword::When) => self.evaluate_when(&args),
            Value::Keyword(Keyword::Unless) => self.evaluate_unless(&args),
            Value::Keyword(Keyword::Let) => self.evaluate_let(&args, false),
            Value::Keyword(Keyword::Letstar | Keyword::Letrec) => self.evaluate_let(&args, true),
            Value::Keyword(Keyword::Begin) => self.evaluate_begin(&args),
            Value::Keyword(Keyword::And) => self.evaluate_and(&args),
            Value::Keyword(Keyword::Or) => self.evaluate_or(&args),
            _ => match self.evaluate(pair.0.clone())? {
                Value::Procedure(proc) => {
                    let mut args_values = Vec::new();
                    for arg in args {
                        args_values.push(self.evaluate(arg)?);
                    }
                    Ok(MaybeValue::TailCall(proc.clone(), args_values))
                }
                stuff => Err(format!("Not a procedure call: {stuff}")),
            },
        }
    }

    fn include_file(&mut self, filename: &str) -> Result<Value, String> {
        let code = read_code(filename)?;
        let exprs = parse(&code)?;
        let mut val = Value::Unspecified;

        for expr in exprs {
            val = self.evaluate(Value::from(expr))?;
        }

        Ok(val)
    }

    fn evaluate_include(&mut self, args: &[Value]) -> Result<Value, String> {
        let mut val = Value::Unspecified;

        for arg in args {
            let Value::Str(filename) = arg else {
                return Err("Include only takes strings as arguments".to_string());
            };
            val = self.include_file(filename)?;
        }

        Ok(val)
    }

    fn import_bindings(&mut self, bindings: &[(&str, BuiltInFnType)]) {
        for (s, f) in bindings {
            self.set(
                Intern::new(s.to_string()),
                Value::Procedure(Rc::new(Procedure::BuiltIn(BuiltInProcedure {
                    name: s.to_string(),
                    func: *f,
                }))),
            );
        }
    }

    fn evaluate_import_set(&mut self, import_set: &Value) -> Result<(), String> {
        let v = import_set.clone().into_vec()?;
        if v == [symbol_from_str("scheme"), symbol_from_str("base")] {
            self.import_bindings(&builtin::BUILTIN_BINDINGS);
            return Ok(());
        }
        if v == [symbol_from_str("scheme"), symbol_from_str("write")] {
            self.import_bindings(&write::EXPORTED_BINDINGS);
            return Ok(());
        }
        Err("Usupported import set format".to_string())
    }

    fn evaluate_import(&mut self, args: &[Value]) -> Result<Value, String> {
        for import_set in args {
            self.evaluate_import_set(import_set)?;
        }
        Ok(Value::Unspecified)
    }

    fn evaluate_quote(&self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!(
                "Quote needs exactly one argument, got {}",
                args.len()
            ));
        }
        Ok(args[0].clone())
    }

    fn do_quasiquote(&mut self, expr: Value) -> Result<Value, String> {
        if let Value::Pair(p) = expr {
            if let Value::Keyword(Keyword::Unquote) = p.borrow().0 {
                let Value::Pair(p) = p.borrow().1.clone() else {
                    return Err("Unquote as part of improper list".to_string());
                };
                let Value::Null = p.borrow().1 else {
                    return Err("Unquote takes a single argument".to_string());
                };
                return self.evaluate(p.borrow().0.clone());
            }
            return Ok(Value::Pair(Rc::new(RefCell::new((
                self.do_quasiquote(p.borrow().0.clone())?,
                self.do_quasiquote(p.borrow().1.clone())?,
            )))));
        }
        Ok(expr.clone())
    }

    fn evaluate_quasiquote(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 1 {
            return Err(format!(
                "Quasiquote needs exactly one argument, got {}",
                args.len()
            ));
        }
        self.do_quasiquote(args[0].clone())
    }

    fn split_args(args: &Value) -> Result<ArgSpec, String> {
        let mut pos_args = Vec::new();
        let mut current = args.clone();
        loop {
            match current {
                Value::Pair(pair_rc) => {
                    let pair = pair_rc.borrow();
                    if let Value::Symbol(s) = pair.0 {
                        pos_args.push(s);
                    } else {
                        return Err("Expecting symbols".to_string());
                    }
                    let pair = pair_rc.borrow();
                    current = pair.1.clone();
                }
                Value::Null => return Ok((pos_args, None)),
                Value::Symbol(last) => return Ok((pos_args, Some(last))),
                _ => return Err("Expecting symbols".to_string()),
            }
        }
    }

    fn evaluate_lambda_helper(
        &self,
        lambda_args: &Value,
        lambda_body: &[Value],
    ) -> Result<Value, String> {
        let (pos_args, var_arg) = Self::split_args(lambda_args)?;
        let mut body = Vec::new();
        for expr in lambda_body {
            body.push((*expr).clone());
        }
        let proc = UserDefinedProcedure {
            params: pos_args,
            varparam: var_arg,
            body,
            env: self.clone(),
        };
        Ok(Value::Procedure(Rc::new(Procedure::UserDefined(proc))))
    }

    fn evaluate_lambda(&self, args: &[Value]) -> Result<Value, String> {
        if args.is_empty() {
            return Err("Lambda needs at least one argument".to_string());
        }
        self.evaluate_lambda_helper(&args[0], &args[1..])
    }

    fn evaluate_define(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() < 2 {
            return Err("Define needs at least two arguments".to_string());
        }
        match &args[0] {
            Value::Symbol(key) => {
                let value = match args.len() {
                    1 => Value::Unspecified,
                    2 => self.evaluate(args[1].clone())?,
                    _ => return Err("Define with a symbol gets at most two arguments".to_string()),
                };
                self.set(*key, value);
                Ok(Value::Unspecified)
            }
            Value::Pair(p) => {
                let car = &p.borrow().0;
                let key = match car {
                    Value::Symbol(s) => Ok(s),
                    _ => Err(format!("Not a symbol: {}", car)),
                }?;
                let proc = self.evaluate_lambda_helper(&p.borrow().1, &args[1..])?;
                self.set(*key, proc);
                Ok(Value::Unspecified)
            }
            _ => Err("Define needs a symbol or a list as first argument".to_string()),
        }
    }

    fn evaluate_set(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("Set! needs exactly two arguments".to_string());
        }
        match args.first() {
            Some(Value::Symbol(s)) => {
                let Some(cell) = self.get(s) else {
                    return Err("Symbol is not bound".to_string());
                };
                let val = self.evaluate(args[1].clone())?;
                *cell.borrow_mut() = val;
                Ok(Value::Unspecified)
            }
            _ => Err("First argument to set! must be a symbol".to_string()),
        }
    }

    fn evaluate_if(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("If accepts two or three arguments".to_string());
        }
        match self.evaluate(args[0].clone())? {
            Value::Bool(true) => self.maybe_evaluate(args[1].clone()),
            Value::Bool(false) => {
                if args.len() == 2 {
                    Ok(MaybeValue::Just(Value::Unspecified))
                } else {
                    self.maybe_evaluate(args[2].clone())
                }
            }
            _ => Err("First argument to if did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_cond(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        for clause in args {
            match clause {
                Value::Pair(p) => {
                    let seq = p.borrow().1.clone().into_vec()?;
                    if let Value::Symbol(s) = p.borrow().0 {
                        if *s == "else" {
                            return self.evaluate_begin(&seq);
                        }
                    }
                    let val = self.evaluate(p.borrow().0.clone())?;
                    match val {
                        Value::Bool(true) => return self.evaluate_begin(&seq),
                        Value::Bool(false) => continue,
                        _ => return Err("Clause did not evaluate to a boolean".to_string()),
                    }
                }
                _ => return Err("Not a list".to_string()),
            }
        }
        Ok(MaybeValue::Just(Value::Unspecified))
    }

    fn evaluate_case(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        let val = self.evaluate(args[0].clone())?;
        for clause in &args[1..] {
            match clause {
                Value::Pair(p) => {
                    let seq = p.borrow().1.clone().into_vec()?;
                    if let Value::Symbol(s) = p.borrow().0 {
                        if *s == "else" {
                            return self.evaluate_begin(&seq);
                        }
                    }
                    for datum in p.borrow().0.clone().into_vec()? {
                        if builtin::eqv(&val, &datum) {
                            return self.evaluate_begin(&seq);
                        }
                    }
                }
                _ => return Err(format!("Not a list: {clause}")),
            }
        }
        Ok(MaybeValue::Just(Value::Unspecified))
    }

    fn evaluate_when(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("When needs at least one argument".to_string());
        }
        match self.evaluate(args[0].clone())? {
            Value::Bool(true) => self.evaluate_begin(&args[1..]),
            Value::Bool(false) => Ok(MaybeValue::Just(Value::Unspecified)),
            _ => Err("First argument to when did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_unless(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Unless needs at least one argument".to_string());
        }
        match self.evaluate(args[0].clone())? {
            Value::Bool(true) => Ok(MaybeValue::Just(Value::Unspecified)),
            Value::Bool(false) => self.evaluate_begin(&args[1..]),
            _ => Err("First argument to unless did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_let(&mut self, args: &[Value], star: bool) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Let needs at least one argument".to_string());
        }
        let mut child = self.child();
        for expr in args[0].clone().into_vec()? {
            match expr {
                Value::Pair(p) => {
                    let borrowed = p.borrow();
                    match (borrowed.0.clone(), borrowed.1.clone()) {
                        (Value::Symbol(s), Value::Pair(inner_p)) => {
                            let inner_borrowed = inner_p.borrow();
                            if inner_borrowed.1 != Value::Null {
                                return Err("Not a 2-list".to_string());
                            }
                            let value = if star {
                                child.evaluate(inner_borrowed.0.clone())?
                            } else {
                                self.evaluate(inner_borrowed.0.clone())?
                            };
                            child.set(s, value);
                        }
                        _ => return Err("Not a symbol".to_string()),
                    }
                }
                _ => return Err("Not a 2-list".to_string()),
            }
        }
        if let Some((last, rest)) = args[1..].split_last() {
            for expr in rest {
                child.evaluate(expr.clone())?;
            }
            return child.maybe_evaluate(last.clone());
        }
        Ok(MaybeValue::Just(Value::Unspecified))
    }

    fn evaluate_begin(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                self.evaluate(expr.clone())?;
            }
            return self.maybe_evaluate(last.clone());
        }
        Ok(MaybeValue::Just(Value::Unspecified))
    }

    fn evaluate_and(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                if self.evaluate(expr.clone())? == Value::Bool(false) {
                    return Ok(MaybeValue::Just(Value::Bool(false)));
                }
            }
            return self.maybe_evaluate(last.clone());
        }
        Ok(MaybeValue::Just(Value::Bool(true)))
    }

    fn evaluate_or(&mut self, args: &[Value]) -> Result<MaybeValue, String> {
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                let val = self.evaluate(expr.clone())?;
                if val != Value::Bool(false) {
                    return Ok(MaybeValue::Just(val));
                }
            }
            return self.maybe_evaluate(last.clone());
        }
        Ok(MaybeValue::Just(Value::Bool(false)))
    }
}

trait Callable {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<Intern<String>>,
    varparam: Option<Intern<String>>,
    body: Vec<Value>,
    env: Environment,
}

impl UserDefinedProcedure {
    fn call_except_tail(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        let mut env = self.env.child();
        if args.len() < self.params.len() {
            return Err("Incorrect number of arguments".to_string());
        }
        for (param, arg) in zip(&self.params, args.clone()) {
            env.set(*param, arg);
        }
        if let Some(name) = self.varparam {
            let rest = Value::from_slice(&args[self.params.len()..]);
            env.set(name, rest);
        }
        let mut out = MaybeValue::Just(Value::Unspecified);
        if let Some((last, rest)) = self.body.split_last() {
            for expr in rest {
                env.evaluate(expr.clone())?;
            }
            out = env.maybe_evaluate(last.clone())?;
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
                MaybeValue::TailCall(rc, args) => match rc.as_ref() {
                    Procedure::BuiltIn(proc) => return proc.call(args),
                    Procedure::UserDefined(proc) => out = proc.call_except_tail(args),
                },
            }
        }
    }
}

type BuiltInFnType = fn(Vec<Value>) -> Result<MaybeValue, String>;

#[derive(Debug, Clone)]
pub struct BuiltInProcedure {
    name: String,
    func: BuiltInFnType,
}

impl Callable for BuiltInProcedure {
    fn call(&self, args: Vec<Value>) -> Result<MaybeValue, String> {
        (self.func)(args)
    }
}

impl PartialEq for BuiltInProcedure {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
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
            Procedure::BuiltIn(proc) => write!(f, "#[built-in procedure {}]", proc.name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_environment() {
        let mut base = Environment::empty();
        base.set(intern_str("a"), Value::Integer(42));

        let mut child = base.child();

        child.set(intern_str("a"), Value::Str("hello".to_string().into()));
        child.set(intern_str("b"), Value::Str("world".to_string().into()));

        assert_eq!(
            base.get(&intern_str("a")).unwrap().take(),
            Value::Integer(42)
        );
        assert_eq!(base.get(&intern_str("b")), None);
        assert_eq!(
            child.get(&intern_str("a")).unwrap().take(),
            Value::Str("hello".to_string().into())
        );
        assert_eq!(
            child.get(&intern_str("b")).unwrap().take(),
            Value::Str("world".to_string().into())
        );
    }

    fn validate(steps: Vec<(&str, Value)>) {
        let mut env = Environment::standard().child();
        for (code, out) in steps {
            let val = Value::from(parse(code).unwrap().remove(0));
            assert_eq!(
                env.evaluate(val),
                Ok(out.clone()),
                "we are testing that {} gives {}",
                code,
                out
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
            (
                "\"hello, world!\"",
                Value::Str("hello, world!".to_string().into()),
            ),
            ("(define a 42)", Value::Unspecified),
            ("a", Value::Integer(42)),
            ("(+ 3 2)", Value::Integer(5)),
            ("(* 3 2)", Value::Integer(6)),
            ("(+ 3 2.0)", Value::Float(5.0)),
            ("(* 3.0 2)", Value::Float(6.0)),
            ("(- 10 2 3)", Value::Integer(5)),
            ("(/ 24 3 2)", Value::Integer(4)),
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
                Value::Pair(Rc::new(RefCell::new((
                    Value::Integer(1),
                    Value::Integer(2),
                )))),
            ),
            (
                "'(1 . 2)",
                Value::Pair(Rc::new(RefCell::new((
                    Value::Integer(1),
                    Value::Integer(2),
                )))),
            ),
            (
                "'(1 2 . 3)",
                Value::Pair(Rc::new(RefCell::new((
                    Value::Integer(1),
                    Value::Pair(Rc::new(RefCell::new((
                        Value::Integer(2),
                        Value::Integer(3),
                    )))),
                )))),
            ),
            (
                "'(1 . (2 . 3))",
                Value::Pair(Rc::new(RefCell::new((
                    Value::Integer(1),
                    Value::Pair(Rc::new(RefCell::new((
                        Value::Integer(2),
                        Value::Integer(3),
                    )))),
                )))),
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
            ("(define (g) (define h 0))", Value::Unspecified),
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
                Value::Pair(Rc::new(RefCell::new((
                    Value::Integer(1),
                    Value::Integer(2),
                )))),
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
                Value::from_slice(
                    &[1, 2, 3, 5, 6, 7, 23, 32, 32, 34, 45, 62, 78, 99].map(Value::Integer),
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
