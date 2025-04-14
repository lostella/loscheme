use crate::parser::{parse_expression, Cons, Expr, Keyword, Tokenizer};
use internment::Intern;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use std::io::{self, BufRead};
use std::iter::zip;
use std::mem::take;
use std::rc::Rc;

fn builtin_add(values: Vec<Expr>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(0), |acc, x| acc.add(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_mul(values: Vec<Expr>) -> Result<MaybeValue, String> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(1), |acc, x| acc.mul(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_sub(values: Vec<Expr>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let res = match values_iter.next() {
        None => Expr::Integer(0),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.sub(&x))?,
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_abs(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Abs needs exactly one argument".to_string());
    }
    let res = match values.first() {
        Some(value) => match value {
            Expr::Integer(n) => Expr::Integer(if *n >= 0 { *n } else { -*n }),
            Expr::Float(n) => Expr::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("Abs needs a number argument".to_string()),
        },
        _ => return Err("Abs needs exactly one argument".to_string()),
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_div(values: Vec<Expr>) -> Result<MaybeValue, String> {
    let mut values_iter = values.into_iter();
    let res = match values_iter.next() {
        None => Expr::Integer(1),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.div(&x))?,
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_quotient(values: Vec<Expr>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Expr::Integer(a), Expr::Integer(b)] => Ok(MaybeValue::Just(Expr::Integer(a / b))),
        _ => Err("Quotient needs exactly two arguments".to_string()),
    }
}

fn builtin_remainder(values: Vec<Expr>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Expr::Integer(a), Expr::Integer(b)] => Ok(MaybeValue::Just(Expr::Integer(a % b))),
        _ => Err("Remainder needs exactly two arguments".to_string()),
    }
}

fn builtin_modulo(values: Vec<Expr>) -> Result<MaybeValue, String> {
    match values.as_slice() {
        [Expr::Integer(a), Expr::Integer(b)] => {
            let r = a % b;
            Ok(MaybeValue::Just(Expr::Integer(
                if a.signum() != b.signum() { r + b } else { r },
            )))
        }
        _ => Err("Modulo needs exactly two integers".to_string()),
    }
}

type CmpFnType = fn(&Expr, &Expr) -> Result<Expr, String>;

fn builtin_cmp(values: Vec<Expr>, method: CmpFnType) -> Result<MaybeValue, String> {
    for (a, b) in values.iter().zip(values.iter().skip(1)) {
        match method(a, b) {
            Ok(Expr::Bool(false)) => return Ok(MaybeValue::Just(Expr::Bool(false))),
            Err(s) => return Err(s),
            _ => (),
        }
    }
    Ok(MaybeValue::Just(Expr::Bool(true)))
}

fn builtin_lt(values: Vec<Expr>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Expr::lt)
}

fn builtin_gt(values: Vec<Expr>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Expr::gt)
}

fn builtin_leq(values: Vec<Expr>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Expr::leq)
}

fn builtin_geq(values: Vec<Expr>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Expr::geq)
}

fn builtin_iseq(values: Vec<Expr>) -> Result<MaybeValue, String> {
    builtin_cmp(values, Expr::iseq)
}

fn builtin_not(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Not needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(match values[0] {
        Expr::Bool(b) => !b,
        _ => false,
    })))
}

fn builtin_list(values: Vec<Expr>) -> Result<MaybeValue, String> {
    Ok(MaybeValue::Just(Expr::from_vec(values)))
}

fn builtin_apply(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Apply needs exactly two arguments".to_string());
    }
    let first = take(&mut values[0]);
    let second = take(&mut values[1]);
    match first {
        Expr::Procedure(proc) => Ok(MaybeValue::TailCall(proc, second.into_vec()?)),
        _ => Err("Apply needs a procedure and a list as arguments".to_string()),
    }
}

fn builtin_length(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument".to_string());
    }
    match take(&mut values[0]).into_vec() {
        Ok(v) => Ok(MaybeValue::Just(Expr::Integer(v.len() as i64))),
        _ => Err("Cannot compute length (is it a list?)".to_string()),
    }
}

fn builtin_append(values: Vec<Expr>) -> Result<MaybeValue, String> {
    let mut all = Vec::new();
    for value in values {
        match value {
            Expr::Cons(_) => all.extend(value.into_vec()?),
            Expr::Null => (),
            _ => return Err("Append needs lists as arguments".to_string()),
        }
    }
    Ok(MaybeValue::Just(Expr::from_vec(all)))
}

fn builtin_ispair(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Cons(_)
    ))))
}

fn builtin_islist(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Expr::Cons(pair) => match builtin_islist(vec![pair.cdr])? {
            MaybeValue::Just(Expr::Bool(b)) => Ok(MaybeValue::Just(Expr::Bool(b))),
            _ => Err("Unexpected non-boolean".to_string()),
        },
        Expr::Null => Ok(MaybeValue::Just(Expr::Bool(true))),
        _ => Ok(MaybeValue::Just(Expr::Bool(false))),
    }
}

fn builtin_isnull(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Null
    ))))
}

fn builtin_isnumber(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Float(_) | Expr::Integer(_)
    ))))
}

fn builtin_issymbol(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Symbol(_)
    ))))
}

fn builtin_isstring(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("String? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Str(_)
    ))))
}

fn builtin_isboolean(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Boolean? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Bool(_)
    ))))
}

fn builtin_isprocedure(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Procedure? needs exactly one argument".to_string());
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Procedure(_)
    ))))
}

fn builtin_iseven(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Even? needs exactly one argument".to_string());
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(v % 2 == 0))),
        _ => Err("Even? needs an integer argument".to_string()),
    }
}

fn builtin_isodd(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Odd? needs exactly one argument".to_string());
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(v % 2 != 0))),
        _ => Err("Odd? needs an integer argument".to_string()),
    }
}

fn builtin_ispositive(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Positive? needs exactly one argument".to_string());
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v > 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v > 0 as f64))),
        _ => Err("Positive? needs a number argument".to_string()),
    }
}

fn builtin_isnegative(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Negative? needs exactly one argument".to_string());
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v < 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v < 0 as f64))),
        _ => Err("Negative? needs a number argument".to_string()),
    }
}

fn builtin_iszero(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Zero? needs exactly one argument".to_string());
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v == 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v == 0 as f64))),
        _ => Err("Zero? needs a number argument".to_string()),
    }
}

fn builtin_cons(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Cons needs exactly two arguments".to_string());
    }
    let car = take(&mut values[0]);
    let cdr = take(&mut values[1]);
    Ok(MaybeValue::Just(Expr::Cons(Box::new(Cons { car, cdr }))))
}

fn builtin_car(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Expr::Cons(p) => Ok(MaybeValue::Just(p.car)),
        _ => Err("Car needs a pair as argument".to_string()),
    }
}

fn builtin_cdr(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument".to_string());
    }
    match take(&mut values[0]) {
        Expr::Cons(p) => Ok(MaybeValue::Just(p.cdr)),
        _ => Err("Cdr needs a pair as argument".to_string()),
    }
}

fn builtin_filter(mut values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 2 {
        return Err("Filter needs exactly two arguments".to_string());
    }
    let pred = take(&mut values[0]);
    let orig = take(&mut values[1]).into_vec()?;
    match pred {
        Expr::Procedure(proc) => {
            let mut v = Vec::new();
            for x in orig {
                if proc.call(vec![x.clone()])?.materialize()? == Expr::Bool(true) {
                    v.push(x)
                }
            }
            Ok(MaybeValue::Just(Expr::from_vec(v)))
        }
        _ => Err("Not a procedure".to_string()),
    }
}

fn builtin_read(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Read takes no arguments".to_string());
    }
    let mut input = String::new();
    let _ = io::stdin().lock().read_line(&mut input);
    loop {
        let mut tokens = Tokenizer::new(&input).peekable();
        if let Ok(expr) = parse_expression(&mut tokens) {
            return Ok(MaybeValue::Just(expr));
        }
        let _ = io::stdin().lock().read_line(&mut input);
    }
}

fn builtin_write(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument".to_string());
    }
    print!("{}", values[0]);
    Ok(MaybeValue::Just(Expr::Unspecified))
}

fn builtin_newline(values: Vec<Expr>) -> Result<MaybeValue, String> {
    if !values.is_empty() {
        return Err("Write takes no arguments".to_string());
    }
    println!();
    Ok(MaybeValue::Just(Expr::Unspecified))
}

#[derive(Debug, PartialEq, Clone)]
enum MaybeValue {
    Just(Expr),
    TailCall(Procedure, Vec<Expr>),
}

impl MaybeValue {
    fn materialize(self) -> Result<Expr, String> {
        match self {
            Self::Just(expr) => Ok(expr),
            Self::TailCall(proc, args) => proc.call(args)?.materialize(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentNode {
    data: FxHashMap<Intern<String>, Expr>,
    parent: Option<EnvironmentLink>,
}

type EnvironmentLink = Rc<RefCell<EnvironmentNode>>;

impl EnvironmentNode {
    #[inline(always)]
    pub fn set(&mut self, key: Intern<String>, value: Expr) -> Option<Expr> {
        self.data.insert(key, value)
    }

    #[inline(always)]
    pub fn get(&self, key: &Intern<String>) -> Option<Expr> {
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

type BuiltInFnType = fn(Vec<Expr>) -> Result<MaybeValue, String>;

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
                Expr::Procedure(Procedure::BuiltIn(BuiltInProcedure { func: f })),
            );
        }
        env
    }

    #[inline(always)]
    pub fn set(&mut self, key: Intern<String>, value: Expr) -> Option<Expr> {
        self.head.borrow_mut().set(key, value)
    }

    #[inline(always)]
    pub fn get(&self, key: &Intern<String>) -> Option<Expr> {
        self.head.borrow().get(key)
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Expr, String> {
        self.maybe_evaluate(expr)?.materialize()
    }

    fn maybe_evaluate(&mut self, expr: &Expr) -> Result<MaybeValue, String> {
        match expr {
            Expr::Integer(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Float(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Rational(_, _) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Str(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Bool(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Cons(p) => self.maybe_evaluate_pair(p),
            Expr::Symbol(s) => match self.get(s) {
                Some(value) => Ok(MaybeValue::Just(value)),
                None => Err(format!("Undefined symbol: {}", s)),
            },
            _ => Err("Cannot evaluate expression".to_string()),
        }
    }

    fn maybe_evaluate_pair(&mut self, pair: &Cons) -> Result<MaybeValue, String> {
        let args = pair.cdr.borrow_vec()?;

        match &pair.car {
            Expr::Keyword(Keyword::Quote) => Ok(MaybeValue::Just(self.evaluate_quote(args)?)),
            Expr::Keyword(Keyword::Lambda) => Ok(MaybeValue::Just(self.evaluate_lambda(args)?)),
            Expr::Keyword(Keyword::Define) => Ok(MaybeValue::Just(self.evaluate_define(args)?)),
            Expr::Keyword(Keyword::Set) => Ok(MaybeValue::Just(self.evaluate_set(args)?)),
            Expr::Keyword(Keyword::If) => self.evaluate_if(args),
            Expr::Keyword(Keyword::Cond) => self.evaluate_cond(args),
            Expr::Keyword(Keyword::When) => self.evaluate_when(args),
            Expr::Keyword(Keyword::Unless) => self.evaluate_unless(args),
            Expr::Keyword(Keyword::Let) => self.evaluate_let(args),
            Expr::Keyword(Keyword::Begin) => self.evaluate_begin(args),
            Expr::Keyword(Keyword::And) => Ok(MaybeValue::Just(self.evaluate_and(args)?)),
            Expr::Keyword(Keyword::Or) => Ok(MaybeValue::Just(self.evaluate_or(args)?)),
            car => match self.evaluate(car)? {
                Expr::Procedure(proc) => {
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

    fn evaluate_quote(&mut self, args: Vec<&Expr>) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!(
                "Quote needs exactly one argument, got {}",
                args.len()
            ));
        }
        Ok(args[0].clone())
    }

    fn evaluate_lambda(&mut self, mut args: Vec<&Expr>) -> Result<Expr, String> {
        if args.is_empty() {
            return Err("Lambda needs at least one argument".to_string());
        }
        match args[0] {
            Expr::Cons(_) => {
                let mut params = Vec::new();
                for expr in args[0].borrow_vec()? {
                    match expr {
                        Expr::Symbol(s) => params.push(*s),
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
                Ok(Expr::Procedure(Procedure::UserDefined(proc)))
            }
            _ => Err("First argument to lambda must be a list of symbols".to_string()),
        }
    }

    fn evaluate_define(&mut self, mut args: Vec<&Expr>) -> Result<Expr, String> {
        if args.is_empty() {
            return Err("Define needs at least one argument".to_string());
        }
        match args[0] {
            Expr::Symbol(key) => {
                let value = match args.len() {
                    1 => Expr::Unspecified,
                    2 => self.evaluate(args[1])?,
                    _ => return Err("Define with a symbol gets at most two arguments".to_string()),
                };
                self.set(*key, value);
                Ok(Expr::Unspecified)
            }
            Expr::Cons(pair) => {
                let key = match &pair.car {
                    Expr::Symbol(s) => Ok(s),
                    _ => Err("Not a symbol"),
                }?;
                let mut params = Vec::new();
                for expr in pair.cdr.borrow_vec()? {
                    match expr {
                        Expr::Symbol(s) => params.push(*s),
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
                self.set(*key, Expr::Procedure(Procedure::UserDefined(proc)));
                Ok(Expr::Unspecified)
            }
            _ => Err("Define needs a symbol or a list as first argument".to_string()),
        }
    }

    fn evaluate_set(&mut self, args: Vec<&Expr>) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("Set! needs exactly two arguments".to_string());
        }
        match args[0] {
            Expr::Symbol(s) => {
                if self.get(s).is_none() {
                    return Err("Symbol is not bound".to_string());
                }
                let value = self.evaluate(args[1])?;
                self.set(*s, value);
                Ok(Expr::Unspecified)
            }
            _ => Err("First argument to set! must be a symbol".to_string()),
        }
    }

    fn evaluate_if(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("If accepts two or three arguments".to_string());
        }
        match self.evaluate(args[0])? {
            Expr::Bool(true) => self.maybe_evaluate(args[1]),
            Expr::Bool(false) => {
                if args.len() == 2 {
                    Ok(MaybeValue::Just(Expr::Unspecified))
                } else {
                    self.maybe_evaluate(args[2])
                }
            }
            _ => Err("First argument to if did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_cond(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, String> {
        for clause in args {
            match clause {
                Expr::Cons(p) => {
                    let seq = p.cdr.borrow_vec()?;
                    if let Expr::Symbol(s) = &p.car {
                        if **s == "else" {
                            return self.evaluate_begin(seq);
                        }
                    }
                    match self.evaluate(&p.car)? {
                        Expr::Bool(true) => return self.evaluate_begin(seq),
                        Expr::Bool(false) => continue,
                        _ => return Err("Clause did not evaluate to a boolean".to_string()),
                    }
                }
                _ => return Err("Not a list".to_string()),
            }
        }
        Ok(MaybeValue::Just(Expr::Unspecified))
    }

    fn evaluate_when(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("When needs at least one argument".to_string());
        }
        match self.evaluate(args[0])? {
            Expr::Bool(true) => self.evaluate_begin(args.split_off(1)),
            Expr::Bool(false) => Ok(MaybeValue::Just(Expr::Unspecified)),
            _ => Err("First argument to when did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_unless(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Unless needs at least one argument".to_string());
        }
        match self.evaluate(args[0])? {
            Expr::Bool(true) => Ok(MaybeValue::Just(Expr::Unspecified)),
            Expr::Bool(false) => self.evaluate_begin(args.split_off(1)),
            _ => Err("First argument to unless did not evaluate to a boolean".to_string()),
        }
    }

    fn evaluate_let(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, String> {
        if args.is_empty() {
            return Err("Let needs at least one argument".to_string());
        }
        let mut child = self.child();
        for expr in args[0].borrow_vec()? {
            match expr {
                Expr::Cons(p) => match (&p.car, &p.cdr) {
                    (Expr::Symbol(s), Expr::Cons(cdr)) => {
                        if cdr.cdr != Expr::Null {
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
        let mut out = MaybeValue::Just(Expr::Unspecified);
        if let Some((last, rest)) = args.split_off(1).split_last() {
            for expr in rest {
                child.evaluate(expr)?;
            }
            out = child.maybe_evaluate(last)?;
        }
        Ok(out)
    }

    fn evaluate_begin(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, String> {
        let mut out = MaybeValue::Just(Expr::Unspecified);
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                self.evaluate(expr)?;
            }
            out = self.maybe_evaluate(last)?;
        }
        Ok(out)
    }

    fn evaluate_and(&mut self, args: Vec<&Expr>) -> Result<Expr, String> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Expr::Bool(true)) => continue,
                Ok(Expr::Bool(false)) => return Ok(Expr::Bool(false)),
                _ => return Err("Cannot \"and\" type".to_string()),
            }
        }
        Ok(Expr::Bool(true))
    }

    fn evaluate_or(&mut self, args: Vec<&Expr>) -> Result<Expr, String> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Expr::Bool(true)) => return Ok(Expr::Bool(true)),
                Ok(Expr::Bool(false)) => continue,
                _ => return Err("Cannot \"or\" type".to_string()),
            }
        }
        Ok(Expr::Bool(false))
    }
}

trait Callable {
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, String>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<Intern<String>>,
    body: Vec<Expr>,
    env: Environment,
}

impl UserDefinedProcedure {
    #[inline(always)]
    fn call_except_tail(&self, args: Vec<Expr>) -> Result<MaybeValue, String> {
        let params = &self.params;
        let body = &self.body;
        let mut env = self.env.child();
        if args.len() != params.len() {
            return Err("Incorrect number of arguments".to_string());
        }
        for (param, arg) in zip(params, args) {
            env.set(*param, arg);
        }
        let mut out = MaybeValue::Just(Expr::Unspecified);
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
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, String> {
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
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, String> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, String> {
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

    fn symbol_from_str(s: &str) -> Expr {
        Expr::Symbol(intern_str(s))
    }

    #[test]
    fn test_environment() {
        let mut base = Environment::empty();
        base.set(intern_str("a"), Expr::Integer(42));

        let mut child = base.child();

        child.set(intern_str("a"), Expr::Str("hello".to_string()));
        child.set(intern_str("b"), Expr::Str("world".to_string()));

        assert_eq!(base.get(&intern_str("a")), Some(Expr::Integer(42)));
        assert_eq!(base.get(&intern_str("b")), None);
        assert_eq!(
            child.get(&intern_str("a")),
            Some(Expr::Str("hello".to_string()))
        );
        assert_eq!(
            child.get(&intern_str("b")),
            Some(Expr::Str("world".to_string()))
        );
    }

    #[test]
    fn test_builtin_add() {
        let values = vec![Expr::Integer(10), Expr::Float(42.0)];

        assert_eq!(builtin_add(values), Ok(MaybeValue::Just(Expr::Float(52.0))));

        let values = vec![Expr::Float(42.0), Expr::Integer(13)];

        assert_eq!(builtin_add(values), Ok(MaybeValue::Just(Expr::Float(55.0))));

        let values = vec![
            Expr::Float(42.0),
            Expr::Integer(13),
            Expr::Str("hey, hey".to_string()),
        ];

        assert_eq!(builtin_add(values), Err("Cannot add types".to_string()));
    }

    fn validate(steps: Vec<(&str, Expr)>) {
        let mut env = Environment::standard().child();
        for (code, val) in steps {
            let expr = parse(code).unwrap().remove(0);
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
            ("13", Expr::Integer(13)),
            ("-25", Expr::Integer(-25)),
            ("42.42", Expr::Float(42.42)),
            ("-12.34", Expr::Float(-12.34)),
            ("5/4", Expr::Rational(5, 4)),
            ("-7/3", Expr::Rational(-7, 3)),
            ("6/4", Expr::Rational(3, 2)),
            ("-6/4", Expr::Rational(-3, 2)),
            ("15/3", Expr::Integer(5)),
            ("-15/3", Expr::Integer(-5)),
            ("#t", Expr::Bool(true)),
            ("#f", Expr::Bool(false)),
            ("\"hello, world!\"", Expr::Str("hello, world!".to_string())),
            ("(define a 42)", Expr::Unspecified),
            ("a", Expr::Integer(42)),
            ("(+ 3 2)", Expr::Integer(5)),
            ("(* 3 2)", Expr::Integer(6)),
            ("(+ 3 2.0)", Expr::Float(5.0)),
            ("(* 3.0 2)", Expr::Float(6.0)),
            ("(- 10 2 3)", Expr::Integer(5)),
            ("(/ 24 3 2)", Expr::Float(4.0)),
            ("(+ 3/4 7/3)", Expr::Rational(37, 12)),
            ("(- 3/4 7/3)", Expr::Rational(-19, 12)),
            ("(* 3/4 7/3)", Expr::Rational(7, 4)),
            ("(/ 3/4 7/3)", Expr::Rational(9, 28)),
            ("(< 3/4 3/2)", Expr::Bool(true)),
            ("(> 9/2 9/3)", Expr::Bool(true)),
            ("(< 5/4 5/6)", Expr::Bool(false)),
            ("(> 9/19 9/18)", Expr::Bool(false)),
            ("(= 3/5 6/10)", Expr::Bool(true)),
            ("(= 16/5 33/10)", Expr::Bool(false)),
            ("(+ 3/4 2)", Expr::Rational(11, 4)),
            ("(- 3/4 2)", Expr::Rational(-5, 4)),
            ("(* 3/4 2)", Expr::Rational(3, 2)),
            ("(/ 3/4 2)", Expr::Rational(3, 8)),
            ("(< 3/4 1)", Expr::Bool(true)),
            ("(> 9/2 4)", Expr::Bool(true)),
            ("(< 5/4 1)", Expr::Bool(false)),
            ("(> 9/19 4)", Expr::Bool(false)),
            ("(= 16/4 4)", Expr::Bool(true)),
            ("(= 16/5 4)", Expr::Bool(false)),
            ("(< 3/4 0.8)", Expr::Bool(true)),
            ("(> 9/2 3.4)", Expr::Bool(true)),
            ("(< 5/4 1.2)", Expr::Bool(false)),
            ("(> 9/19 0.5)", Expr::Bool(false)),
            ("(= 16/4 4.0)", Expr::Bool(true)),
            ("(= 16/5 3.9)", Expr::Bool(false)),
            ("(abs -5)", Expr::Integer(5)),
            ("(abs 5)", Expr::Integer(5)),
            ("(abs -5.0)", Expr::Float(5.0)),
            ("(abs 5.0)", Expr::Float(5.0)),
            ("(< 1 2 3)", Expr::Bool(true)),
            ("(< 1 3 2)", Expr::Bool(false)),
            ("(<= 1 1 1)", Expr::Bool(true)),
            ("(<= 1 0 1)", Expr::Bool(false)),
            ("(> 3 2 1)", Expr::Bool(true)),
            ("(> 1 3 2)", Expr::Bool(false)),
            ("(>= 1 1 1)", Expr::Bool(true)),
            ("(>= 1 1 2)", Expr::Bool(false)),
            ("(= -1 -1 -1)", Expr::Bool(true)),
            ("(= -1 -1 -2)", Expr::Bool(false)),
            (
                "(cons 1 2)",
                Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
                })),
            ),
            (
                "'(1 . 2)",
                Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
                })),
            ),
            (
                "'(1 2 . 3)",
                Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Cons(Box::new(Cons {
                        car: Expr::Integer(2),
                        cdr: Expr::Integer(3),
                    })),
                })),
            ),
            (
                "'(1 . (2 . 3))",
                Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Cons(Box::new(Cons {
                        car: Expr::Integer(2),
                        cdr: Expr::Integer(3),
                    })),
                })),
            ),
            (
                "(list 1 2 3)",
                Expr::from_vec(vec![Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)]),
            ),
            (
                "'(1 2 3)",
                Expr::from_vec(vec![Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)]),
            ),
            (
                "'(1 . (2 . (3 . ())))",
                Expr::from_vec(vec![Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)]),
            ),
            ("(define a 13)", Expr::Unspecified),
            ("(+ 8 a)", Expr::Integer(21)),
            ("(define f (lambda (a b) (+ (* 3 a) b)))", Expr::Unspecified),
            ("(f 7 a)", Expr::Integer(34)),
            ("(f 7.0 a)", Expr::Float(34.0)),
            ("(if (> 3 7) (- 3 7) (- 7 3))", Expr::Integer(4)),
            ("(if (< 3 7) (- 3 7) (- 7 3))", Expr::Integer(-4)),
            ("(begin (+ 4 7) (- 5 2) (* 7 3))", Expr::Integer(21)),
            ("(let ((a 14) (b 7)) (+ a b) (- a b))", Expr::Integer(7)),
            ("(length '())", Expr::Integer(0)),
            ("(length '(4 5 6))", Expr::Integer(3)),
            (
                "(append '(1 2) '(3) '() '(4))",
                Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ]),
            ),
            ("(set! a -1)", Expr::Unspecified),
            ("a", Expr::Integer(-1)),
        ];
        validate(steps);
    }

    #[test]
    fn test_and_or_not() {
        let steps = vec![
            ("(and)", Expr::Bool(true)),
            ("(and #t #t #f)", Expr::Bool(false)),
            ("(and #t #t #t)", Expr::Bool(true)),
            ("(or)", Expr::Bool(false)),
            ("(or #f #f #f)", Expr::Bool(false)),
            ("(or #f #t #f)", Expr::Bool(true)),
            ("(not #t)", Expr::Bool(false)),
            ("(not #f)", Expr::Bool(true)),
            ("(not 3)", Expr::Bool(false)),
            ("(not (list 3))", Expr::Bool(false)),
            ("(not '())", Expr::Bool(false)),
            ("(not 'nil)", Expr::Bool(false)),
        ];
        validate(steps);
    }

    #[test]
    fn test_quote() {
        let steps = vec![
            ("(quote ())", Expr::from_vec(vec![])),
            (
                "(quote (#t #f))",
                Expr::from_vec(vec![Expr::Bool(true), Expr::Bool(false)]),
            ),
            ("(quote 42.0)", Expr::Float(42.0)),
            (
                "(quote (* 3 4))",
                Expr::from_vec(vec![
                    symbol_from_str("*"),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ]),
            ),
            ("'()", Expr::from_vec(vec![])),
            (
                "'(#t #f)",
                Expr::from_vec(vec![Expr::Bool(true), Expr::Bool(false)]),
            ),
            ("'42.0", Expr::Float(42.0)),
            (
                "'(* 3 4)",
                Expr::from_vec(vec![
                    symbol_from_str("*"),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ]),
            ),
        ];
        validate(steps);
    }

    #[test]
    fn test_predicates() {
        let steps = vec![
            ("(list? '())", Expr::Bool(true)),
            ("(list? '(1 2 3))", Expr::Bool(true)),
            ("(list? (list 1 2 3))", Expr::Bool(true)),
            ("(list? 42)", Expr::Bool(false)),
            ("(list? (cons 17 18))", Expr::Bool(false)),
            ("(pair? '())", Expr::Bool(false)),
            ("(pair? '(1 2 3))", Expr::Bool(true)),
            ("(pair? (list 1 2 3))", Expr::Bool(true)),
            ("(pair? 42)", Expr::Bool(false)),
            ("(pair? (cons 17 18))", Expr::Bool(true)),
            ("(null? 0)", Expr::Bool(false)),
            ("(null? #f)", Expr::Bool(false)),
            ("(null? '())", Expr::Bool(true)),
            ("(null? '(1))", Expr::Bool(false)),
            ("(number? 42)", Expr::Bool(true)),
            ("(number? 42.0)", Expr::Bool(true)),
            ("(number? \"hello\")", Expr::Bool(false)),
            ("(number? 'a)", Expr::Bool(false)),
            ("(number? '())", Expr::Bool(false)),
            ("(number? '(1 2 3))", Expr::Bool(false)),
            ("(number? #t)", Expr::Bool(false)),
            ("(symbol? 42)", Expr::Bool(false)),
            ("(symbol? 42.0)", Expr::Bool(false)),
            ("(symbol? \"hello\")", Expr::Bool(false)),
            ("(symbol? 'a)", Expr::Bool(true)),
            ("(symbol? '())", Expr::Bool(false)),
            ("(symbol? '(1 2 3))", Expr::Bool(false)),
            ("(symbol? #t)", Expr::Bool(false)),
            ("(string? \"hello\")", Expr::Bool(true)),
            ("(string? 3.14)", Expr::Bool(false)),
            ("(string? '())", Expr::Bool(false)),
            ("(boolean? #t)", Expr::Bool(true)),
            ("(boolean? #f)", Expr::Bool(true)),
            ("(boolean? \"hello\")", Expr::Bool(false)),
            ("(boolean? 3.14)", Expr::Bool(false)),
            ("(boolean? '())", Expr::Bool(false)),
            ("(procedure? (lambda (x) (* 2 x)))", Expr::Bool(true)),
            ("(procedure? #f)", Expr::Bool(false)),
            ("(procedure? \"hello\")", Expr::Bool(false)),
            ("(procedure? 3.14)", Expr::Bool(false)),
            ("(procedure? '())", Expr::Bool(false)),
            ("(even? 2)", Expr::Bool(true)),
            ("(even? 3)", Expr::Bool(false)),
            ("(even? -2)", Expr::Bool(true)),
            ("(even? -3)", Expr::Bool(false)),
            ("(odd? 2)", Expr::Bool(false)),
            ("(odd? 3)", Expr::Bool(true)),
            ("(odd? -2)", Expr::Bool(false)),
            ("(odd? -3)", Expr::Bool(true)),
            ("(positive? 2)", Expr::Bool(true)),
            ("(positive? 2.0)", Expr::Bool(true)),
            ("(positive? -2)", Expr::Bool(false)),
            ("(positive? -2.0)", Expr::Bool(false)),
            ("(negative? 2)", Expr::Bool(false)),
            ("(negative? 2.0)", Expr::Bool(false)),
            ("(negative? -2)", Expr::Bool(true)),
            ("(negative? -2.0)", Expr::Bool(true)),
            ("(zero? 0)", Expr::Bool(true)),
            ("(zero? 0.0)", Expr::Bool(true)),
            ("(zero? -0)", Expr::Bool(true)),
            ("(zero? -0.0)", Expr::Bool(true)),
            ("(zero? 1)", Expr::Bool(false)),
            ("(zero? 0.0001)", Expr::Bool(false)),
            ("(zero? -1)", Expr::Bool(false)),
            ("(zero? -0.0001)", Expr::Bool(false)),
        ];
        validate(steps);
    }

    #[test]
    fn test_apply() {
        let steps = vec![
            ("(apply + '(3 4))", Expr::Integer(7)),
            ("(apply * (list -5 4))", Expr::Integer(-20)),
        ];
        validate(steps);
    }

    #[test]
    fn test_multistep_function_1() {
        let steps = vec![
            (
                "(define f (lambda (x) (define a 3) (* a x)))",
                Expr::Unspecified,
            ),
            ("(f 4)", Expr::Integer(12)),
        ];
        validate(steps);
    }

    #[test]
    fn test_multistep_function_2() {
        let steps = vec![
            ("(define (f x) (define a 3) (* a x))", Expr::Unspecified),
            ("(f 4)", Expr::Integer(12)),
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
                Expr::Unspecified,
            ),
            ("(fact 11)", Expr::Integer(39916800)),
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
                Expr::Unspecified,
            ),
            ("(sqrt 2)", Expr::Float(1.4142156862745097)),
        ];
        validate(steps);
    }

    #[test]
    fn test_sqrt_newton_2() {
        let steps = vec![
            ("(define (square x) (* x x))", Expr::Unspecified),
            ("(define (average x y) (/ (+ x y) 2))", Expr::Unspecified),
            (
                "(define (good-enough? guess x)
                    (< (abs (- (square guess) x)) 0.001))",
                Expr::Unspecified,
            ),
            (
                "(define (improve guess x)
                    (average guess (/ x guess)))",
                Expr::Unspecified,
            ),
            (
                "(define (sqrt-iter guess x)
                    (if (good-enough? guess x)
                        guess
                        (sqrt-iter (improve guess x) x)))",
                Expr::Unspecified,
            ),
            (
                "(define (sqrt x)
                    (sqrt-iter 1.0 x))",
                Expr::Unspecified,
            ),
            ("(sqrt 2)", Expr::Float(1.4142156862745097)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_let_lambda() {
        let steps = vec![
            (
                "(define f (let ((a 3)) (lambda (x) (* a x))))",
                Expr::Unspecified,
            ),
            ("(f 5)", Expr::Integer(15)),
            ("(f -4)", Expr::Integer(-12)),
            ("(define a 1)", Expr::Unspecified),
            ("(f 5)", Expr::Integer(15)),
            ("(f -4)", Expr::Integer(-12)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_with_unbound() {
        let steps = vec![
            ("(define (f x) (+ a x))", Expr::Unspecified),
            ("(define a 3)", Expr::Unspecified),
            ("(f 5)", Expr::Integer(8)),
            ("(define a -17)", Expr::Unspecified),
            ("(f 3)", Expr::Integer(-14)),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_unspecified() {
        let steps = vec![
            ("(define (g) (define h))", Expr::Unspecified),
            ("(define a (g))", Expr::Unspecified),
            ("a", Expr::Unspecified),
        ];
        validate(steps);
    }

    #[test]
    fn test_define_function_using_closure() {
        let steps = vec![
            ("(define (f a) (lambda (b) (+ a b)))", Expr::Unspecified),
            ("(define g1 (f 2))", Expr::Unspecified),
            ("(g1 3)", Expr::Integer(5)),
            ("(define (g2 c) (g1 c))", Expr::Unspecified),
            ("(g2 4)", Expr::Integer(6)),
            ("(define (g3 c) ((f 2) c))", Expr::Unspecified),
            ("(g3 5)", Expr::Integer(7)),
            ("(define g4 (lambda (c) (g1 c)))", Expr::Unspecified),
            ("(g4 6)", Expr::Integer(8)),
            ("(define g5 (lambda (c) ((f 2) c)))", Expr::Unspecified),
            ("(g5 7)", Expr::Integer(9)),
        ];
        validate(steps);
    }

    #[test]
    fn test_higher_order() {
        let steps = vec![
            (
                "(define (make-adder n) (lambda (x) (+ x n)))",
                Expr::Unspecified,
            ),
            ("((make-adder 3) 7)", Expr::Integer(10)),
        ];
        validate(steps);
    }

    #[test]
    fn test_nested_function() {
        let steps = vec![
            (
                "(define (outer-func x) (define (inner-func y) (+ x y)) (inner-func 10))",
                Expr::Unspecified,
            ),
            ("(outer-func 5)", Expr::Integer(15)),
        ];
        validate(steps);
    }

    #[test]
    fn test_function_scope() {
        let steps = vec![
            (
                "(define (outer x) (define y (+ x 1)) (lambda (z) (+ y z)))",
                Expr::Unspecified,
            ),
            ("((outer 3) 2)", Expr::Integer(6)),
        ];
        validate(steps);
    }

    #[test]
    fn test_cons_car_cdr() {
        let steps = vec![
            ("(cons 1 '())", Expr::from_vec(vec![Expr::Integer(1)])),
            (
                "(cons 1 '(2 3))",
                Expr::from_vec(vec![Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)]),
            ),
            ("(car '(1 2 3))", Expr::Integer(1)),
            (
                "(cdr '(1 2 3))",
                Expr::from_vec(vec![Expr::Integer(2), Expr::Integer(3)]),
            ),
            (
                "(cons 1 2)",
                Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
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
                Expr::Unspecified,
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
            ("(define a 42)", Expr::Unspecified),
            ("(when (> 0 1) (set! a 43))", Expr::Unspecified),
            ("a", Expr::Integer(42)),
            ("(when (> 1 0) (set! a 44))", Expr::Unspecified),
            ("a", Expr::Integer(44)),
            ("(unless (> 0 1) (set! a 43))", Expr::Unspecified),
            ("a", Expr::Integer(43)),
            ("(unless (> 1 0) (set! a 42))", Expr::Unspecified),
            ("a", Expr::Integer(43)),
        ];
        validate(steps);
    }

    #[test]
    fn test_filter() {
        let steps = vec![
            (
                "(filter (lambda (x) (< x 3)) '(5 4 3 2 1))",
                Expr::from_vec(vec![Expr::Integer(2), Expr::Integer(1)]),
            ),
            (
                "(filter (lambda (x) (< x 0)) '(-5 4 -3 2 -1))",
                Expr::from_vec(vec![
                    Expr::Integer(-5),
                    Expr::Integer(-3),
                    Expr::Integer(-1),
                ]),
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
                Expr::Unspecified,
            ),
            (
                "(quicksort '(34 7 23 32 5 62 32 2 1 6 45 78 99 3))",
                Expr::from_vec(
                    vec![1, 2, 3, 5, 6, 7, 23, 32, 32, 34, 45, 62, 78, 99]
                        .into_iter()
                        .map(Expr::Integer)
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
                Expr::Unspecified,
            ),
            ("(ackermann 3 4)", Expr::Integer(125)),
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
                Expr::Unspecified,
            ),
            ("(fib 13)", Expr::Integer(233)),
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
                Expr::Unspecified,
            ),
            ("(fib 20)", Expr::Integer(6765)),
        ];
        validate(steps);
    }
}
