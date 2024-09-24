use std::cell::RefCell;
use std::fmt;
use std::io::{self, BufRead};
use std::iter::{zip, Peekable};
use std::mem::take;
use std::rc::Rc;
use std::str::{Chars, FromStr};

use internment::Intern;
use rustc_hash::FxHashMap;

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Quote,
    Dot,
    Atom(String),
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
        }
    }

    fn read_other(&mut self) -> String {
        let mut result = String::new();

        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '\'' {
                break;
            } else {
                result.push(c);
                self.input.next();
            }
        }

        result
    }

    fn read_string_literal(&mut self) -> String {
        let mut result = String::new();
        let mut cnt = 0;

        while let Some(&c) = self.input.peek() {
            result.push(c);
            self.input.next();
            if c == '"' {
                cnt += 1;
            }
            if cnt == 2 {
                break;
            }
        }

        result
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&c) = self.input.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' => {
                    self.input.next();
                    continue;
                }
                '(' => {
                    self.input.next();
                    return Some(Token::LParen);
                }
                ')' => {
                    self.input.next();
                    return Some(Token::RParen);
                }
                '\'' => {
                    self.input.next();
                    return Some(Token::Quote);
                }
                '.' => {
                    self.input.next();
                    return Some(Token::Dot);
                }
                '"' => {
                    return Some(Token::Atom(self.read_string_literal()));
                }
                _ => {
                    return Some(Token::Atom(self.read_other()));
                }
            }
        }
        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Quote,
    Lambda,
    Define,
    If,
    When,
    Unless,
    Cond,
    Let,
    Set,
    Begin,
    And,
    Or,
    // TODO add more special forms here
}

impl FromStr for Keyword {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lambda" => Ok(Keyword::Lambda),
            "quote" => Ok(Keyword::Quote),
            "define" => Ok(Keyword::Define),
            "if" => Ok(Keyword::If),
            "when" => Ok(Keyword::When),
            "unless" => Ok(Keyword::Unless),
            "cond" => Ok(Keyword::Cond),
            "let" => Ok(Keyword::Let),
            "set!" => Ok(Keyword::Set),
            "begin" => Ok(Keyword::Begin),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            _ => Err("Not a keyword"),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Lambda => write!(f, "lambda"),
            Keyword::Quote => write!(f, "quote"),
            Keyword::Define => write!(f, "define"),
            Keyword::If => write!(f, "if"),
            Keyword::When => write!(f, "when"),
            Keyword::Unless => write!(f, "unless"),
            Keyword::Cond => write!(f, "cond"),
            Keyword::Let => write!(f, "let"),
            Keyword::Set => write!(f, "set!"),
            Keyword::Begin => write!(f, "begin"),
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cons {
    car: Expr,
    cdr: Expr,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum Expr {
    #[default]
    Unspecified,
    Null,
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Keyword(Keyword),
    Symbol(Intern<String>),
    Procedure(Procedure),
    Cons(Box<Cons>),
}

pub fn parse_tokens(tokens: &mut Peekable<Tokenizer>) -> Result<Vec<Expr>, &'static str> {
    let mut expressions = Vec::new();
    while tokens.peek().is_some() {
        expressions.push(parse_expression(tokens)?);
    }
    Ok(expressions)
}

fn parse_expression(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, &'static str> {
    match tokens.next() {
        Some(Token::LParen) => parse_list(tokens),
        Some(Token::Quote) => parse_quote(tokens),
        Some(Token::Atom(s)) => parse_atom(s),
        Some(Token::Dot) => Err("Unexpected dot"),
        Some(Token::RParen) => Err("Unexpected closing parenthesis"),
        None => Err("Unexpected end of input"),
    }
}

fn parse_list(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, &'static str> {
    if let Some(token) = tokens.peek() {
        match token {
            Token::RParen => {
                tokens.next();
                return Ok(Expr::Null);
            }
            Token::Dot => {
                tokens.next();
                let res = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::RParen) => return res,
                    _ => return Err("Expected closing parenthesis"),
                }
            }
            _ => {
                return Ok(Expr::Cons(Box::new(Cons {
                    car: parse_expression(tokens)?,
                    cdr: parse_list(tokens)?,
                })));
            }
        }
    }
    Err("Unterminated list")
}

fn parse_quote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, &'static str> {
    Ok(Expr::Cons(Box::new(Cons {
        car: Expr::Keyword(Keyword::Quote),
        cdr: Expr::Cons(Box::new(Cons {
            car: parse_expression(tokens)?,
            cdr: Expr::Null,
        })),
    })))
}

fn parse_atom(s: String) -> Result<Expr, &'static str> {
    if let Ok(int) = s.parse::<i64>() {
        Ok(Expr::Integer(int))
    } else if let Ok(float) = s.parse::<f64>() {
        Ok(Expr::Float(float))
    } else if s == "#t" {
        Ok(Expr::Bool(true))
    } else if s == "#f" {
        Ok(Expr::Bool(false))
    } else if s.starts_with('"') && s.ends_with('"') {
        Ok(Expr::Str(s[1..s.len() - 1].to_string()))
    } else if let Ok(keyword) = Keyword::from_str(&s) {
        Ok(Expr::Keyword(keyword))
    } else {
        Ok(Expr::Symbol(Intern::new(s)))
    }
}

pub fn parse(code: &str) -> Result<Vec<Expr>, &'static str> {
    let tokens = Tokenizer::new(code);
    parse_tokens(&mut tokens.peekable())
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unspecified => Ok(()),
            Expr::Null => write!(f, "()"),
            Expr::Bool(v) => write!(f, "{}", if *v { "#t" } else { "#f" }),
            Expr::Integer(v) => write!(f, "{}", v),
            Expr::Float(v) => write!(f, "{}", v),
            Expr::Str(v) => write!(f, "\"{}\"", v),
            Expr::Keyword(k) => write!(f, "{}", k),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Procedure(Procedure::BuiltIn(_)) => write!(f, "#[built-in procedure]"),
            Expr::Procedure(Procedure::UserDefined(_)) => write!(f, "#[user-defined procedure]"),
            Expr::Cons(p) => {
                write!(f, "(")?;
                let mut cur = p;
                loop {
                    write!(f, "{}", cur.car)?;
                    match &cur.cdr {
                        Expr::Null => break,
                        Expr::Cons(pp) => cur = pp,
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
        }
    }
}

impl Expr {
    fn from_vec(mut v: Vec<Expr>) -> Self {
        match v.len() {
            0 => Expr::Null,
            _ => Expr::Cons(Box::new(Cons {
                car: take(&mut v[0]),
                cdr: Expr::from_vec(v.split_off(1)),
            })),
        }
    }

    fn into_vec(self) -> Result<Vec<Expr>, &'static str> {
        let mut res = Vec::new();
        let mut cur = self;
        loop {
            match cur {
                Expr::Cons(pair) => {
                    res.push(pair.car);
                    cur = pair.cdr;
                }
                Expr::Null => return Ok(res),
                _ => return Err("Not a proper list"),
            }
        }
    }

    fn borrow_vec(&self) -> Result<Vec<&Expr>, &'static str> {
        let mut cur = self;
        let mut res = Vec::new();
        loop {
            match cur {
                Expr::Null => break,
                Expr::Cons(pair) => {
                    res.push(&pair.car);
                    cur = &pair.cdr;
                }
                _ => return Err("Not a proper list"),
            }
        }
        Ok(res)
    }

    fn add(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Integer(a + b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Float(*a as f64 + b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Float(a + b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Float(a + *b as f64)),
            _ => Err("Cannot add types"),
        }
    }

    fn mul(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Integer(a * b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Float(*a as f64 * b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Float(a * b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Float(a * *b as f64)),
            _ => Err("Cannot multiply types"),
        }
    }

    fn sub(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Integer(a - b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Float(*a as f64 - b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Float(a - b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Float(a - *b as f64)),
            _ => Err("Cannot subtract types"),
        }
    }

    fn div(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Float(*a as f64 / *b as f64)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Float(*a as f64 / b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Float(a / b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Float(a / *b as f64)),
            _ => Err("Cannot divide types"),
        }
    }

    fn lt(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Bool(a < b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Bool((*a as f64) < *b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Bool(a < b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Bool(*a < (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn gt(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Bool(a > b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Bool((*a as f64) > *b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Bool(a > b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Bool(*a > (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn leq(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Bool(a <= b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Bool((*a as f64) <= *b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Bool(a <= b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Bool(*a <= (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn geq(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Bool(a >= b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Bool((*a as f64) >= *b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Bool(a >= b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Bool(*a >= (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn iseq(&self, other: &Expr) -> Result<Expr, &'static str> {
        match (self, other) {
            (Expr::Integer(a), Expr::Integer(b)) => Ok(Expr::Bool(a == b)),
            (Expr::Integer(a), Expr::Float(b)) => Ok(Expr::Bool(*a as f64 == *b)),
            (Expr::Float(a), Expr::Float(b)) => Ok(Expr::Bool(a == b)),
            (Expr::Float(a), Expr::Integer(b)) => Ok(Expr::Bool(*a == *b as f64)),
            _ => Err("Cannot compare types"),
        }
    }
}

fn builtin_add(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(0), |acc, x| acc.add(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_mul(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(1), |acc, x| acc.mul(&x))?;
    Ok(MaybeValue::Just(res))
}

fn builtin_sub(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    let mut values_iter = values.into_iter();
    let res = match values_iter.next() {
        None => Expr::Integer(0),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.sub(&x))?,
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_abs(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Abs needs exactly one argument");
    }
    let res = match values.first() {
        Some(value) => match value {
            Expr::Integer(n) => Expr::Integer(if *n >= 0 { *n } else { -*n }),
            Expr::Float(n) => Expr::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("Abs needs a number argument"),
        },
        _ => return Err("Abs needs exactly one argument"),
    };
    Ok(MaybeValue::Just(res))
}

fn builtin_div(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    let mut values_iter = values.into_iter();
    let res = match values_iter.next() {
        None => Expr::Integer(1),
        Some(v) => values_iter.try_fold(v, |acc, x| acc.div(&x))?,
    };
    Ok(MaybeValue::Just(res))
}

type CmpFnType = fn(&Expr, &Expr) -> Result<Expr, &'static str>;

fn builtin_cmp(values: Vec<Expr>, method: CmpFnType) -> Result<MaybeValue, &'static str> {
    for (a, b) in values.iter().zip(values.iter().skip(1)) {
        match method(a, b) {
            Ok(Expr::Bool(false)) => return Ok(MaybeValue::Just(Expr::Bool(false))),
            Err(s) => return Err(s),
            _ => (),
        }
    }
    Ok(MaybeValue::Just(Expr::Bool(true)))
}

fn builtin_lt(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    builtin_cmp(values, Expr::lt)
}

fn builtin_gt(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    builtin_cmp(values, Expr::gt)
}

fn builtin_leq(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    builtin_cmp(values, Expr::leq)
}

fn builtin_geq(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    builtin_cmp(values, Expr::geq)
}

fn builtin_iseq(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    builtin_cmp(values, Expr::iseq)
}

fn builtin_not(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Not needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(match values[0] {
        Expr::Bool(b) => !b,
        _ => false,
    })))
}

fn builtin_list(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    Ok(MaybeValue::Just(Expr::from_vec(values)))
}

fn builtin_apply(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 2 {
        return Err("Apply needs exactly two argument");
    }
    let first = take(&mut values[0]);
    let second = take(&mut values[1]);
    match first {
        Expr::Procedure(proc) => Ok(MaybeValue::TailCall(proc, second.into_vec()?)),
        _ => Err("Apply needs a procedure and a list as arguments"),
    }
}

fn builtin_length(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument");
    }
    match take(&mut values[0]).into_vec() {
        Ok(v) => Ok(MaybeValue::Just(Expr::Integer(v.len() as i64))),
        _ => Err("Cannot compute length (is it a list?)"),
    }
}

fn builtin_append(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    let mut all = Vec::new();
    for value in values {
        match value {
            Expr::Cons(_) => all.extend(value.into_vec()?),
            Expr::Null => (),
            _ => return Err("Append needs lists as arguments"),
        }
    }
    Ok(MaybeValue::Just(Expr::from_vec(all)))
}

fn builtin_ispair(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Cons(_)
    ))))
}

fn builtin_islist(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument");
    }
    match take(&mut values[0]) {
        Expr::Cons(pair) => match builtin_islist(vec![pair.cdr])? {
            MaybeValue::Just(Expr::Bool(b)) => Ok(MaybeValue::Just(Expr::Bool(b))),
            _ => Err("Unexpected non-boolean"),
        },
        Expr::Null => Ok(MaybeValue::Just(Expr::Bool(true))),
        _ => Ok(MaybeValue::Just(Expr::Bool(false))),
    }
}

fn builtin_isnull(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Null
    ))))
}

fn builtin_isnumber(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Float(_) | Expr::Integer(_)
    ))))
}

fn builtin_issymbol(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Symbol(_)
    ))))
}

fn builtin_isstring(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("String? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Str(_)
    ))))
}

fn builtin_isboolean(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Boolean? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Bool(_)
    ))))
}

fn builtin_isprocedure(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Procedure? needs exactly one argument");
    }
    Ok(MaybeValue::Just(Expr::Bool(matches!(
        &values[0],
        Expr::Procedure(_)
    ))))
}

fn builtin_iseven(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Even? needs exactly one argument");
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(v % 2 == 0))),
        _ => Err("Even? needs an integer argument"),
    }
}

fn builtin_isodd(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Odd? needs exactly one argument");
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(v % 2 != 0))),
        _ => Err("Odd? needs an integer argument"),
    }
}

fn builtin_ispositive(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Positive? needs exactly one argument");
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v > 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v > 0 as f64))),
        _ => Err("Positive? needs a number argument"),
    }
}

fn builtin_isnegative(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Negative? needs exactly one argument");
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v < 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v < 0 as f64))),
        _ => Err("Negative? needs a number argument"),
    }
}

fn builtin_iszero(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Zero? needs exactly one argument");
    }
    match &values[0] {
        Expr::Integer(v) => Ok(MaybeValue::Just(Expr::Bool(*v == 0))),
        Expr::Float(v) => Ok(MaybeValue::Just(Expr::Bool(*v == 0 as f64))),
        _ => Err("Zero? needs a number argument"),
    }
}

fn builtin_cons(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 2 {
        return Err("Cons needs exactly two argument");
    }
    let car = take(&mut values[0]);
    let cdr = take(&mut values[1]);
    Ok(MaybeValue::Just(Expr::Cons(Box::new(Cons { car, cdr }))))
}

fn builtin_car(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument");
    }
    match take(&mut values[0]) {
        Expr::Cons(p) => Ok(MaybeValue::Just(p.car)),
        _ => Err("Car needs a pair as argument"),
    }
}

fn builtin_cdr(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument");
    }
    match take(&mut values[0]) {
        Expr::Cons(p) => Ok(MaybeValue::Just(p.cdr)),
        _ => Err("Cdr needs a pair as argument"),
    }
}

fn builtin_filter(mut values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 2 {
        return Err("Filter needs exactly two argument");
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
        _ => Err("Not a procedure"),
    }
}

fn builtin_read(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if !values.is_empty() {
        return Err("Read takes no arguments");
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

fn builtin_write(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if values.len() != 1 {
        return Err("Write needs exactly one argument");
    }
    print!("{}", values[0]);
    Ok(MaybeValue::Just(Expr::Unspecified))
}

fn builtin_newline(values: Vec<Expr>) -> Result<MaybeValue, &'static str> {
    if !values.is_empty() {
        return Err("Write takes no arguments");
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
    fn materialize(self) -> Result<Expr, &'static str> {
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

type BuiltInFnType = fn(Vec<Expr>) -> Result<MaybeValue, &'static str>;

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

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Expr, &'static str> {
        self.maybe_evaluate(expr)?.materialize()
    }

    fn maybe_evaluate(&mut self, expr: &Expr) -> Result<MaybeValue, &'static str> {
        match expr {
            Expr::Integer(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Float(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Str(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Bool(_) => Ok(MaybeValue::Just(expr.clone())),
            Expr::Cons(p) => self.maybe_evaluate_pair(p),
            Expr::Symbol(s) => match self.get(s) {
                Some(value) => Ok(MaybeValue::Just(value)),
                None => Err("Undefined symbol"),
            },
            _ => Err("Cannot evaluate expression"),
        }
    }

    fn maybe_evaluate_pair(&mut self, pair: &Cons) -> Result<MaybeValue, &'static str> {
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
            car => match self.evaluate(car) {
                Ok(Expr::Procedure(proc)) => {
                    let mut args_values = Vec::new();
                    for arg in args {
                        args_values.push(self.evaluate(arg)?)
                    }
                    Ok(MaybeValue::TailCall(proc, args_values))
                }
                _ => Err("Not a procedure call"),
            },
        }
    }

    fn evaluate_quote(&mut self, args: Vec<&Expr>) -> Result<Expr, &'static str> {
        if args.len() != 1 {
            return Err("Quote needs exactly one argument");
        }
        Ok(args[0].clone())
    }

    fn evaluate_lambda(&mut self, mut args: Vec<&Expr>) -> Result<Expr, &'static str> {
        if args.is_empty() {
            return Err("Lambda needs at least one argument");
        }
        match args[0] {
            Expr::Cons(_) => {
                let mut params = Vec::new();
                for expr in args[0].borrow_vec()? {
                    match expr {
                        Expr::Symbol(s) => params.push(*s),
                        _ => return Err("Not a symbol"),
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
            _ => Err("First argument to lambda must be a list of symbols"),
        }
    }

    fn evaluate_define(&mut self, mut args: Vec<&Expr>) -> Result<Expr, &'static str> {
        if args.is_empty() {
            return Err("Define needs at least one argument");
        }
        match args[0] {
            Expr::Symbol(key) => {
                let value = match args.len() {
                    1 => Expr::Unspecified,
                    2 => self.evaluate(args[1])?,
                    _ => return Err("Define with a symbol gets at most two arguments"),
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
                        _ => return Err("Not a symbol"),
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
            _ => Err("Define needs a symbol or a list as first argument"),
        }
    }

    fn evaluate_set(&mut self, args: Vec<&Expr>) -> Result<Expr, &'static str> {
        if args.len() != 2 {
            return Err("Set! needs exactly two arguments");
        }
        match args[0] {
            Expr::Symbol(s) => {
                if self.get(s).is_none() {
                    return Err("Symbol is not bound");
                }
                let value = self.evaluate(args[1])?;
                self.set(*s, value);
                Ok(Expr::Unspecified)
            }
            _ => Err("First argument to set! must be a symbol"),
        }
    }

    fn evaluate_if(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
        if args.len() < 2 || args.len() > 3 {
            return Err("If accepts two or three arguments");
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
            _ => Err("First argument to if did not evaluate to a boolean"),
        }
    }

    fn evaluate_cond(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
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
                        _ => return Err("Clause did not evaluate to a boolean"),
                    }
                }
                _ => return Err("Not a list"),
            }
        }
        Ok(MaybeValue::Just(Expr::Unspecified))
    }

    fn evaluate_when(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
        if args.is_empty() {
            return Err("When needs at least one argument");
        }
        match self.evaluate(args[0])? {
            Expr::Bool(true) => self.evaluate_begin(args.split_off(1)),
            Expr::Bool(false) => Ok(MaybeValue::Just(Expr::Unspecified)),
            _ => Err("First argument to when did not evaluate to a boolean"),
        }
    }

    fn evaluate_unless(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
        if args.is_empty() {
            return Err("Unless needs at least one argument");
        }
        match self.evaluate(args[0])? {
            Expr::Bool(true) => Ok(MaybeValue::Just(Expr::Unspecified)),
            Expr::Bool(false) => self.evaluate_begin(args.split_off(1)),
            _ => Err("First argument to unless did not evaluate to a boolean"),
        }
    }

    fn evaluate_let(&mut self, mut args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
        if args.is_empty() {
            return Err("Let needs at least one argument");
        }
        let mut child = self.child();
        for expr in args[0].borrow_vec()? {
            match expr {
                Expr::Cons(p) => match (&p.car, &p.cdr) {
                    (Expr::Symbol(s), Expr::Cons(cdr)) => {
                        if cdr.cdr != Expr::Null {
                            return Err("Not a 2-list");
                        }
                        let value = child.evaluate(&cdr.car)?;
                        child.set(*s, value);
                    }
                    _ => return Err("Not a symbol"),
                },
                _ => return Err("Not a 2-list"),
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

    fn evaluate_begin(&mut self, args: Vec<&Expr>) -> Result<MaybeValue, &'static str> {
        let mut out = MaybeValue::Just(Expr::Unspecified);
        if let Some((last, rest)) = args.split_last() {
            for expr in rest {
                self.evaluate(expr)?;
            }
            out = self.maybe_evaluate(last)?;
        }
        Ok(out)
    }

    fn evaluate_and(&mut self, args: Vec<&Expr>) -> Result<Expr, &'static str> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Expr::Bool(true)) => continue,
                Ok(Expr::Bool(false)) => return Ok(Expr::Bool(false)),
                _ => return Err("Cannot \"and\" type"),
            }
        }
        Ok(Expr::Bool(true))
    }

    fn evaluate_or(&mut self, args: Vec<&Expr>) -> Result<Expr, &'static str> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Expr::Bool(true)) => return Ok(Expr::Bool(true)),
                Ok(Expr::Bool(false)) => continue,
                _ => return Err("Cannot \"or\" type"),
            }
        }
        Ok(Expr::Bool(false))
    }
}

trait Callable {
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, &'static str>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<Intern<String>>,
    body: Vec<Expr>,
    env: Environment,
}

impl UserDefinedProcedure {
    #[inline(always)]
    fn call_except_tail(
        &self,
        env: &mut Environment,
        args: Vec<Expr>,
    ) -> Result<MaybeValue, &'static str> {
        let params = &proc.params;
        let body = &proc.body;
        if args.len() != params.len() {
            return Err("Incorrect number of arguments");
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
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, &'static str> {
        let mut local_env = self.env.child();
        let mut out = self.call_except_tail(&mut local_env, args);
        loop {
            match out? {
                MaybeValue::Just(expr) => return Ok(MaybeValue::Just(expr)),
                MaybeValue::TailCall(Procedure::BuiltIn(proc), args) => return proc.call(args),
                MaybeValue::TailCall(Procedure::UserDefined(proc), args) => {
                    out = proc.call_except_tail(&mut local_env, args)
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
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, &'static str> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&self, args: Vec<Expr>) -> Result<MaybeValue, &'static str> {
        match self {
            Procedure::UserDefined(proc) => proc.call(args),
            Procedure::BuiltIn(proc) => proc.call(args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn intern_str(s: &str) -> Intern<String> {
        Intern::new(s.to_string())
    }

    fn symbol_from_str(s: &str) -> Expr {
        Expr::Symbol(intern_str(s))
    }

    #[test]
    fn test_tokenizer() {
        let code = "(define (square x) (* x x)) 'expr (quote (1 2 3)) \"hello world\"";
        let expected: Vec<Token> = vec![
            Token::LParen,
            Token::Atom("define".to_string()),
            Token::LParen,
            Token::Atom("square".to_string()),
            Token::Atom("x".to_string()),
            Token::RParen,
            Token::LParen,
            Token::Atom("*".to_string()),
            Token::Atom("x".to_string()),
            Token::Atom("x".to_string()),
            Token::RParen,
            Token::RParen,
            Token::Quote,
            Token::Atom("expr".to_string()),
            Token::LParen,
            Token::Atom("quote".to_string()),
            Token::LParen,
            Token::Atom("1".to_string()),
            Token::Atom("2".to_string()),
            Token::Atom("3".to_string()),
            Token::RParen,
            Token::RParen,
            Token::Atom("\"hello world\"".to_string()),
        ];
        let tokens: Vec<Token> = Tokenizer::new(code).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_parser() {
        let code = "(define (square x) (* x x))";
        let expected = vec![Expr::from_vec(vec![
            Expr::Keyword(Keyword::Define),
            Expr::from_vec(vec![symbol_from_str("square"), symbol_from_str("x")]),
            Expr::from_vec(vec![
                symbol_from_str("*"),
                symbol_from_str("x"),
                symbol_from_str("x"),
            ]),
        ])];
        let expressions: Vec<Expr> = parse(code).unwrap();
        assert_eq!(expressions, expected);
    }

    #[test]
    fn test_pair_vs_vec() {
        let v = vec![Expr::Integer(3), Expr::Integer(2), Expr::Integer(1)];
        let expr = Expr::from_vec(v.clone());
        let res = expr.into_vec();
        assert_eq!(res, Ok(v));
    }

    #[test]
    fn test_external_repr() {
        let expr = Expr::from_vec(vec![
            Expr::Keyword(Keyword::Define),
            Expr::from_vec(vec![symbol_from_str("f"), symbol_from_str("x")]),
            Expr::Str("hello, world!".to_string()),
            Expr::from_vec(vec![Expr::Keyword(Keyword::Quote), symbol_from_str("a")]),
            Expr::from_vec(vec![
                symbol_from_str("*"),
                symbol_from_str("x"),
                symbol_from_str("2"),
            ]),
            Expr::Cons(Box::new(Cons {
                car: Expr::Integer(-1),
                cdr: Expr::Integer(1),
            })),
        ]);
        assert_eq!(
            expr.to_string(),
            "(define (f x) \"hello, world!\" (quote a) (* x 2) (-1 . 1))"
        );
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

        assert_eq!(builtin_add(values), Err("Cannot add types"));
    }

    fn validate(cases: Vec<(&str, Expr)>) {
        let mut env = Environment::standard().child();
        for (code, val) in cases {
            let expr = parse(code).unwrap().remove(0);
            assert_eq!(env.evaluate(&expr), Ok(val));
        }
    }

    #[test]
    fn test_evaluate() {
        let cases = vec![
            ("(define a 42)", Expr::Unspecified),
            ("42.42", Expr::Float(42.42)),
            ("#t", Expr::Bool(true)),
            ("#f", Expr::Bool(false)),
            ("\"hello, world!\"", Expr::Str("hello, world!".to_string())),
            ("a", Expr::Integer(42)),
            ("(+ 3 2)", Expr::Integer(5)),
            ("(* 3 2)", Expr::Integer(6)),
            ("(+ 3 2.0)", Expr::Float(5.0)),
            ("(* 3.0 2)", Expr::Float(6.0)),
            ("(- 10 2 3)", Expr::Integer(5)),
            ("(/ 24 3 2)", Expr::Float(4.0)),
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
        validate(cases);
    }

    #[test]
    fn test_and_or_not() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_quote() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_predicates() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_apply() {
        let cases = vec![
            ("(apply + '(3 4))", Expr::Integer(7)),
            ("(apply * (list -5 4))", Expr::Integer(-20)),
        ];
        validate(cases);
    }

    #[test]
    fn test_multistep_function_1() {
        let cases = vec![
            (
                "(define f (lambda (x) (define a 3) (* a x)))",
                Expr::Unspecified,
            ),
            ("(f 4)", Expr::Integer(12)),
        ];
        validate(cases);
    }

    #[test]
    fn test_multistep_function_2() {
        let cases = vec![
            ("(define (f x) (define a 3) (* a x))", Expr::Unspecified),
            ("(f 4)", Expr::Integer(12)),
        ];
        validate(cases);
    }

    #[test]
    fn test_factorial() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_sqrt_newton_1() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_sqrt_newton_2() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_define_let_lambda() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_define_with_unbound() {
        let cases = vec![
            ("(define (f x) (+ a x))", Expr::Unspecified),
            ("(define a 3)", Expr::Unspecified),
            ("(f 5)", Expr::Integer(8)),
            ("(define a -17)", Expr::Unspecified),
            ("(f 3)", Expr::Integer(-14)),
        ];
        validate(cases);
    }

    #[test]
    fn test_define_unspecified() {
        let cases = vec![
            ("(define (g) (define h))", Expr::Unspecified),
            ("(define a (g))", Expr::Unspecified),
            ("a", Expr::Unspecified),
        ];
        validate(cases);
    }

    #[test]
    fn test_higher_order() {
        let cases = vec![
            (
                "(define (make-adder n) (lambda (x) (+ x n)))",
                Expr::Unspecified,
            ),
            ("((make-adder 3) 7)", Expr::Integer(10)),
        ];
        validate(cases);
    }

    #[test]
    fn test_nested_function() {
        let cases = vec![
            (
                "(define (outer-func x) (define (inner-func y) (+ x y)) (inner-func 10))",
                Expr::Unspecified,
            ),
            ("(outer-func 5)", Expr::Integer(15)),
        ];
        validate(cases);
    }

    #[test]
    fn test_function_scope() {
        let cases = vec![
            (
                "(define (outer x) (define y (+ x 1)) (lambda (z) (+ y z)))",
                Expr::Unspecified,
            ),
            ("((outer 3) 2)", Expr::Integer(6)),
        ];
        validate(cases);
    }

    #[test]
    fn test_cons_car_cdr() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_cond() {
        let cases = vec![
            (
                "(define (f x) (cond ((< x 0) 'negative) ((> x 0) 'positive) (else 'zero)))",
                Expr::Unspecified,
            ),
            ("(f -1)", symbol_from_str("negative")),
            ("(f 0)", symbol_from_str("zero")),
            ("(f 1)", symbol_from_str("positive")),
        ];
        validate(cases);
    }

    #[test]
    fn test_when_unless() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_filter() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_quicksort() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_ackermann() {
        let cases = vec![
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
        validate(cases);
    }

    #[test]
    fn test_fibonacci_naive() {
        let cases = vec![
            (
                "(define (fib n)
                  (if (< n 2)
                      n
                      (+ (fib (- n 1)) (fib (- n 2)))))",
                Expr::Unspecified,
            ),
            ("(fib 13)", Expr::Integer(233)),
        ];
        validate(cases);
    }

    #[test]
    fn test_fibonacci_tailcall() {
        let cases = vec![
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
        validate(cases);
    }
}
