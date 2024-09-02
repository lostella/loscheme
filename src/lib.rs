use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter::{zip, Peekable};
use std::rc::Rc;
use std::str::{Chars, FromStr};

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
    Lambda,
    Quote,
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Null,
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Keyword(Keyword),
    Symbol(String),
    Procedure(Procedure),
    Cons(Box<Cons>),
}

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Tokenizer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, &'static str> {
        let mut expressions = Vec::new();
        while self.tokens.peek().is_some() {
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expr, &'static str> {
        match self.tokens.next() {
            Some(Token::LParen) => self.parse_list(),
            Some(Token::Quote) => self.parse_quote(),
            Some(Token::Atom(s)) => self.parse_atom(s),
            Some(Token::Dot) => Err("Unexpected dot"),
            Some(Token::RParen) => Err("Unexpected closing parenthesis"),
            None => Err("Unexpected end of input"),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, &'static str> {
        if let Some(token) = self.tokens.peek() {
            match token {
                Token::RParen => {
                    self.tokens.next();
                    return Ok(Expr::Null);
                }
                Token::Dot => {
                    self.tokens.next();
                    let res = self.parse_expression();
                    match self.tokens.next() {
                        Some(Token::RParen) => return res,
                        _ => return Err("Expected closing parenthesis"),
                    }
                }
                _ => {
                    return Ok(Expr::Cons(Box::new(Cons {
                        car: self.parse_expression()?,
                        cdr: self.parse_list()?,
                    })));
                }
            }
        }
        Err("Unterminated list")
    }

    fn parse_quote(&mut self) -> Result<Expr, &'static str> {
        Ok(Expr::Cons(Box::new(Cons {
            car: Expr::Keyword(Keyword::Quote),
            cdr: Expr::Cons(Box::new(Cons {
                car: self.parse_expression()?,
                cdr: Expr::Null,
            })),
        })))
    }

    fn parse_atom(&self, s: String) -> Result<Expr, &'static str> {
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
            Ok(Expr::Symbol(s))
        }
    }
}

pub fn parse_code(code: &str) -> Result<Vec<Expr>, &'static str> {
    let tokenizer = Tokenizer::new(code);
    let mut parser = Parser::new(tokenizer);
    parser.parse()
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
                car: v.remove(0),
                cdr: Expr::from_vec(v),
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
}

fn builtin_add(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(0), |acc, x| acc.add(&x))?;
    Ok(Some(res))
}

fn builtin_mul(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Expr::Integer(1), |acc, x| acc.mul(&x))?;
    Ok(Some(res))
}

fn builtin_sub(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    let mut values_iter = values.into_iter();
    match values_iter.next() {
        None => Ok(Some(Expr::Integer(0))),
        Some(v) => {
            let res = values_iter.try_fold(v, |acc, x| acc.sub(&x))?;
            Ok(Some(res))
        }
    }
}

fn builtin_abs(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Abs needs exactly one argument");
    }
    let res_value = match values.first() {
        Some(value) => match value {
            Expr::Integer(n) => Expr::Integer(if *n >= 0 { *n } else { -*n }),
            Expr::Float(n) => Expr::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("Abs needs a number argument"),
        },
        _ => return Err("Abs needs exactly one argument"),
    };
    Ok(Some(res_value))
}

fn builtin_div(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    let mut values_iter = values.into_iter();
    match values_iter.next() {
        None => Ok(Some(Expr::Integer(1))),
        Some(v) => {
            let res = values_iter.try_fold(v, |acc, x| acc.div(&x))?;
            Ok(Some(res))
        }
    }
}

type CmpFnType = fn(&Expr, &Expr) -> Result<Expr, &'static str>;

fn builtin_cmp(values: Vec<Expr>, method: CmpFnType) -> Result<Option<Expr>, &'static str> {
    for (a, b) in values.iter().zip(values.iter().skip(1)) {
        match method(a, b) {
            Ok(Expr::Bool(false)) => return Ok(Some(Expr::Bool(false))),
            Err(s) => return Err(s),
            _ => (),
        }
    }
    Ok(Some(Expr::Bool(true)))
}

fn builtin_lt(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    builtin_cmp(values, Expr::lt)
}

fn builtin_gt(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    builtin_cmp(values, Expr::gt)
}

fn builtin_leq(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    builtin_cmp(values, Expr::leq)
}

fn builtin_geq(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    builtin_cmp(values, Expr::geq)
}

fn builtin_not(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Not needs exactly one argument");
    }
    match values[0] {
        Expr::Bool(b) => Ok(Some(Expr::Bool(!b))),
        _ => Err("Cannot negate type"),
    }
}

fn builtin_list(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    Ok(Some(Expr::from_vec(values)))
}

fn builtin_apply(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 2 {
        return Err("Apply needs exactly two argument");
    }
    match values.remove(0) {
        Expr::Procedure(proc) => proc.call(values.remove(0).into_vec()?),
        _ => Err("Apply needs a procedure and a list as arguments"),
    }
}

fn builtin_length(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Length needs exactly one argument");
    }
    match values.remove(0).into_vec() {
        Ok(v) => Ok(Some(Expr::Integer(v.len() as i64))),
        _ => Err("Cannot compute length (is it a list?)"),
    }
}

fn builtin_append(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    let mut all = Vec::new();
    for value in values {
        match value {
            Expr::Cons(_) => all.extend(value.into_vec()?),
            Expr::Null => (),
            _ => return Err("Append needs lists as arguments"),
        }
    }
    Ok(Some(Expr::from_vec(all)))
}

fn builtin_ispair(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Pair? needs exactly one argument");
    }
    match &values[0] {
        Expr::Cons(_) => Ok(Some(Expr::Bool(true))),
        _ => Ok(Some(Expr::Bool(false))),
    }
}

fn builtin_islist(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("List? needs exactly one argument");
    }
    match values.remove(0) {
        Expr::Cons(pair) => match builtin_islist(vec![pair.cdr])? {
            Some(Expr::Bool(true)) => Ok(Some(Expr::Bool(true))),
            Some(Expr::Bool(false)) => Ok(Some(Expr::Bool(false))),
            _ => Err("Unexpected non-boolean"),
        },
        Expr::Null => Ok(Some(Expr::Bool(true))),
        _ => Ok(Some(Expr::Bool(false))),
    }
}

fn builtin_isnull(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Null? needs exactly one argument");
    }
    match &values[0] {
        Expr::Null => Ok(Some(Expr::Bool(true))),
        _ => Ok(Some(Expr::Bool(false))),
    }
}

fn builtin_isnumber(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Number? needs exactly one argument");
    }
    match &values[0] {
        Expr::Float(_) | Expr::Integer(_) => Ok(Some(Expr::Bool(true))),
        _ => Ok(Some(Expr::Bool(false))),
    }
}

fn builtin_issymbol(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Symbol? needs exactly one argument");
    }
    match &values[0] {
        Expr::Symbol(_) => Ok(Some(Expr::Bool(true))),
        _ => Ok(Some(Expr::Bool(false))),
    }
}

fn builtin_cons(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 2 {
        return Err("Cons needs exactly two argument");
    }
    let car = values.remove(0);
    let cdr = values.remove(0);
    Ok(Some(Expr::Cons(Box::new(Cons { car, cdr }))))
}

fn builtin_car(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Car needs exactly one argument");
    }
    match values.remove(0) {
        Expr::Cons(p) => Ok(Some(p.car)),
        _ => Err("Car needs a pair as argument"),
    }
}

fn builtin_cdr(mut values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    if values.len() != 1 {
        return Err("Cdr needs exactly one argument");
    }
    match values.remove(0) {
        Expr::Cons(p) => Ok(Some(p.cdr)),
        _ => Err("Cdr needs a pair as argument"),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentNode {
    data: HashMap<String, Expr>,
    parent: Option<EnvironmentLink>,
}

type EnvironmentLink = Rc<RefCell<EnvironmentNode>>;

impl EnvironmentNode {
    pub fn set(&mut self, key: String, value: Expr) -> Option<Expr> {
        self.data.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<Expr> {
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

type BuiltInFnType = fn(Vec<Expr>) -> Result<Option<Expr>, &'static str>;

impl Environment {
    pub fn empty() -> Environment {
        let node = EnvironmentNode {
            data: HashMap::new(),
            parent: None,
        };
        Environment {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn child(&self) -> Environment {
        let node = EnvironmentNode {
            data: HashMap::new(),
            parent: Some(self.head.clone()),
        };
        Environment {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn standard() -> Environment {
        let node = EnvironmentNode {
            data: HashMap::new(),
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
            ("cons", builtin_cons as BuiltInFnType),
            ("car", builtin_car as BuiltInFnType),
            ("cdr", builtin_cdr as BuiltInFnType),
        ];
        for (s, f) in to_set {
            env.set(
                s.to_string(),
                Expr::Procedure(Procedure::BuiltIn(BuiltInProcedure { func: f })),
            );
        }
        env
    }

    pub fn set(&mut self, key: String, value: Expr) -> Option<Expr> {
        self.head.borrow_mut().set(key, value)
    }

    pub fn get(&self, key: &str) -> Option<Expr> {
        self.head.borrow().get(key)
    }

    pub fn evaluate(&mut self, expr: Expr) -> Result<Option<Expr>, &'static str> {
        match expr {
            Expr::Integer(_) => Ok(Some(expr)),
            Expr::Float(_) => Ok(Some(expr)),
            Expr::Str(_) => Ok(Some(expr)),
            Expr::Bool(_) => Ok(Some(expr)),
            Expr::Cons(p) => self.evaluate_pair(*p),
            Expr::Symbol(s) => match self.get(&s) {
                Some(value) => Ok(Some(value)),
                None => Err("Undefined symbol"),
            },
            _ => Err("Cannot evaluate expression"),
        }
    }

    fn evaluate_pair(&mut self, pair: Cons) -> Result<Option<Expr>, &'static str> {
        let args = pair.cdr.into_vec()?;

        match pair.car {
            Expr::Keyword(Keyword::Lambda) => self.evaluate_lambda(args),
            Expr::Keyword(Keyword::Quote) => self.evaluate_quote(args),
            Expr::Keyword(Keyword::Define) => self.evaluate_define(args),
            Expr::Keyword(Keyword::If) => self.evaluate_if(args),
            Expr::Keyword(Keyword::Cond) => self.evaluate_cond(args),
            Expr::Keyword(Keyword::When) => self.evaluate_when(args),
            Expr::Keyword(Keyword::Unless) => self.evaluate_unless(args),
            Expr::Keyword(Keyword::Let) => self.evaluate_let(args),
            Expr::Keyword(Keyword::Set) => self.evaluate_set(args),
            Expr::Keyword(Keyword::Begin) => self.evaluate_begin(args),
            Expr::Keyword(Keyword::And) => self.evaluate_and(args),
            Expr::Keyword(Keyword::Or) => self.evaluate_or(args),
            _ => match self.evaluate(pair.car) {
                Ok(Some(Expr::Procedure(proc))) => {
                    let call_args = self.evaluate_non_none_args(args)?;
                    proc.call(call_args)
                }
                _ => Err("Not a procedure call"),
            },
        }
    }

    fn evaluate_non_none_args(&mut self, exprs: Vec<Expr>) -> Result<Vec<Expr>, &'static str> {
        let mut res = Vec::with_capacity(exprs.len());
        for expr in exprs {
            match self.evaluate(expr)? {
                Some(value) => {
                    res.push(value);
                }
                None => return Err("Some argument has no value"),
            }
        }
        Ok(res)
    }

    fn evaluate_lambda(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.is_empty() {
            return Err("Lambda needs at least one argument");
        }
        let lambda_args = args.remove(0);
        match lambda_args {
            Expr::Cons(_) => {
                let mut params = Vec::new();
                for expr in lambda_args.into_vec()? {
                    match expr {
                        Expr::Symbol(s) => params.push(s),
                        _ => return Err("Not a symbol"),
                    }
                }
                let proc = UserDefinedProcedure {
                    params,
                    body: args,
                    env: self.clone(),
                };
                Ok(Some(Expr::Procedure(Procedure::UserDefined(proc))))
            }
            _ => Err("First argument to lambda must be a list of symbols"),
        }
    }

    fn evaluate_quote(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() != 1 {
            return Err("Quote needs exactly one argument");
        }
        Ok(Some(args.remove(0)))
    }

    fn evaluate_define(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() < 2 {
            return Err("Define needs at least two arguments");
        }
        let form = args.remove(0);
        match form {
            Expr::Symbol(key) => {
                if args.len() != 1 {
                    return Err("Define with a symbol needs two arguments");
                }
                if let Some(value) = self.evaluate(args.remove(0))? {
                    self.set(key, value);
                }
                Ok(None)
            }
            Expr::Cons(_) => {
                let v = form.into_vec()?;
                let mut ids = Vec::with_capacity(v.len() - 1);
                for expr in v {
                    match expr {
                        Expr::Symbol(s) => ids.push(s),
                        _ => return Err("Not a symbol"),
                    }
                }
                let key = ids.remove(0);
                let proc = UserDefinedProcedure {
                    params: ids,
                    body: args,
                    env: self.clone(),
                };
                self.set(key, Expr::Procedure(Procedure::UserDefined(proc)));
                Ok(None)
            }
            _ => Err("Define needs a symbol or a list as first argument"),
        }
    }

    fn evaluate_if(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() < 2 || args.len() > 3 {
            return Err("If accepts two or three arguments");
        }
        match self.evaluate(args.remove(0))? {
            Some(Expr::Bool(true)) => self.evaluate(args.remove(0)),
            Some(Expr::Bool(false)) => {
                if args.len() == 1 {
                    Ok(None)
                } else {
                    self.evaluate(args.remove(1))
                }
            }
            _ => Err("First argument to if did not evaluate to a boolean"),
        }
    }

    fn evaluate_cond(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        for clause in args {
            match clause {
                Expr::Cons(p) => {
                    let seq = p.cdr.into_vec()?;
                    if let Expr::Symbol(s) = &p.car {
                        if s == "else" {
                            return self.evaluate_begin(seq);
                        }
                    }
                    match self.evaluate(p.car)? {
                        Some(Expr::Bool(true)) => return self.evaluate_begin(seq),
                        Some(Expr::Bool(false)) => continue,
                        _ => return Err("Clause did not evaluate to a boolean"),
                    }
                }
                _ => return Err("Not a list"),
            }
        }
        Ok(None)
    }

    fn evaluate_when(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.is_empty() {
            return Err("When needs at least one argument");
        }
        let _ = match self.evaluate(args.remove(0))? {
            Some(Expr::Bool(true)) => self.evaluate_begin(args),
            Some(Expr::Bool(false)) => Ok(None),
            _ => Err("First argument to when did not evaluate to a boolean"),
        };
        Ok(None)
    }

    fn evaluate_unless(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.is_empty() {
            return Err("Unless needs at least one argument");
        }
        let _ = match self.evaluate(args.remove(0))? {
            Some(Expr::Bool(true)) => Ok(None),
            Some(Expr::Bool(false)) => self.evaluate_begin(args),
            _ => Err("First argument to unless did not evaluate to a boolean"),
        };
        Ok(None)
    }

    fn evaluate_let(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.is_empty() {
            return Err("Let needs at least one argument");
        }
        let bindings = args.remove(0);
        let mut child = self.child();
        for expr in bindings.into_vec()? {
            match expr {
                Expr::Cons(p) => match (p.car, p.cdr) {
                    (Expr::Symbol(s), Expr::Cons(cdr)) => {
                        if cdr.cdr != Expr::Null {
                            return Err("Not a 2-list");
                        }
                        if let Some(vv) = child.evaluate(cdr.car)? {
                            child.set(s, vv);
                        } else {
                            return Err("No value to bind");
                        }
                    }
                    _ => return Err("Not a symbol"),
                },
                _ => return Err("Not a 2-list"),
            }
        }
        let mut out = None;
        for expr in args.into_iter() {
            out = child.evaluate(expr)?;
        }
        Ok(out)
    }

    fn evaluate_set(&mut self, mut args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() != 2 {
            return Err("Set! needs exactly two arguments");
        }
        let what = args.remove(0);
        match what {
            Expr::Symbol(s) => {
                if self.get(&s) == None {
                    return Err("Symbol is not bound");
                }
                match self.evaluate(args.remove(0))? {
                    Some(val) => {
                        self.set(s, val);
                        Ok(None)
                    }
                    None => Err("No value to set"),
                }
            }
            _ => Err("First argument to set! must be a symbol"),
        }
    }

    fn evaluate_begin(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        let mut out = None;
        for expr in args.into_iter() {
            out = self.evaluate(expr)?;
        }
        Ok(out)
    }

    fn evaluate_and(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Some(Expr::Bool(true))) => continue,
                Ok(Some(Expr::Bool(false))) => return Ok(Some(Expr::Bool(false))),
                _ => return Err("Cannot \"and\" type"),
            }
        }
        Ok(Some(Expr::Bool(true)))
    }

    fn evaluate_or(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        for expr in args {
            match self.evaluate(expr) {
                Ok(Some(Expr::Bool(true))) => return Ok(Some(Expr::Bool(true))),
                Ok(Some(Expr::Bool(false))) => continue,
                _ => return Err("Cannot \"or\" type"),
            }
        }
        Ok(Some(Expr::Bool(false)))
    }
}

trait Callable {
    fn call(&self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: Environment,
}

impl Callable for UserDefinedProcedure {
    fn call(&self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() != self.params.len() {
            return Err("Incorrect number of arguments");
        }
        let mut local_env = self.env.child();
        for (param, arg) in zip(&self.params, args) {
            local_env.set(param.to_string(), arg);
        }
        let mut out = None;
        for expr in self.body.iter() {
            out = local_env.evaluate(expr.clone())?;
        }
        Ok(out)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltInProcedure {
    func: fn(Vec<Expr>) -> Result<Option<Expr>, &'static str>,
}

impl Callable for BuiltInProcedure {
    fn call(&self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        match self {
            Procedure::UserDefined(proc) => proc.call(args),
            Procedure::BuiltIn(proc) => proc.call(args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            Expr::from_vec(vec![
                Expr::Symbol("square".to_string()),
                Expr::Symbol("x".to_string()),
            ]),
            Expr::from_vec(vec![
                Expr::Symbol("*".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("x".to_string()),
            ]),
        ])];
        let expressions: Vec<Expr> = parse_code(code).unwrap();
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
            Expr::from_vec(vec![
                Expr::Symbol("f".to_string()),
                Expr::Symbol("x".to_string()),
            ]),
            Expr::Str("hello, world!".to_string()),
            Expr::from_vec(vec![
                Expr::Keyword(Keyword::Quote),
                Expr::Symbol("a".to_string()),
            ]),
            Expr::from_vec(vec![
                Expr::Symbol("*".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("2".to_string()),
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
        base.set("a".to_string(), Expr::Integer(42));

        let mut child = base.child();

        child.set("a".to_string(), Expr::Str("hello".to_string()));
        child.set("b".to_string(), Expr::Str("world".to_string()));

        assert_eq!(base.get("a"), Some(Expr::Integer(42)));
        assert_eq!(base.get("b"), None);
        assert_eq!(child.get("a"), Some(Expr::Str("hello".to_string())));
        assert_eq!(child.get("b"), Some(Expr::Str("world".to_string())));
    }

    #[test]
    fn test_builtin_add() {
        let values = vec![Expr::Integer(10), Expr::Float(42.0)];

        assert_eq!(builtin_add(values), Ok(Some(Expr::Float(52.0))));

        let values = vec![Expr::Float(42.0), Expr::Integer(13)];

        assert_eq!(builtin_add(values), Ok(Some(Expr::Float(55.0))));

        let values = vec![
            Expr::Float(42.0),
            Expr::Integer(13),
            Expr::Str("hey, hey".to_string()),
        ];

        assert_eq!(builtin_add(values), Err("Cannot add types"));
    }

    fn validate(cases: Vec<(&str, Option<Expr>)>) {
        let mut env = Environment::standard().child();
        for (code, val) in cases {
            let expr = parse_code(code).unwrap().remove(0);
            assert_eq!(env.evaluate(expr), Ok(val));
        }
    }

    #[test]
    fn test_evaluate() {
        let cases = vec![
            ("(define a 42)", None),
            ("42.42", Some(Expr::Float(42.42))),
            ("#t", Some(Expr::Bool(true))),
            ("#f", Some(Expr::Bool(false))),
            (
                "\"hello, world!\"",
                Some(Expr::Str("hello, world!".to_string())),
            ),
            ("a", Some(Expr::Integer(42))),
            ("(+ 3 2)", Some(Expr::Integer(5))),
            ("(* 3 2)", Some(Expr::Integer(6))),
            ("(+ 3 2.0)", Some(Expr::Float(5.0))),
            ("(* 3.0 2)", Some(Expr::Float(6.0))),
            ("(- 10 2 3)", Some(Expr::Integer(5))),
            ("(/ 24 3 2)", Some(Expr::Float(4.0))),
            ("(abs -5)", Some(Expr::Integer(5))),
            ("(abs 5)", Some(Expr::Integer(5))),
            ("(abs -5.0)", Some(Expr::Float(5.0))),
            ("(abs 5.0)", Some(Expr::Float(5.0))),
            ("(< 1 2 3)", Some(Expr::Bool(true))),
            ("(< 1 3 2)", Some(Expr::Bool(false))),
            ("(<= 1 1 1)", Some(Expr::Bool(true))),
            ("(<= 1 0 1)", Some(Expr::Bool(false))),
            ("(> 3 2 1)", Some(Expr::Bool(true))),
            ("(> 1 3 2)", Some(Expr::Bool(false))),
            ("(>= 1 1 1)", Some(Expr::Bool(true))),
            ("(>= 1 1 2)", Some(Expr::Bool(false))),
            (
                "(cons 1 2)",
                Some(Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
                }))),
            ),
            (
                "'(1 . 2)",
                Some(Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
                }))),
            ),
            (
                "'(1 2 . 3)",
                Some(Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Cons(Box::new(Cons {
                        car: Expr::Integer(2),
                        cdr: Expr::Integer(3),
                    })),
                }))),
            ),
            (
                "'(1 . (2 . 3))",
                Some(Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Cons(Box::new(Cons {
                        car: Expr::Integer(2),
                        cdr: Expr::Integer(3),
                    })),
                }))),
            ),
            (
                "(list 1 2 3)",
                Some(Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                ])),
            ),
            (
                "'(1 2 3)",
                Some(Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                ])),
            ),
            (
                "'(1 . (2 . (3 . ())))",
                Some(Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                ])),
            ),
            ("(define a 13)", None),
            ("(+ 8 a)", Some(Expr::Integer(21))),
            ("(define f (lambda (a b) (+ (* 3 a) b)))", None),
            ("(f 7 a)", Some(Expr::Integer(34))),
            ("(f 7.0 a)", Some(Expr::Float(34.0))),
            ("(if (> 3 7) (- 3 7) (- 7 3))", Some(Expr::Integer(4))),
            ("(if (< 3 7) (- 3 7) (- 7 3))", Some(Expr::Integer(-4))),
            ("(begin (+ 4 7) (- 5 2) (* 7 3))", Some(Expr::Integer(21))),
            (
                "(let ((a 14) (b 7)) (+ a b) (- a b))",
                Some(Expr::Integer(7)),
            ),
            ("(length '())", Some(Expr::Integer(0))),
            ("(length '(4 5 6))", Some(Expr::Integer(3))),
            (
                "(append '(1 2) '(3) '() '(4))",
                Some(Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ])),
            ),
            ("(set! a -1)", None),
            ("a", Some(Expr::Integer(-1))),
        ];
        validate(cases);
    }

    #[test]
    fn test_and_or_not() {
        let cases = vec![
            ("(and)", Some(Expr::Bool(true))),
            ("(and #t #t #f)", Some(Expr::Bool(false))),
            ("(and #t #t #t)", Some(Expr::Bool(true))),
            ("(or)", Some(Expr::Bool(false))),
            ("(or #f #f #f)", Some(Expr::Bool(false))),
            ("(or #f #t #f)", Some(Expr::Bool(true))),
            ("(not #t)", Some(Expr::Bool(false))),
            ("(not #f)", Some(Expr::Bool(true))),
        ];
        validate(cases);
    }

    #[test]
    fn test_quote() {
        let cases = vec![
            ("(quote ())", Some(Expr::from_vec(vec![]))),
            (
                "(quote (#t #f))",
                Some(Expr::from_vec(vec![Expr::Bool(true), Expr::Bool(false)])),
            ),
            ("(quote 42.0)", Some(Expr::Float(42.0))),
            (
                "(quote (* 3 4))",
                Some(Expr::from_vec(vec![
                    Expr::Symbol("*".to_string()),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ])),
            ),
            ("'()", Some(Expr::from_vec(vec![]))),
            (
                "'(#t #f)",
                Some(Expr::from_vec(vec![Expr::Bool(true), Expr::Bool(false)])),
            ),
            ("'42.0", Some(Expr::Float(42.0))),
            (
                "'(* 3 4)",
                Some(Expr::from_vec(vec![
                    Expr::Symbol("*".to_string()),
                    Expr::Integer(3),
                    Expr::Integer(4),
                ])),
            ),
        ];
        validate(cases);
    }

    #[test]
    fn test_predicates() {
        let cases = vec![
            ("(list? '())", Some(Expr::Bool(true))),
            ("(list? '(1 2 3))", Some(Expr::Bool(true))),
            ("(list? (list 1 2 3))", Some(Expr::Bool(true))),
            ("(list? 42)", Some(Expr::Bool(false))),
            ("(list? (cons 17 18))", Some(Expr::Bool(false))),
            ("(pair? '())", Some(Expr::Bool(false))),
            ("(pair? '(1 2 3))", Some(Expr::Bool(true))),
            ("(pair? (list 1 2 3))", Some(Expr::Bool(true))),
            ("(pair? 42)", Some(Expr::Bool(false))),
            ("(pair? (cons 17 18))", Some(Expr::Bool(true))),
            ("(null? 0)", Some(Expr::Bool(false))),
            ("(null? #f)", Some(Expr::Bool(false))),
            ("(null? '())", Some(Expr::Bool(true))),
            ("(null? '(1))", Some(Expr::Bool(false))),
            ("(number? 42)", Some(Expr::Bool(true))),
            ("(number? 42.0)", Some(Expr::Bool(true))),
            ("(number? \"hello\")", Some(Expr::Bool(false))),
            ("(number? 'a)", Some(Expr::Bool(false))),
            ("(number? '())", Some(Expr::Bool(false))),
            ("(number? '(1 2 3))", Some(Expr::Bool(false))),
            ("(number? #t)", Some(Expr::Bool(false))),
            ("(symbol? 42)", Some(Expr::Bool(false))),
            ("(symbol? 42.0)", Some(Expr::Bool(false))),
            ("(symbol? \"hello\")", Some(Expr::Bool(false))),
            ("(symbol? 'a)", Some(Expr::Bool(true))),
            ("(symbol? '())", Some(Expr::Bool(false))),
            ("(symbol? '(1 2 3))", Some(Expr::Bool(false))),
            ("(symbol? #t)", Some(Expr::Bool(false))),
        ];
        validate(cases);
    }

    #[test]
    fn test_apply() {
        let cases = vec![
            ("(apply + '(3 4))", Some(Expr::Integer(7))),
            ("(apply * (list -5 4))", Some(Expr::Integer(-20))),
        ];
        validate(cases);
    }

    #[test]
    fn test_multistep_function_1() {
        let cases = vec![
            ("(define f (lambda (x) (define a 3) (* a x)))", None),
            ("(f 4)", Some(Expr::Integer(12))),
        ];
        validate(cases);
    }

    #[test]
    fn test_multistep_function_2() {
        let cases = vec![
            ("(define (f x) (define a 3) (* a x))", None),
            ("(f 4)", Some(Expr::Integer(12))),
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
                None,
            ),
            ("(fact 11)", Some(Expr::Integer(39916800))),
        ];
        validate(cases);
    }

    #[test]
    fn test_fib() {
        let cases = vec![
            (
                "(define (fib n) (
                    if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))",
                None,
            ),
            ("(fib 20)", Some(Expr::Integer(6765))),
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
                None,
            ),
            ("(sqrt 2)", Some(Expr::Float(1.4142156862745097))),
        ];
        validate(cases);
    }

    #[test]
    fn test_sqrt_newton_2() {
        let cases = vec![
            ("(define (square x) (* x x))", None),
            ("(define (average x y) (/ (+ x y) 2))", None),
            (
                "(define (good-enough? guess x)
                    (< (abs (- (square guess) x)) 0.001))",
                None,
            ),
            (
                "(define (improve guess x)
                    (average guess (/ x guess)))",
                None,
            ),
            (
                "(define (sqrt-iter guess x)
                    (if (good-enough? guess x)
                        guess
                        (sqrt-iter (improve guess x) x)))",
                None,
            ),
            (
                "(define (sqrt x)
                    (sqrt-iter 1.0 x))",
                None,
            ),
            ("(sqrt 2)", Some(Expr::Float(1.4142156862745097))),
        ];
        validate(cases);
    }

    #[test]
    fn test_define_let_lambda() {
        let cases = vec![
            ("(define f (let ((a 3)) (lambda (x) (* a x))))", None),
            ("(f 5)", Some(Expr::Integer(15))),
            ("(f -4)", Some(Expr::Integer(-12))),
            ("(define a 1)", None),
            ("(f 5)", Some(Expr::Integer(15))),
            ("(f -4)", Some(Expr::Integer(-12))),
        ];
        validate(cases);
    }

    #[test]
    fn test_define_with_unbound() {
        let cases = vec![
            ("(define (f x) (+ a x))", None),
            ("(define a 3)", None),
            ("(f 5)", Some(Expr::Integer(8))),
            ("(define a -17)", None),
            ("(f 3)", Some(Expr::Integer(-14))),
        ];
        validate(cases);
    }

    #[test]
    fn test_higher_order() {
        let cases = vec![
            ("(define (make-adder n) (lambda (x) (+ x n)))", None),
            ("((make-adder 3) 7)", Some(Expr::Integer(10))),
        ];
        validate(cases);
    }

    #[test]
    fn test_nested_function() {
        let cases = vec![
            (
                "(define (outer-func x) (define (inner-func y) (+ x y)) (inner-func 10))",
                None,
            ),
            ("(outer-func 5)", Some(Expr::Integer(15))),
        ];
        validate(cases);
    }

    #[test]
    fn test_function_scope() {
        let cases = vec![
            (
                "(define (outer x) (define y (+ x 1)) (lambda (z) (+ y z)))",
                None,
            ),
            ("((outer 3) 2)", Some(Expr::Integer(6))),
        ];
        validate(cases);
    }

    #[test]
    fn test_cons_car_cdr() {
        let cases = vec![
            ("(cons 1 '())", Some(Expr::from_vec(vec![Expr::Integer(1)]))),
            (
                "(cons 1 '(2 3))",
                Some(Expr::from_vec(vec![
                    Expr::Integer(1),
                    Expr::Integer(2),
                    Expr::Integer(3),
                ])),
            ),
            ("(car '(1 2 3))", Some(Expr::Integer(1))),
            (
                "(cdr '(1 2 3))",
                Some(Expr::from_vec(vec![Expr::Integer(2), Expr::Integer(3)])),
            ),
            (
                "(cons 1 2)",
                Some(Expr::Cons(Box::new(Cons {
                    car: Expr::Integer(1),
                    cdr: Expr::Integer(2),
                }))),
            ),
        ];
        validate(cases);
    }

    #[test]
    fn test_cond() {
        let cases = vec![
            (
                "(define (f x) (cond ((< x 0) 'negative) ((> x 0) 'positive) (else 'zero)))",
                None,
            ),
            ("(f -1)", Some(Expr::Symbol("negative".to_string()))),
            ("(f 0)", Some(Expr::Symbol("zero".to_string()))),
            ("(f 1)", Some(Expr::Symbol("positive".to_string()))),
        ];
        validate(cases);
    }

    #[test]
    fn test_when_unless() {
        let cases = vec![
            ("(define a 42)", None),
            ("(when (> 0 1) (set! a 43))", None),
            ("a", Some(Expr::Integer(42))),
            ("(when (> 1 0) (set! a 44))", None),
            ("a", Some(Expr::Integer(44))),
            ("(unless (> 0 1) (set! a 43))", None),
            ("a", Some(Expr::Integer(43))),
            ("(unless (> 1 0) (set! a 42))", None),
            ("a", Some(Expr::Integer(43))),
        ];
        validate(cases);
    }
}
