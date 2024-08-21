use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::{zip, Peekable};
use std::rc::Rc;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Quote,
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
    Let,
    Begin,
    // TODO add more special forms here
}

impl Keyword {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "lambda" => Some(Keyword::Lambda),
            "quote" => Some(Keyword::Quote),
            "define" => Some(Keyword::Define),
            "if" => Some(Keyword::If),
            "let" => Some(Keyword::Let),
            "begin" => Some(Keyword::Begin),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Procedure(Procedure),
    List(Vec<Expr>),
    Keyword(Keyword),
    Symbol(String),
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
            Some(Token::RParen) => Err("Unexpected closing parenthesis"),
            None => Err("Unexpected end of input"),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, &'static str> {
        let mut expressions = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::RParen => {
                    self.tokens.next();
                    return Ok(Expr::List(expressions));
                }
                _ => {
                    expressions.push(self.parse_expression()?);
                }
            }
        }
        Err("Unterminated list")
    }

    fn parse_quote(&mut self) -> Result<Expr, &'static str> {
        Ok(Expr::List(vec![
            Expr::Keyword(Keyword::Quote),
            self.parse_expression()?,
        ]))
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
        } else if let Some(keyword) = Keyword::from_str(&s) {
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

impl Expr {
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
    if values.len() > 1 {
        return Err("abs needs exactly one argument");
    }
    let res_value = match values.first() {
        Some(value) => match value {
            Expr::Integer(n) => Expr::Integer(if *n >= 0 { *n } else { -*n }),
            Expr::Float(n) => Expr::Float(if *n >= 0.0 { *n } else { -*n }),
            _ => return Err("abs needs a number argument"),
        },
        _ => return Err("abs needs exactly one argument"),
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

fn builtin_list(values: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
    Ok(Some(Expr::List(values)))
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
            ("list", builtin_list as BuiltInFnType),
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

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Option<Expr>, &'static str> {
        match expr {
            Expr::Integer(_) => Ok(Some(expr.clone())),
            Expr::Float(_) => Ok(Some(expr.clone())),
            Expr::Str(_) => Ok(Some(expr.clone())),
            Expr::Bool(_) => Ok(Some(expr.clone())),
            Expr::List(v) => self.evaluate_nonatomic(v),
            Expr::Symbol(s) => match self.get(s) {
                Some(value) => Ok(Some(value.clone())),
                None => Err("Undefined symbol"),
            },
            _ => Err("Cannot evaluate expression"),
        }
    }

    fn evaluate_non_none_args(&mut self, exprs: &[Expr]) -> Result<Vec<Expr>, &'static str> {
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

    fn evaluate_nonatomic(&mut self, exprs: &[Expr]) -> Result<Option<Expr>, &'static str> {
        if exprs.is_empty() {
            return Err("Cannot evaluate empty, non-atomic expressions");
        }
        match &exprs[0] {
            Expr::Keyword(Keyword::Lambda) => self.evaluate_lambda(&exprs[1..]),
            Expr::Keyword(Keyword::Quote) => self.evaluate_quote(&exprs[1..]),
            Expr::Keyword(Keyword::Define) => self.evaluate_define(&exprs[1..]),
            Expr::Keyword(Keyword::If) => self.evaluate_if(&exprs[1..]),
            Expr::Keyword(Keyword::Let) => self.evaluate_let(&exprs[1..]),
            Expr::Keyword(Keyword::Begin) => self.evaluate_begin(&exprs[1..]),
            _ => match self.evaluate(&exprs[0]) {
                Ok(Some(Expr::Procedure(mut p))) => {
                    let args = self.evaluate_non_none_args(&exprs[1..])?;
                    p.call(args)
                }
                _ => Err("Not a procedure call"),
            },
        }
    }

    fn evaluate_lambda(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        match &args[0] {
            Expr::List(v) => {
                let mut ids = Vec::with_capacity(v.len());
                for expr in v {
                    match expr {
                        Expr::Symbol(s) => ids.push(s.clone()),
                        _ => return Err("Not a symbol"),
                    }
                }
                let proc = UserDefinedProcedure::new(ids, args[1..].to_vec(), self.clone());
                Ok(Some(Expr::Procedure(Procedure::UserDefined(proc))))
            }
            _ => Err("First argument to lambda must be a list of symbols"),
        }
    }

    fn evaluate_quote(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        if args.len() != 1 {
            return Err("Quote needs exactly one arguments");
        }
        Ok(Some(args[0].clone()))
    }

    fn evaluate_define(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        if args.len() != 2 {
            return Err("Define needs exactly two arguments");
        }
        match &args[0] {
            Expr::Symbol(key) => {
                if let Some(value) = self.evaluate(&args[1])? {
                    self.set(key.to_string(), value);
                }
                Ok(None)
            }
            _ => Err("Define needs identifier as first argument"),
        }
    }

    fn evaluate_if(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        if args.len() != 3 {
            return Err("If needs exactly three arguments");
        }
        match self.evaluate(&args[0])? {
            Some(Expr::Bool(true)) => self.evaluate(&args[1]),
            Some(Expr::Bool(false)) => self.evaluate(&args[2]),
            _ => Err("First argument to if did not evaluate to a boolean"),
        }
    }

    fn evaluate_let(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        if args.is_empty() {
            return Err("Let needs at least one argument");
        }
        if let Expr::List(v) = &args[0] {
            let mut child = self.child();
            for expr in v {
                match expr {
                    Expr::List(p) => {
                        if p.len() != 2 {
                            return Err("Not a 2-list");
                        }
                        match &p[0] {
                            Expr::Symbol(s) => {
                                if let Some(vv) = child.evaluate(&p[1])? {
                                    child.set(s.to_string(), vv);
                                }
                            }
                            _ => return Err("Not a symbol"),
                        }
                    }
                    _ => return Err("Not a list"),
                }
            }
            for expr in &args[1..args.len() - 1] {
                let _ = child.evaluate(expr);
            }
            match args.last() {
                Some(expr) => return child.evaluate(expr),
                None => return Ok(None),
            }
        }
        Err("First argument to let must be a list")
    }

    fn evaluate_begin(&mut self, args: &[Expr]) -> Result<Option<Expr>, &'static str> {
        for expr in &args[..args.len() - 1] {
            let _ = self.evaluate(expr);
        }
        match args.last() {
            Some(expr) => self.evaluate(expr),
            None => Ok(None),
        }
    }
}

trait Callable {
    fn call(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: Environment,
}

impl UserDefinedProcedure {
    pub fn new(params: Vec<String>, body: Vec<Expr>, env: Environment) -> Self {
        Self { params, body, env }
    }
}

impl Callable for UserDefinedProcedure {
    fn call(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        if args.len() != self.params.len() {
            return Err("Incorrect number of arguments");
        }
        let mut local_env = self.env.child();
        for (param, arg) in zip(&self.params, args) {
            local_env.set(param.to_string(), arg);
        }
        for expr in &self.body[..self.body.len() - 1] {
            let _ = local_env.evaluate(expr);
        }
        match self.body.last() {
            Some(expr) => local_env.evaluate(expr),
            None => Ok(None),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltInProcedure {
    func: fn(Vec<Expr>) -> Result<Option<Expr>, &'static str>,
}

impl Callable for BuiltInProcedure {
    fn call(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&mut self, args: Vec<Expr>) -> Result<Option<Expr>, &'static str> {
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
        let expected = vec![Expr::List(vec![
            Expr::Keyword(Keyword::Define),
            Expr::List(vec![
                Expr::Symbol("square".to_string()),
                Expr::Symbol("x".to_string()),
            ]),
            Expr::List(vec![
                Expr::Symbol("*".to_string()),
                Expr::Symbol("x".to_string()),
                Expr::Symbol("x".to_string()),
            ]),
        ])];
        let expressions: Vec<Expr> = parse_code(code).unwrap();
        assert_eq!(expressions, expected);
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
            let expr = &parse_code(code).unwrap()[0];
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
                "(list 1 2 3)",
                Some(Expr::List(vec![
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

    // #[test]
    // fn test_fib() {
    //     let cases = vec![
    //         ("(define (fib n) (
    //             if (< n 2)
    //             n
    //             (+ (fib (- n 1)) (fib (- n 2)))))", None),
    //         ("(fib 20)", Some(Expr::Integer(6765))),
    //     ];
    //     validate(cases);
    // }
}
