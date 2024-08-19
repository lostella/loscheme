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
    // TODO add more special forms here
}

impl Keyword {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "lambda" => Some(Keyword::Lambda),
            "quote" => Some(Keyword::Quote),
            "define" => Some(Keyword::Define),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Literal {
    fn to_value(&self) -> Value {
        match self {
            Literal::Integer(x) => Value::Integer(*x),
            Literal::Float(x) => Value::Float(*x),
            Literal::Str(x) => Value::Str(x.clone()),
            Literal::Bool(x) => Value::Bool(*x),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
    List(Vec<Expression>),
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

    pub fn parse(&mut self) -> Result<Vec<Expression>, &'static str> {
        let mut expressions = Vec::new();
        while self.tokens.peek().is_some() {
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expression, &'static str> {
        match self.tokens.next() {
            Some(Token::LParen) => self.parse_list(),
            Some(Token::Quote) => self.parse_quote(),
            Some(Token::Atom(s)) => self.parse_atom(s),
            Some(Token::RParen) => Err("Unexpected closing parenthesis"),
            None => Err("Unexpected end of input"),
        }
    }

    fn parse_list(&mut self) -> Result<Expression, &'static str> {
        let mut expressions = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::RParen => {
                    self.tokens.next();
                    return Ok(Expression::List(expressions));
                }
                _ => {
                    expressions.push(self.parse_expression()?);
                }
            }
        }
        Err("Unterminated list")
    }

    fn parse_quote(&mut self) -> Result<Expression, &'static str> {
        Ok(Expression::List(vec![
            Expression::Keyword(Keyword::Quote),
            self.parse_expression()?,
        ]))
    }

    fn parse_atom(&self, s: String) -> Result<Expression, &'static str> {
        if let Ok(int) = s.parse::<i64>() {
            Ok(Expression::Literal(Literal::Integer(int)))
        } else if let Ok(float) = s.parse::<f64>() {
            Ok(Expression::Literal(Literal::Float(float)))
        } else if s == "#t" {
            Ok(Expression::Literal(Literal::Bool(true)))
        } else if s == "#f" {
            Ok(Expression::Literal(Literal::Bool(false)))
        } else if s.starts_with('"') && s.ends_with('"') {
            Ok(Expression::Literal(Literal::Str(
                s[1..s.len() - 1].to_string(),
            )))
        } else if let Some(keyword) = Keyword::from_str(&s) {
            Ok(Expression::Keyword(keyword))
        } else {
            Ok(Expression::Identifier(s))
        }
    }
}

pub fn parse_code(code: &str) -> Result<Vec<Expression>, &'static str> {
    let tokenizer = Tokenizer::new(code);
    let mut parser = Parser::new(tokenizer);
    parser.parse()
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Procedure(Procedure),
    List(Vec<Value>),
    Expression(Expression),
}

impl Value {
    fn add_to(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a + *b as f64)),
            _ => Err("Cannot add types"),
        }
    }

    fn mul_by(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a * *b as f64)),
            _ => Err("Cannot multiply types"),
        }
    }
}

fn add_numbers(values: Vec<Value>) -> Result<Option<Value>, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(0), |acc, x| acc.add_to(&x))?;
    Ok(Some(res))
}

fn mul_numbers(values: Vec<Value>) -> Result<Option<Value>, &'static str> {
    let res = values
        .into_iter()
        .try_fold(Value::Integer(1), |acc, x| acc.mul_by(&x))?;
    Ok(Some(res))
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentNode {
    data: HashMap<String, Value>,
    parent: Option<EnvironmentLink>,
}

type EnvironmentLink = Rc<RefCell<EnvironmentNode>>;

impl EnvironmentNode {
    pub fn set(node: &mut EnvironmentNode, key: String, value: Value) -> Option<Value> {
        node.data.insert(key, value)
    }

    pub fn get(node: &EnvironmentNode, key: &str) -> Option<Value> {
        match node.data.get(key) {
            Some(value) => Some(value.clone()),
            None => match &node.parent {
                Some(link) => Self::get(&link.borrow(), key),
                None => None,
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    head: EnvironmentLink,
}

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

    pub fn child(parent: &Environment) -> Environment {
        let node = EnvironmentNode {
            data: HashMap::new(),
            parent: Some(parent.head.clone()),
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
        Self::set(
            &mut env,
            "+".to_string(),
            Value::Procedure(Procedure::BuiltIn(BuiltInProcedure { func: add_numbers })),
        );
        Self::set(
            &mut env,
            "*".to_string(),
            Value::Procedure(Procedure::BuiltIn(BuiltInProcedure { func: mul_numbers })),
        );
        env
    }

    pub fn set(env: &mut Environment, key: String, value: Value) -> Option<Value> {
        EnvironmentNode::set(&mut env.head.borrow_mut(), key, value)
    }

    pub fn get(env: &Environment, key: &str) -> Option<Value> {
        EnvironmentNode::get(&env.head.borrow(), key)
    }

    pub fn evaluate_expr(
        env: &mut Environment,
        expr: &Expression,
    ) -> Result<Option<Value>, &'static str> {
        match expr {
            Expression::Identifier(s) => match Self::get(env, s) {
                Some(value) => Ok(Some(value.clone())),
                None => Err("Undefined identifier"),
            },
            Expression::Literal(l) => Ok(Some(l.to_value())),
            Expression::List(v) => Self::evaluate_nonatomic(env, v),
            _ => Ok(None),
        }
    }

    fn evaluate_non_none_args(
        env: &mut Environment,
        exprs: &[Expression],
    ) -> Result<Vec<Value>, &'static str> {
        let mut res = Vec::with_capacity(exprs.len());
        for expr in exprs {
            match Self::evaluate_expr(env, expr)? {
                Some(value) => {
                    res.push(value);
                }
                None => return Err("Some argument has no value"),
            }
        }
        Ok(res)
    }

    fn evaluate_nonatomic(
        env: &mut Environment,
        exprs: &[Expression],
    ) -> Result<Option<Value>, &'static str> {
        match exprs.len() {
            0 => Err("Cannot evaluate empty, non-atomic expressions"),
            _ => match &exprs[0] {
                Expression::Keyword(k) => Self::evaluate_special_form(env, k, &exprs[1..]),
                _ => match Self::evaluate_expr(env, &exprs[0]) {
                    Ok(Some(Value::Procedure(mut p))) => {
                        let args = Self::evaluate_non_none_args(env, &exprs[1..])?;
                        p.call(args)
                    }
                    _ => Err("Not a procedure call"),
                },
            },
        }
    }

    fn evaluate_special_form(
        env: &mut Environment,
        keyword: &Keyword,
        args: &[Expression],
    ) -> Result<Option<Value>, &'static str> {
        match keyword {
            Keyword::Lambda => match &args[0] {
                Expression::List(v) => {
                    let mut ids = Vec::with_capacity(v.len());
                    for expr in v {
                        match expr {
                            Expression::Identifier(s) => ids.push(s.clone()),
                            _ => return Err("Not an identifier"),
                        }
                    }
                    let proc = UserDefinedProcedure::new(ids, args[1..].to_vec(), env.clone());
                    Ok(Some(Value::Procedure(Procedure::UserDefined(proc))))
                }
                _ => Err("First argument to lambda must be a list of identifiers"),
            },
            Keyword::Quote => match args.len() {
                1 => Ok(Some(Value::Expression(args[0].clone()))),
                _ => Err("Must quote exactly one expression"),
            },
            Keyword::Define => {
                if args.len() != 2 {
                    return Err("Define needs exactly two arguments");
                }
                match &args[0] {
                    Expression::Identifier(key) => {
                        if let Some(value) = Self::evaluate_expr(env, &args[1])? {
                            Self::set(env, key.to_string(), value);
                        }
                        Ok(None)
                    }
                    _ => Err("Define needs identifier as first argument"),
                }
            }
        }
    }
}

trait Callable {
    fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &'static str>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UserDefinedProcedure {
    params: Vec<String>,
    body: Vec<Expression>,
    env: Environment,
}

impl UserDefinedProcedure {
    pub fn new(params: Vec<String>, body: Vec<Expression>, env: Environment) -> Self {
        Self { params, body, env }
    }
}

impl Callable for UserDefinedProcedure {
    fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &'static str> {
        if args.len() != self.params.len() {
            return Err("Incorrect number of arguments");
        }
        let mut local_env = Environment::child(&self.env);
        for (param, arg) in zip(&self.params, args) {
            Environment::set(&mut local_env, param.to_string(), arg);
        }
        for expr in &self.body[..self.body.len() - 1] {
            let _ = Environment::evaluate_expr(&mut local_env, expr);
        }
        match self.body.last() {
            Some(expr) => Environment::evaluate_expr(&mut local_env, expr),
            None => Ok(None),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltInProcedure {
    func: fn(Vec<Value>) -> Result<Option<Value>, &'static str>,
}

impl Callable for BuiltInProcedure {
    fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &'static str> {
        (self.func)(args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    UserDefined(UserDefinedProcedure),
    BuiltIn(BuiltInProcedure),
}

impl Callable for Procedure {
    fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &'static str> {
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
        let expected = vec![Expression::List(vec![
            Expression::Keyword(Keyword::Define),
            Expression::List(vec![
                Expression::Identifier("square".to_string()),
                Expression::Identifier("x".to_string()),
            ]),
            Expression::List(vec![
                Expression::Identifier("*".to_string()),
                Expression::Identifier("x".to_string()),
                Expression::Identifier("x".to_string()),
            ]),
        ])];
        let expressions: Vec<Expression> = parse_code(code).unwrap();
        assert_eq!(expressions, expected);
    }

    #[test]
    fn test_environment() {
        let mut base = Environment::empty();
        Environment::set(&mut base, "a".to_string(), Value::Integer(42));

        let mut child = Environment::child(&base);

        Environment::set(&mut child, "a".to_string(), Value::Str("hello".to_string()));
        Environment::set(&mut child, "b".to_string(), Value::Str("world".to_string()));

        assert_eq!(Environment::get(&base, "a"), Some(Value::Integer(42)));
        assert_eq!(Environment::get(&base, "b"), None);
        assert_eq!(
            Environment::get(&child, "a"),
            Some(Value::Str("hello".to_string()))
        );
        assert_eq!(
            Environment::get(&child, "b"),
            Some(Value::Str("world".to_string()))
        );
    }

    #[test]
    fn test_add_numbers() {
        let values = vec![Value::Integer(10), Value::Float(42.0)];

        assert_eq!(add_numbers(values), Ok(Some(Value::Float(52.0))));

        let values = vec![Value::Float(42.0), Value::Integer(13)];

        assert_eq!(add_numbers(values), Ok(Some(Value::Float(55.0))));

        let values = vec![
            Value::Float(42.0),
            Value::Integer(13),
            Value::Str("hey, hey".to_string()),
        ];

        assert_eq!(add_numbers(values), Err("Cannot add types"));
    }

    #[test]
    fn test_evaluate_expr() {
        let mut env = Environment::child(&Environment::standard());

        let cases = vec![
            ("(define a 42)", Ok(None)),
            ("42.42", Ok(Some(Value::Float(42.42)))),
            ("a", Ok(Some(Value::Integer(42)))),
            ("(+ 3 2)", Ok(Some(Value::Integer(5)))),
            ("(* 3 2)", Ok(Some(Value::Integer(6)))),
            ("(+ 3 2.0)", Ok(Some(Value::Float(5.0)))),
            ("(* 3.0 2)", Ok(Some(Value::Float(6.0)))),
            ("(define a 13)", Ok(None)),
            ("(+ 8 a)", Ok(Some(Value::Integer(21)))),
            ("(define f (lambda (a b) (+ (* 3 a) b)))", Ok(None)),
            ("(f 7 a)", Ok(Some(Value::Integer(34)))),
            ("(f 7.0 a)", Ok(Some(Value::Float(34.0)))),
        ];

        for (code, res) in cases {
            let expr = &parse_code(code).unwrap()[0];
            assert_eq!(Environment::evaluate_expr(&mut env, expr), res);
        }
    }
}
