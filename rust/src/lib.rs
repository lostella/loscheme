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

    pub fn parse(&mut self) -> Result<Vec<Expression>, String> {
        let mut expressions = Vec::new();
        while self.tokens.peek().is_some() {
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        match self.tokens.next() {
            Some(Token::LParen) => self.parse_list(),
            Some(Token::Quote) => self.parse_quote(),
            Some(Token::Atom(s)) => self.parse_atom(s),
            Some(Token::RParen) => Err("Unexpected closing parenthesis".into()),
            None => Err("Unexpected end of input".into()),
        }
    }

    fn parse_list(&mut self) -> Result<Expression, String> {
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
        Err("Unterminated list".into())
    }

    fn parse_quote(&mut self) -> Result<Expression, String> {
        Ok(Expression::List(vec![
            Expression::Keyword(Keyword::Quote),
            self.parse_expression()?,
        ]))
    }

    fn parse_atom(&self, s: String) -> Result<Expression, String> {
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

pub fn parse_code(code: &str) -> Result<Vec<Expression>, String> {
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
    Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    data: HashMap<String, Value>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<Environment>) -> Self {
        Self {
            data: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn set(&mut self, key: String, value: Value) -> Option<Value> {
        self.data.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        match self.data.get(key) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(environment) => environment.get(key),
                None => None,
            },
        }
    }

    pub fn evaluate(&mut self, expr: &Expression) -> Result<Option<Value>, &str> {
        match expr {
            Expression::Identifier(s) => match self.get(s) {
                Some(value) => Ok(Some(value.clone())),
                None => Err("Undefined identifier"),
            },
            Expression::Literal(l) => Ok(Some(l.to_value())),
            Expression::List(v) => self.evaluate_nonatomic(v),
            _ => Ok(None),
        }
    }

    fn evaluate_nonatomic(&mut self, exprs: &[Expression]) -> Result<Option<Value>, &str> {
        match exprs.len() {
            0 => Err("Cannot evaluate empty, non-atomic expressions"),
            _ => match &exprs[0] {
                Expression::Keyword(k) => self.evaluate_special(k, &exprs[1..]),
                _ => {
                    // NOTE placeholder
                    // TODO evaluate exprs[0]
                    // TODO if not a procedure, then error
                    // TODO evaluate exprs[1], exprs[2], ... to get arguments values
                    // TODO call procedure on arguments
                    Ok(None)
                }
            },
        }
    }

    fn evaluate_special(
        &mut self,
        keyword: &Keyword,
        args: &[Expression],
    ) -> Result<Option<Value>, &str> {
        match keyword {
            Keyword::Lambda => {
                // NOTE placeholder
                // TODO implement
                Ok(None)
            }
            Keyword::Quote => {
                match args.len() {
                    1 => Ok(args[0].clone()),
                    _ => Err("Must quote exactly one expression"),
                }
            }
            Keyword::Define => {
                // NOTE placeholder
                // TODO implement
                Ok(None)
            }
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Procedure {
    params: Vec<String>,
    body: Vec<Expression>,
    local_env: Environment,
}

impl Procedure {
    pub fn new(params: Vec<String>, body: Vec<Expression>, env: Rc<Environment>) -> Self {
        Self {
            params,
            body,
            local_env: Environment::new_with_parent(env),
        }
    }

    pub fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &str> {
        if args.len() != self.params.len() {
            return Err("Incorrect number of arguments");
        }
        self.local_env.data.drain();
        for (param, arg) in zip(&self.params, args) {
            self.local_env.set(param.to_string(), arg);
        }
        for expr in &self.body[..self.body.len() - 1] {
            let _ = self.local_env.evaluate(expr);
        }
        match self.body.last() {
            Some(expr) => self.local_env.evaluate(expr),
            None => Ok(None),
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
        let mut base_env = Environment::new();
        base_env.set("a".to_string(), Value::Integer(42));
        let base_env = Rc::new(base_env);

        let mut child_env = Environment::new_with_parent(base_env.clone());

        child_env.set("a".to_string(), Value::Str("hello".to_string()));
        child_env.set("b".to_string(), Value::Str("world".to_string()));

        assert_eq!(base_env.get("a"), Some(Value::Integer(42)).as_ref());
        assert_eq!(base_env.get("b"), None);
        assert_eq!(
            child_env.get("a"),
            Some(Value::Str("hello".to_string())).as_ref()
        );
        assert_eq!(
            child_env.get("b"),
            Some(Value::Str("world".to_string())).as_ref()
        );
    }

    #[test]
    fn test_evaluate() {
        let mut env = Environment::new();
        env.set("a".to_string(), Value::Integer(42));

        let expr = &parse_code("42.42").unwrap()[0];
        assert_eq!(env.evaluate(expr), Ok(Some(Value::Float(42.42))));

        let expr = &parse_code("a").unwrap()[0];
        assert_eq!(env.evaluate(expr), Ok(Some(Value::Integer(42))));
    }
}
