use std::collections::HashMap;
use std::iter::{zip, Peekable};
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    // TODO add procedures
}

pub struct Environment<'a> {
    data: HashMap<String, Value>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: &'a Environment) -> Self {
        Self {
            data: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn set(&mut self, key: String, value: Value) {
        self.data.insert(key, value);
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

    pub fn evaluate(&self, _expr: &Expression) -> Option<Value> {
        Some(Value::Integer(0))
    }
}

pub struct Procedure<'a> {
    params: Vec<String>,
    body: Vec<Expression>,
    env: &'a Environment<'a>,
}

impl Procedure<'_> {
    pub fn call(&mut self, args: Vec<Value>) -> Result<Option<Value>, &str> {
        if args.len() != self.params.len() {
            return Err("Incorrect number of arguments");
        }
        let mut local_env = Environment::new_with_parent(self.env);
        for (param, arg) in zip(&self.params, args) {
            local_env.set(param.to_string(), arg)
        }
        let mut res: Option<Value> = None;
        for expr in &self.body {
            res = local_env.evaluate(expr)
        }
        Ok(res)
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

        let mut child_env = Environment::new_with_parent(&base_env);

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
}