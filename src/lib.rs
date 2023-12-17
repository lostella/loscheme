use std::collections::{HashMap, VecDeque};
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    OpenParen,
    CloseParen,
    Other(String),
}

pub fn tokenize(input: &str) -> VecDeque<Token> {
    let mut tokens = VecDeque::new();
    let mut current_token = String::new();

    for ch in input.chars() {
        match ch {
            '(' => {
                if !current_token.is_empty() {
                    tokens.push_back(Token::Other(current_token.clone()));
                    current_token.clear();
                }
                tokens.push_back(Token::OpenParen);
            }
            ')' => {
                if !current_token.is_empty() {
                    tokens.push_back(Token::Other(current_token.clone()));
                    current_token.clear();
                }
                tokens.push_back(Token::CloseParen);
            }
            c if c.is_whitespace() => {
                if !current_token.is_empty() {
                    tokens.push_back(Token::Other(current_token.clone()));
                    current_token.clear();
                }
            }
            _ => current_token.push(ch),
        }
    }

    // Check if there's any remaining token at the end
    if !current_token.is_empty() {
        tokens.push_back(Token::Other(current_token));
    }

    tokens
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i32),
    Bool(bool),
    Float(f32),
}

impl FromStr for Value {
    type Err = ();

    fn from_str(input: &str) -> Result<Value, Self::Err> {
        if input.chars().nth(0) == Some('#') {
            match &input[1..] {
                "t" | "true" => return Ok(Value::Bool(true)),
                "f" | "false" => return Ok(Value::Bool(false)),
                _ => return Err(())
            }
        }
        let as_i32 = input.parse::<i32>();
        match as_i32 {
            Ok(n) => return Ok(Value::Integer(n)),
            Err(_e) => (),
        }
        let as_f32 = input.parse::<f32>();
        match as_f32 {
            Ok(r) => return Ok(Value::Float(r)),
            Err(_e) => (),
        }
        Err(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Literal(Value),
    Symbol(String),
}

impl FromStr for Atom {
    type Err = ();

    fn from_str(input: &str) -> Result<Atom, Self::Err> {
        let as_value = Value::from_str(input);
        match as_value {
            Ok(v) => return Ok(Atom::Literal(v)),
            Err(_e) => (),
        }
        Ok(Atom::Symbol(input.to_string()))
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Atomic(Atom),
    Composed(Vec<Expr>),
}

impl Expr {
    fn from_tokens(tokens: &mut VecDeque<Token>) -> Result<Expr, &'static str> {
        match tokens.pop_front() {
            Some(token) => match token {
                Token::OpenParen => {
                    let mut sub_expressions = Vec::new();
                    while let Some(peeked) = tokens.front().cloned() {
                        if peeked == Token::CloseParen {
                            tokens.pop_front();
                            return Ok(Expr::Composed(sub_expressions));
                        } else if let Ok(expr) = Self::from_tokens(tokens) {
                            sub_expressions.push(expr);
                        } else {
                            return Err("Failed to parse sub-expression");
                        }
                    }
                    Err("Unmatched '('")
                }
                Token::CloseParen => Err("Unmatched ')'"),
                Token::Other(s) => {
                    if let Ok(atom) = Atom::from_str(&s) {
                        Ok(Expr::Atomic(atom))
                    } else {
                        Err("Failed to parse as Atom")
                    }
                }
            },
            None => Err("No more tokens"),
        }
    }
}

impl FromStr for Expr {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut tokens = tokenize(input);
        let res = Expr::from_tokens(&mut tokens);
        match res {
            Ok(expr) => Ok(expr),
            Err(s) => Err(()),
        }
    }
}

pub fn parse_str(input: &str) -> Result<Vec<Expr>, &'static str> {
    let mut tokens = tokenize(input);
    let mut expressions = Vec::<Expr>::new();
    while !tokens.is_empty() {
        let res = Expr::from_tokens(&mut tokens);
        match res {
            Ok(expr) => expressions.push(expr),
            Err(s) => return Err(s),
        }
    }
    Ok(expressions)
}

#[derive(Debug)]
pub struct Environment<'a> {
    values: HashMap<String, Value>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    fn create_child(&'a self) -> Self {
        Environment {
            values: HashMap::new(),
            parent: Some(self),
        }
    }

    fn set(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        if let Some(value) = self.values.get(name) {
            Some(value)
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn test_tokenize() {
        let source_code = "(define (add x y) (+ :x y))";
        let tokens = tokenize(source_code);

        let expected_tokens = vec![
            Token::OpenParen,
            Token::Other("define".to_string()),
            Token::OpenParen,
            Token::Other("add".to_string()),
            Token::Other("x".to_string()),
            Token::Other("y".to_string()),
            Token::CloseParen,
            Token::OpenParen,
            Token::Other("+".to_string()),
            Token::Other(":x".to_string()),
            Token::Other("y".to_string()),
            Token::CloseParen,
            Token::CloseParen,
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_atom() {
        assert_eq!(
            Atom::from_str("abc").unwrap(),
            Atom::Symbol("abc".to_string())
        );
        assert_eq!(
            Atom::from_str("1").unwrap(),
            Atom::Literal(Value::Integer(1))
        );
        assert_eq!(
            Atom::from_str("-42").unwrap(),
            Atom::Literal(Value::Integer(-42))
        );
    }

    #[test]
    fn parse_integer_literal() {
        assert_eq!(Expr::from_str("42"), Ok(Expr::Atomic(Atom::Literal(Value::Integer(42)))));
    }

    #[test]
    fn parse_boolean_literal() {
        assert_eq!(Expr::from_str("#t"), Ok(Expr::Atomic(Atom::Literal(Value::Bool(true)))));
        assert_eq!(Expr::from_str("#f"), Ok(Expr::Atomic(Atom::Literal(Value::Bool(false)))));
    }

    #[test]
    fn parse_float_literal() {
        assert_eq!(Expr::from_str("3.14"), Ok(Expr::Atomic(Atom::Literal(Value::Float(3.14)))));
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(Expr::from_str("variable"), Ok(Expr::Atomic(Atom::Symbol("variable".to_string()))));
    }

    #[test]
    fn parse_nested_expression() {
        assert_eq!(
            Expr::from_str("(+ 1 2)"),
            Ok(Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("+".to_string())),
                Expr::Atomic(Atom::Literal(Value::Integer(1))),
                Expr::Atomic(Atom::Literal(Value::Integer(2)))
            ]))
        );
    }

    #[test]
    fn parse_complex_expression() {
        assert_eq!(
            Expr::from_str("(if (> x 0) (* x 2) (- x 1))"),
            Ok(Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("if".to_string())),
                Expr::Composed(vec![
                    Expr::Atomic(Atom::Symbol(">".to_string())),
                    Expr::Atomic(Atom::Symbol("x".to_string())),
                    Expr::Atomic(Atom::Literal(Value::Integer(0)))
                ]),
                Expr::Composed(vec![
                    Expr::Atomic(Atom::Symbol("*".to_string())),
                    Expr::Atomic(Atom::Symbol("x".to_string())),
                    Expr::Atomic(Atom::Literal(Value::Integer(2)))
                ]),
                Expr::Composed(vec![
                    Expr::Atomic(Atom::Symbol("-".to_string())),
                    Expr::Atomic(Atom::Symbol("x".to_string())),
                    Expr::Atomic(Atom::Literal(Value::Integer(1)))
                ])
            ]))
        );
    }

    #[test]
    fn test_expr() {
        let expr_ref = Expr::Composed(vec![
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("a".to_string())),
                Expr::Atomic(Atom::Literal(Value::Integer(1))),
            ]),
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("b".to_string())),
                Expr::Atomic(Atom::Literal(Value::Integer(2))),
            ]),
        ]);
        let mut tokens = tokenize("((a 1) (b 2))");
        let expr = Expr::from_tokens(&mut tokens).unwrap();

        assert_eq!(expr, expr_ref);

        let expr_ref = Expr::Composed(vec![
            Expr::Atomic(Atom::Symbol("define".to_string())),
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("add".to_string())),
                Expr::Atomic(Atom::Symbol("x".to_string())),
                Expr::Atomic(Atom::Symbol("y".to_string())),
            ]),
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("+".to_string())),
                Expr::Atomic(Atom::Symbol(":x".to_string())),
                Expr::Atomic(Atom::Symbol("y".to_string())),
            ]),
        ]);
        let mut tokens = tokenize("(define (add x y) (+ :x y))");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(tokens.len(), 0);
        assert_eq!(expr, Ok(expr_ref));

        let mut tokens = tokenize("(define ((add x y) (+ :x y))");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(tokens.len(), 0);
        assert_eq!(expr, Err("Unmatched '('"));

        let mut tokens = tokenize(") abc");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(expr, Err("Unmatched ')'"));

        let mut tokens = tokenize("");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(tokens.len(), 0);
        assert_eq!(expr, Err("No more tokens"));
    }

    #[test]
    fn test_environment() {
        let mut global_environment = Environment::new();
        global_environment.set("x", Value::Integer(42));
        global_environment.set("y", Value::Bool(false));

        let mut child_environment = global_environment.create_child();
        child_environment.set("x", Value::Float(3.14));

        // Accessing 'x' in the child environment
        assert_eq!(child_environment.get("x"), Some(&Value::Float(3.14)));

        // Accessing 'y' in the child environment (falls back to global)
        assert_eq!(
            child_environment.get("y"),
            Some(&Value::Bool(false))
        );

        // Accessing 'z' in the child environment (not defined anywhere)
        assert_eq!(child_environment.get("z"), None);

        // Accessing 'x' in the global environment
        assert_eq!(global_environment.get("x"), Some(&Value::Integer(42)));

        // Accessing 'y' in the global environment
        assert_eq!(
            global_environment.get("y"),
            Some(&Value::Bool(false))
        );
    }
}
