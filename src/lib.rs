use std::collections::{VecDeque, HashMap};
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

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    IntegerNumber(i32),
    Symbol(String),
}

impl FromStr for Atom {
    type Err = ();

    fn from_str(input: &str) -> Result<Atom, Self::Err> {
        let as_i32 = input.parse::<i32>();
        match as_i32 {
            Ok(n) => return Ok(Atom::IntegerNumber(n)),
            Err(_e) => (),
        }
        Ok(Atom::Symbol(input.to_string()))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Atomic(Atom),
    Composed(Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
struct ParseExprError;

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

pub fn parse_tokens(tokens: &mut VecDeque<Token>) -> Result<Vec<Expr>, &'static str> {
    let mut expressions = Vec::<Expr>::new();
    while !tokens.is_empty() {
        let res = Expr::from_tokens(tokens);
        match res {
            Ok(expr) => expressions.push(expr),
            Err(s) => return Err(s),
        }
    }
    Ok(expressions)
}

struct Environment {
    // TODO this should really map "symbols" to a bunch of different things
    // - values (of different types)
    // - functions (built-in and custom defined)
    // - ...?
    stack: Vec<HashMap<String, String>>,
}

impl Environment {
    fn new() -> Self {
        Environment { stack: Vec::new() }
    }

    fn push(&mut self, map: HashMap<String, String>) {
        self.stack.push(map);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn get(&self, key: &str) -> Option<&String> {
        for map in self.stack.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }
        None
    }

    fn set(&mut self, key: String, value: String) {
        if let Some(top_map) = self.stack.last_mut() {
            top_map.insert(key, value);
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
        assert_eq!(Atom::from_str("1").unwrap(), Atom::IntegerNumber(1));
        assert_eq!(Atom::from_str("-42").unwrap(), Atom::IntegerNumber(-42));
    }

    #[test]
    fn test_expr() {
        let expr_ref = Expr::Composed(vec![
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("a".to_string())),
                Expr::Atomic(Atom::IntegerNumber(1)),
            ]),
            Expr::Composed(vec![
                Expr::Atomic(Atom::Symbol("b".to_string())),
                Expr::Atomic(Atom::IntegerNumber(2)),
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
        let mut env = Environment::new();

        env.push(HashMap::new());
        env.set("key1".to_string(), "value1".to_string());

        assert_eq!(env.get("key1"), Some(&"value1".to_string()));

        env.push(HashMap::new());
        env.set("key1".to_string(), "value2".to_string());

        assert_eq!(env.get("key1"), Some(&"value2".to_string()));

        env.pop();

        assert_eq!(env.get("key1"), Some(&"value1".to_string()));

        env.pop();

        assert_eq!(env.get("key1"), None);
    }
}
