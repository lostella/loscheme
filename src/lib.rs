use std::collections::VecDeque;
use std::{collections::btree_map::IterMut, iter::Peekable, str::FromStr};

#[derive(Debug, PartialEq, Clone)]
enum Token {
    OpenParen,
    CloseParen,
    Other(String),
}

fn tokenize(input: &str) -> VecDeque<Token> {
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
enum Atom {
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
        return Ok(Atom::Symbol(input.to_string()));
    }
}

// NOTE: not sure this is the best way
// see: https://rust-unofficial.github.io/too-many-lists/
#[derive(Debug, PartialEq, Eq)]
enum Expr {
    Atomic(Atom),
    Composed(Vec<Box<Expr>>),
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
                        } else {
                            if let Ok(expr) = Self::from_tokens(tokens) {
                                sub_expressions.push(Box::new(expr));
                            } else {
                                return Err("Failed to parse sub-expression");
                            }
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
            Box::new(Expr::Composed(vec![
                Box::new(Expr::Atomic(Atom::Symbol("a".to_string()))),
                Box::new(Expr::Atomic(Atom::IntegerNumber(1))),
            ])),
            Box::new(Expr::Composed(vec![
                Box::new(Expr::Atomic(Atom::Symbol("b".to_string()))),
                Box::new(Expr::Atomic(Atom::IntegerNumber(2))),
            ])),
        ]);
        let mut tokens = tokenize("((a 1) (b 2))");
        let expr = Expr::from_tokens(&mut tokens).unwrap();

        assert_eq!(expr, expr_ref);

        let expr_ref = Expr::Composed(vec![
            Box::new(Expr::Atomic(Atom::Symbol("define".to_string()))),
            Box::new(Expr::Composed(vec![
                Box::new(Expr::Atomic(Atom::Symbol("add".to_string()))),
                Box::new(Expr::Atomic(Atom::Symbol("x".to_string()))),
                Box::new(Expr::Atomic(Atom::Symbol("y".to_string()))),
            ])),
            Box::new(Expr::Composed(vec![
                Box::new(Expr::Atomic(Atom::Symbol("+".to_string()))),
                Box::new(Expr::Atomic(Atom::Symbol(":x".to_string()))),
                Box::new(Expr::Atomic(Atom::Symbol("y".to_string()))),
            ])),
        ]);
        let mut tokens = tokenize("(define (add x y) (+ :x y))");
        let expr = Expr::from_tokens(&mut tokens).unwrap();

        assert_eq!(expr, expr_ref);

        let mut tokens = tokenize("(define ((add x y) (+ :x y))");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(expr, Err("Unmatched '('"));

        let mut tokens = tokenize(") abc");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(expr, Err("Unmatched ')'"));

        let mut tokens = tokenize("");
        let expr = Expr::from_tokens(&mut tokens);

        assert_eq!(expr, Err("No more tokens"));
    }
}
