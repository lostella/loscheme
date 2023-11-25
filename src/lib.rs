use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum Token {
    OpenParen,
    CloseParen,
    Other(String),
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();

    for ch in input.chars() {
        match ch {
            '(' => {
                if !current_token.is_empty() {
                    tokens.push(Token::Other(current_token.clone()));
                    current_token.clear();
                }
                tokens.push(Token::OpenParen);
            }
            ')' => {
                if !current_token.is_empty() {
                    tokens.push(Token::Other(current_token.clone()));
                    current_token.clear();
                }
                tokens.push(Token::CloseParen);
            }
            c if c.is_whitespace() => {
                if !current_token.is_empty() {
                    tokens.push(Token::Other(current_token.clone()));
                    current_token.clear();
                }
            }
            _ => current_token.push(ch),
        }
    }

    // Check if there's any remaining token at the end
    if !current_token.is_empty() {
        tokens.push(Token::Other(current_token));
    }

    tokens
}

#[derive(Debug, PartialEq)]
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
        return Ok(Atom::Symbol(input.to_string()))
    }
}

// NOTE: not sure this is the best way
// see: https://rust-unofficial.github.io/too-many-lists/
enum Expression {
    Atomic(Atom),
    Composed(Vec<Box<Expression>>),
}

#[derive(Debug, PartialEq, Eq)]
struct ParseExpressionError;

impl FromStr for Expression {
    type Err = ParseExpressionError};

    fn from_str(input: &str) -> Result<Expression, Self::Err> {
        let tokens = tokenize(input);

        // if tokens is empty, return error

        // keep counter of open parenthesis
        // if first token is parenthesis
        // - initialize counter to 1
        // - set current expression to composed
        // else
        // - initialize counter to 0
        // - set current expression as atomic

        // for each token:
        // - if current expression is atomic, return error
        // - 
        // if counter goes below zero, return error

        // check that counter is zero
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(Atom::from_str("abc").unwrap(), Atom::Symbol("abc".to_string()));
        assert_eq!(Atom::from_str("1").unwrap(), Atom::IntegerNumber(1));
        assert_eq!(Atom::from_str("-42").unwrap(), Atom::IntegerNumber(-42));
    }
}
