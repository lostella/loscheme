use std::{str::FromStr, collections::btree_map::IterMut, iter::Peekable};

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
enum Expr {
    Atomic(Atom),
    Composed(Vec<Box<Expr>>),
}

#[derive(Debug, PartialEq, Eq)]
struct ParseExprError;

// def read_from_tokens(tokens: list) -> Exp:
//     "Read an expression from a sequence of tokens."
//     if len(tokens) == 0:
//         raise SyntaxError("unexpected EOF")
//     token = tokens.pop(0)
//     if token == "(":
//         L = []
//         while tokens[0] != ")":
//             L.append(read_from_tokens(tokens))
//         tokens.pop(0)  # pop off ')'
//         return L
//     elif token == ")":
//         raise SyntaxError("unexpected )")
//     else:
//         return atom(token)

impl<'a> Expr {
    fn from_peekable_tokens<I>(tokens: &mut Peekable<I>) -> Result<Self, &str>
    where I: Iterator<Item = &'a Token> {
        // TODO implement here

        // if tokens is empty, return error
        let token = tokens.next().ok_or("unexpected end of tokens")?;

        match token {
            Token::OpenParen => {
                let mut subexprs = Vec::<Box<Expr>>::new();
                while tokens.peek().unwrap() != Token::CloseParen {
                    let next_subexpr = Self::from_peekable_tokens(tokens)?;
                    subexprs.push(Box::new(next_subexpr))
                }
                tokens.next();
                Ok(Expr::Composed(subexprs))
            }
            Token::CloseParen => {
                Err("unexpected closed parenthesis")
            }
            Token::Other(s) => {
                Ok(Expr::Atomic(Atom::Symbol(s.to_string())))
            }
        }
    }

    fn from_tokens(tokens: &[Token]) -> Result<Self, &str> {
        Self::from_peekable_tokens(&mut tokens.iter().peekable())
    }
}

// impl FromStr for Expr {
//     fn from_str(input: &str) -> Result<Expr, Self::Err> {
//         let mut tokens = tokenize(input).into_iter();
//         let current_token = tokens.next().ok_or(ParseExprError)?;

//         // if tokens is empty, return error
//         if tokens.len() == 0 {
//             return Err(ParseExprError)
//         }

//         return Ok(Expr::Atomic(Atom::Symbol("a".to_string())))

//         // keep counter of open parenthesis
//         // if first token is parenthesis
//         // - initialize counter to 1
//         // - set current expression to composed
//         // else
//         // - initialize counter to 0
//         // - set current expression as atomic

//         // for each token:
//         // - if current expression is atomic, return error
//         // - 
//         // if counter goes below zero, return error

//         // check that counter is zero
//     }
// }

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
        assert_eq!(Atom::from_str("abc").unwrap(), Atom::Symbol("abc".to_string()));
        assert_eq!(Atom::from_str("1").unwrap(), Atom::IntegerNumber(1));
        assert_eq!(Atom::from_str("-42").unwrap(), Atom::IntegerNumber(-42));
    }

    #[test]
    fn test_expr() {
        let c1 = Expr::Composed(vec![
            Box::new(Expr::Atomic(Atom::Symbol("a".to_string()))),
            Box::new(Expr::Atomic(Atom::IntegerNumber(1))),
        ]);
        let c2 = Expr::Composed(vec![
            Box::new(Expr::Atomic(Atom::Symbol("b".to_string()))),
            Box::new(Expr::Atomic(Atom::IntegerNumber(2))),
        ]);
        let c3 = Expr::Composed(vec![Box::new(c1), Box::new(c2)]);
        let tokens = tokenize("(define (add x y) (+ :x y))");
        let expr = Expr::from_tokens(&tokens);
    }
}
