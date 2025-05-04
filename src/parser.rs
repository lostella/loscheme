use internment::Intern;
use std::fmt;
use std::iter::Peekable;
use std::str::{Chars, FromStr};

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Quote,
    Quasiquote,
    Unquote,
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
            if c.is_whitespace() || c == '(' || c == ')' || c == '\'' || c == '`' || c == ',' {
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
                ';' => {
                    self.input.next();
                    while let Some(&cc) = self.input.peek() {
                        if cc == '\n' {
                            break;
                        }
                        self.input.next();
                    }
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
                '`' => {
                    self.input.next();
                    return Some(Token::Quasiquote);
                }
                ',' => {
                    self.input.next();
                    return Some(Token::Unquote);
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
    Quote,
    Quasiquote,
    Unquote,
    Lambda,
    Define,
    If,
    When,
    Unless,
    Cond,
    Let,
    LetStar,
    Set,
    Begin,
    And,
    Or,
    // TODO add more special forms here
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lambda" => Ok(Keyword::Lambda),
            "quote" => Ok(Keyword::Quote),
            "quasiquote" => Ok(Keyword::Quasiquote),
            "unquote" => Ok(Keyword::Unquote),
            "define" => Ok(Keyword::Define),
            "if" => Ok(Keyword::If),
            "when" => Ok(Keyword::When),
            "unless" => Ok(Keyword::Unless),
            "cond" => Ok(Keyword::Cond),
            "let" => Ok(Keyword::Let),
            "let*" => Ok(Keyword::LetStar),
            "set!" => Ok(Keyword::Set),
            "begin" => Ok(Keyword::Begin),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            _ => Err(format!("Not a keyword: {}", s)),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Lambda => write!(f, "lambda"),
            Keyword::Quote => write!(f, "quote"),
            Keyword::Quasiquote => write!(f, "quasiquote"),
            Keyword::Unquote => write!(f, "unquote"),
            Keyword::Define => write!(f, "define"),
            Keyword::If => write!(f, "if"),
            Keyword::When => write!(f, "when"),
            Keyword::Unless => write!(f, "unless"),
            Keyword::Cond => write!(f, "cond"),
            Keyword::Let => write!(f, "let"),
            Keyword::LetStar => write!(f, "let*"),
            Keyword::Set => write!(f, "set!"),
            Keyword::Begin => write!(f, "begin"),
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Null,
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Str(String),
    Bool(bool),
    Keyword(Keyword),
    Symbol(Intern<String>),
    Cons(Box<(Expr, Expr)>),
}

pub fn parse_tokens(tokens: &mut Peekable<Tokenizer>) -> Result<Vec<Expr>, String> {
    let mut expressions = Vec::new();
    while tokens.peek().is_some() {
        expressions.push(parse_expression(tokens)?);
    }
    Ok(expressions)
}

pub fn parse_expression(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    match tokens.next() {
        Some(Token::LParen) => parse_list(tokens),
        Some(Token::Quote) => parse_quote(tokens),
        Some(Token::Quasiquote) => parse_quasiquote(tokens),
        Some(Token::Unquote) => parse_unquote(tokens),
        Some(Token::Atom(s)) => parse_atom(s),
        Some(Token::Dot) => Err("Unexpected dot".to_string()),
        Some(Token::RParen) => Err("Unexpected closing parenthesis".to_string()),
        None => Err("Unexpected end of input".to_string()),
    }
}

fn parse_list(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    if let Some(token) = tokens.peek() {
        match token {
            Token::RParen => {
                tokens.next();
                return Ok(Expr::Null);
            }
            Token::Dot => {
                tokens.next();
                let res = parse_expression(tokens);
                match tokens.next() {
                    Some(Token::RParen) => return res,
                    _ => return Err("Expected closing parenthesis".to_string()),
                }
            }
            _ => {
                return Ok(Expr::Cons(Box::new((
                    parse_expression(tokens)?,
                    parse_list(tokens)?,
                ))));
            }
        }
    }
    Err("Unterminated list".to_string())
}

fn parse_quote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::Cons(Box::new((
        Expr::Keyword(Keyword::Quote),
        Expr::Cons(Box::new((parse_expression(tokens)?, Expr::Null))),
    ))))
}

fn parse_quasiquote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::Cons(Box::new((
        Expr::Keyword(Keyword::Quasiquote),
        Expr::Cons(Box::new((parse_expression(tokens)?, Expr::Null))),
    ))))
}

fn parse_unquote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::Cons(Box::new((
        Expr::Keyword(Keyword::Unquote),
        Expr::Cons(Box::new((parse_expression(tokens)?, Expr::Null))),
    ))))
}

fn parse_rational(input: &str) -> Result<(i64, i64), ()> {
    let parts: Vec<&str> = input.split('/').collect();
    if parts.len() != 2 {
        return Err(());
    }
    let num = parts[0].parse().map_err(|_| ())?;
    if parts[1].starts_with('+') || parts[1].starts_with('-') {
        return Err(());
    }
    let denom: i64 = parts[1].parse().map_err(|_| ())?;
    if denom <= 0 {
        return Err(());
    }
    Ok((num, denom))
}

fn parse_atom(s: String) -> Result<Expr, String> {
    if let Ok(int) = s.parse::<i64>() {
        Ok(Expr::Integer(int))
    } else if let Ok(float) = s.parse::<f64>() {
        Ok(Expr::Float(float))
    } else if let Ok((num, denom)) = parse_rational(&s) {
        Ok(Expr::Rational(num, denom))
    } else if s == "#t" {
        Ok(Expr::Bool(true))
    } else if s == "#f" {
        Ok(Expr::Bool(false))
    } else if s.starts_with('"') && s.ends_with('"') {
        Ok(Expr::Str(s[1..s.len() - 1].to_string()))
    } else if let Ok(keyword) = Keyword::from_str(&s) {
        Ok(Expr::Keyword(keyword))
    } else {
        Ok(Expr::Symbol(Intern::new(s)))
    }
}

pub fn parse(code: &str) -> Result<Vec<Expr>, String> {
    let tokens = Tokenizer::new(code);
    parse_tokens(&mut tokens.peekable())
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Null => write!(f, "()"),
            Expr::Bool(v) => write!(f, "{}", if *v { "#t" } else { "#f" }),
            Expr::Integer(v) => write!(f, "{}", v),
            Expr::Float(v) => write!(f, "{}", v),
            Expr::Rational(n, d) => write!(f, "{}/{}", n, d),
            Expr::Str(v) => write!(f, "\"{}\"", v),
            Expr::Keyword(k) => write!(f, "{}", k),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Cons(p) => {
                write!(f, "(")?;
                let mut cur = p;
                loop {
                    write!(f, "{}", cur.0)?;
                    match &cur.1 {
                        Expr::Null => break,
                        Expr::Cons(pp) => cur = pp,
                        _ => {
                            write!(f, " . {}", cur.1)?;
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
    pub fn from_slice(v: &[Expr]) -> Self {
        match v.len() {
            0 => Expr::Null,
            _ => Expr::Cons(Box::new((v[0].clone(), Expr::from_slice(&v[1..v.len()])))),
        }
    }

    pub fn into_vec(self) -> Result<Vec<Expr>, String> {
        let mut res = Vec::new();
        let mut cur = self;
        loop {
            match cur {
                Expr::Cons(pair) => {
                    res.push(pair.0.clone());
                    cur = pair.1.clone();
                }
                Expr::Null => return Ok(res),
                _ => return Err("Not a proper list".to_string()),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn intern_str(s: &str) -> Intern<String> {
        Intern::new(s.to_string())
    }

    fn symbol_from_str(s: &str) -> Expr {
        Expr::Symbol(intern_str(s))
    }

    #[test]
    fn test_tokenizer() {
        let code = "(define (square x) (* x x)) 'expr (quote ; comment\n(1 2 3)) \"hello world\"";
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
        let code = "; comment\n(define (square x) ; comment\n\t(* x x))";
        let expected = vec![Expr::from_slice(&[
            Expr::Keyword(Keyword::Define),
            Expr::from_slice(&[symbol_from_str("square"), symbol_from_str("x")]),
            Expr::from_slice(&[
                symbol_from_str("*"),
                symbol_from_str("x"),
                symbol_from_str("x"),
            ]),
        ])];
        let expressions: Vec<Expr> = parse(code).unwrap();
        assert_eq!(expressions, expected);
    }

    #[test]
    fn test_pair_vs_vec() {
        let v = vec![Expr::Integer(3), Expr::Integer(2), Expr::Integer(1)];
        let expr = Expr::from_slice(&v);
        let res = expr.into_vec();
        assert_eq!(res, Ok(v));
    }

    #[test]
    fn test_external_repr() {
        let expr = Expr::from_slice(&[
            Expr::Keyword(Keyword::Define),
            Expr::from_slice(&[symbol_from_str("f"), symbol_from_str("x")]),
            Expr::Str("hello, world!".to_string()),
            Expr::from_slice(&[Expr::Keyword(Keyword::Quote), symbol_from_str("a")]),
            Expr::from_slice(&[
                symbol_from_str("*"),
                symbol_from_str("x"),
                symbol_from_str("2"),
            ]),
            Expr::Cons(Box::new((Expr::Integer(-1), Expr::Integer(1)))),
        ]);
        assert_eq!(
            expr.to_string(),
            "(define (f x) \"hello, world!\" (quote a) (* x 2) (-1 . 1))"
        );
    }
}
