use crate::char::char_from_external;
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
    Pound,
    Atom(String),
}

#[derive(Debug, PartialEq)]
pub struct TokenInfo {
    token: Token,
    line: usize,
    column: usize,
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    fn emit_token_info(&self, token: Token) -> TokenInfo {
        TokenInfo {
            token,
            line: self.line,
            column: self.column,
        }
    }

    fn read_other(&mut self) -> String {
        let mut result = String::new();

        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '\'' || c == '`' || c == ',' {
                break;
            }
            result.push(c);
            self.input.next();
            self.column += 1;
        }

        result
    }

    fn read_string_literal(&mut self) -> String {
        let mut result = String::new();
        let mut cnt = 0;

        while let Some(&c) = self.input.peek() {
            result.push(c);
            self.input.next();
            self.column += 1;
            if c == '"' {
                cnt += 1;
            }
            if cnt == 2 {
                break;
            }
        }

        result
    }

    pub fn next_token(&mut self) -> Option<TokenInfo> {
        while let Some(&c) = self.input.peek() {
            match c {
                '\n' => {
                    self.input.next();
                    self.line += 1;
                    self.column = 1;
                    continue;
                }
                ' ' | '\t' | '\r' => {
                    self.input.next();
                    self.column += 1;
                    continue;
                }
                ';' => {
                    self.input.next();
                    self.column += 1;
                    while let Some(&cc) = self.input.peek() {
                        if cc == '\n' {
                            break;
                        }
                        self.input.next();
                    }
                }
                '(' => {
                    let token_info = self.emit_token_info(Token::LParen);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                ')' => {
                    let token_info = self.emit_token_info(Token::RParen);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                '\'' => {
                    let token_info = self.emit_token_info(Token::Quote);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                '`' => {
                    let token_info = self.emit_token_info(Token::Quasiquote);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                ',' => {
                    let token_info = self.emit_token_info(Token::Unquote);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                '.' => {
                    let token_info = self.emit_token_info(Token::Dot);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                '#' => {
                    let token_info = self.emit_token_info(Token::Pound);
                    self.input.next();
                    self.column += 1;
                    return Some(token_info);
                }
                '"' => {
                    let line = self.line;
                    let column = self.column;
                    let token = Token::Atom(self.read_string_literal());
                    return Some(TokenInfo {
                        token,
                        line,
                        column,
                    });
                }
                _ => {
                    let line = self.line;
                    let column = self.column;
                    let token = Token::Atom(self.read_other());
                    return Some(TokenInfo {
                        token,
                        line,
                        column,
                    });
                }
            }
        }
        None
    }
}

impl Iterator for Tokenizer<'_> {
    type Item = TokenInfo;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Include,
    Import,
    Quote,
    Quasiquote,
    Unquote,
    Lambda,
    Define,
    If,
    When,
    Unless,
    Cond,
    Case,
    Let,
    Letstar,
    Letrec,
    Set,
    Begin,
    And,
    Or,
    Dot,
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "include" => Ok(Keyword::Include),
            "import" => Ok(Keyword::Import),
            "lambda" => Ok(Keyword::Lambda),
            "quote" => Ok(Keyword::Quote),
            "quasiquote" => Ok(Keyword::Quasiquote),
            "unquote" => Ok(Keyword::Unquote),
            "define" => Ok(Keyword::Define),
            "if" => Ok(Keyword::If),
            "when" => Ok(Keyword::When),
            "unless" => Ok(Keyword::Unless),
            "cond" => Ok(Keyword::Cond),
            "case" => Ok(Keyword::Case),
            "let" => Ok(Keyword::Let),
            "let*" => Ok(Keyword::Letstar),
            "letrec" => Ok(Keyword::Letrec),
            "set!" => Ok(Keyword::Set),
            "begin" => Ok(Keyword::Begin),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            "." => Ok(Keyword::Dot),
            _ => Err(format!("Not a keyword: {s}")),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Include => write!(f, "include"),
            Keyword::Import => write!(f, "import"),
            Keyword::Lambda => write!(f, "lambda"),
            Keyword::Quote => write!(f, "quote"),
            Keyword::Quasiquote => write!(f, "quasiquote"),
            Keyword::Unquote => write!(f, "unquote"),
            Keyword::Define => write!(f, "define"),
            Keyword::If => write!(f, "if"),
            Keyword::When => write!(f, "when"),
            Keyword::Unless => write!(f, "unless"),
            Keyword::Cond => write!(f, "cond"),
            Keyword::Case => write!(f, "case"),
            Keyword::Let => write!(f, "let"),
            Keyword::Letstar => write!(f, "let*"),
            Keyword::Letrec => write!(f, "letrec"),
            Keyword::Set => write!(f, "set!"),
            Keyword::Begin => write!(f, "begin"),
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
            Keyword::Dot => write!(f, "."),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Str(String),
    Char(char),
    Bool(bool),
    Keyword(Keyword),
    Symbol(Intern<String>),
    List(Vec<Expr>),
    Vector(Vec<Expr>),
}

fn format_error(msg: String, token_info: &TokenInfo) -> Result<Expr, String> {
    Err(format!(
        "{msg} (at line {}, column {})",
        token_info.line, token_info.column
    ))
}

pub fn parse_tokens(tokens: &mut Peekable<Tokenizer>) -> Result<Vec<Expr>, String> {
    let mut expressions = Vec::new();
    while tokens.peek().is_some() {
        expressions.push(parse_expression(tokens)?);
    }
    Ok(expressions)
}

pub fn parse_expression(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    let Some(token_info) = tokens.next() else {
        return Err("Unexpected end of input".to_string());
    };
    match token_info.token {
        Token::LParen => parse_list(tokens),
        Token::Quote => parse_quote(tokens),
        Token::Quasiquote => parse_quasiquote(tokens),
        Token::Unquote => parse_unquote(tokens),
        Token::Atom(s) => Ok(parse_atom(s)),
        Token::Dot => Ok(Expr::Keyword(Keyword::Dot)),
        Token::Pound => {
            let Some(token_info) = tokens.next() else {
                return format_error("Unexpected end of tokens after `#`".into(), &token_info);
            };
            if token_info.token == Token::LParen {
                return parse_vector(tokens);
            }
            if let Token::Atom(ref s) = token_info.token {
                if s == "t" {
                    return Ok(Expr::Bool(true));
                }
                if s == "f" {
                    return Ok(Expr::Bool(false));
                }
                if let Some(stripped) = s.strip_prefix('\\') {
                    if let Some(c) = char_from_external(stripped) {
                        return Ok(Expr::Char(c));
                    }
                }
            }
            format_error(
                format!("Unexpected token `{:?}` after `#`", token_info.token),
                &token_info,
            )
        }
        Token::RParen => format_error("Unexpected `)`".into(), &token_info),
    }
}

fn parse_list(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    let mut v = vec![];
    let mut dot_found = false;
    let mut last_expression_parsed = false;
    while let Some(token_info) = tokens.peek() {
        match token_info.token {
            Token::RParen => {
                if dot_found && !last_expression_parsed {
                    return format_error("No expression following `.`".into(), token_info);
                }
                tokens.next();
                return Ok(Expr::List(v));
            }
            Token::Dot => {
                if v.is_empty() {
                    return format_error("No expressions preceeding `.`".into(), token_info);
                }
                if dot_found {
                    return format_error("Multiple `.` in list".into(), token_info);
                }
                dot_found = true;
                v.push(parse_expression(tokens)?);
            }
            _ => {
                if last_expression_parsed {
                    return format_error("Multiple expressions following `.`".into(), token_info);
                }
                v.push(parse_expression(tokens)?);
                if dot_found {
                    last_expression_parsed = true;
                }
            }
        }
    }
    Err("Unterminated list".to_string())
}

fn parse_vector(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    let mut vec = vec![];
    loop {
        if let Some(token_info) = tokens.peek() {
            match token_info.token {
                Token::RParen => {
                    tokens.next();
                    return Ok(Expr::Vector(vec));
                }
                _ => {
                    vec.push(parse_expression(tokens)?);
                }
            }
        } else {
            return Err("Unterminated vector".to_string());
        }
    }
}

fn parse_quote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::List(vec![
        Expr::Keyword(Keyword::Quote),
        parse_expression(tokens)?,
    ]))
}

fn parse_quasiquote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::List(vec![
        Expr::Keyword(Keyword::Quasiquote),
        parse_expression(tokens)?,
    ]))
}

fn parse_unquote(tokens: &mut Peekable<Tokenizer>) -> Result<Expr, String> {
    Ok(Expr::List(vec![
        Expr::Keyword(Keyword::Unquote),
        parse_expression(tokens)?,
    ]))
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

fn parse_atom(s: String) -> Expr {
    if let Ok(int) = s.parse::<i64>() {
        Expr::Integer(int)
    } else if let Ok(float) = s.parse::<f64>() {
        Expr::Float(float)
    } else if let Ok((num, denom)) = parse_rational(&s) {
        Expr::Rational(num, denom)
    } else if s.starts_with('"') && s.ends_with('"') {
        Expr::Str(s[1..s.len() - 1].to_string())
    } else if let Ok(keyword) = Keyword::from_str(&s) {
        Expr::Keyword(keyword)
    } else {
        Expr::Symbol(Intern::new(s))
    }
}

pub fn parse(code: &str) -> Result<Vec<Expr>, String> {
    let tokens = Tokenizer::new(code);
    parse_tokens(&mut tokens.peekable())
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Bool(v) => write!(f, "{}", if *v { "#t" } else { "#f" }),
            Expr::Integer(v) => write!(f, "{v}"),
            Expr::Float(v) => write!(f, "{v}"),
            Expr::Rational(n, d) => write!(f, "{n}/{d}"),
            Expr::Str(v) => write!(f, "\"{v}\""),
            Expr::Char(c) => write!(f, "#\\{c}"),
            Expr::Keyword(k) => write!(f, "{k}"),
            Expr::Symbol(s) => write!(f, "{s}"),
            Expr::List(v) => {
                write!(f, "(")?;
                if let Some((first, rest)) = v.split_first() {
                    write!(f, "{first}")?;
                    for el in rest {
                        write!(f, " {el}")?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            Expr::Vector(v) => {
                write!(f, "#(")?;
                for el in v {
                    write!(f, "{el}")?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

impl Expr {
    #[must_use]
    pub fn from_slice(v: &[Expr]) -> Self {
        Expr::List(v.to_vec())
    }

    pub fn into_vec(self) -> Result<Vec<Expr>, String> {
        match self {
            Expr::List(vec) => Ok(vec.clone()),
            _ => Err("Not a proper list".to_string()),
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
        let code = "(define (square x)\n  (* x x))\n'expr (quote ; comment (1 2 3)\n\n; comment line\n\n(1 2 3)) \"hello world\"";
        let expected = vec![
            TokenInfo {
                token: Token::LParen,
                line: 1,
                column: 1,
            },
            TokenInfo {
                token: Token::Atom("define".to_string()),
                line: 1,
                column: 2,
            },
            TokenInfo {
                token: Token::LParen,
                line: 1,
                column: 9,
            },
            TokenInfo {
                token: Token::Atom("square".to_string()),
                line: 1,
                column: 10,
            },
            TokenInfo {
                token: Token::Atom("x".to_string()),
                line: 1,
                column: 17,
            },
            TokenInfo {
                token: Token::RParen,
                line: 1,
                column: 18,
            },
            TokenInfo {
                token: Token::LParen,
                line: 2,
                column: 3,
            },
            TokenInfo {
                token: Token::Atom("*".to_string()),
                line: 2,
                column: 4,
            },
            TokenInfo {
                token: Token::Atom("x".to_string()),
                line: 2,
                column: 6,
            },
            TokenInfo {
                token: Token::Atom("x".to_string()),
                line: 2,
                column: 8,
            },
            TokenInfo {
                token: Token::RParen,
                line: 2,
                column: 9,
            },
            TokenInfo {
                token: Token::RParen,
                line: 2,
                column: 10,
            },
            TokenInfo {
                token: Token::Quote,
                line: 3,
                column: 1,
            },
            TokenInfo {
                token: Token::Atom("expr".to_string()),
                line: 3,
                column: 2,
            },
            TokenInfo {
                token: Token::LParen,
                line: 3,
                column: 7,
            },
            TokenInfo {
                token: Token::Atom("quote".to_string()),
                line: 3,
                column: 8,
            },
            TokenInfo {
                token: Token::LParen,
                line: 7,
                column: 1,
            },
            TokenInfo {
                token: Token::Atom("1".to_string()),
                line: 7,
                column: 2,
            },
            TokenInfo {
                token: Token::Atom("2".to_string()),
                line: 7,
                column: 4,
            },
            TokenInfo {
                token: Token::Atom("3".to_string()),
                line: 7,
                column: 6,
            },
            TokenInfo {
                token: Token::RParen,
                line: 7,
                column: 7,
            },
            TokenInfo {
                token: Token::RParen,
                line: 7,
                column: 8,
            },
            TokenInfo {
                token: Token::Atom("\"hello world\"".to_string()),
                line: 7,
                column: 10,
            },
        ];
        let tokens: Vec<TokenInfo> = Tokenizer::new(code).collect();
        for (token_info, expected_info) in tokens.into_iter().zip(expected) {
            assert_eq!(token_info, expected_info);
        }
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
            Expr::List(vec![
                Expr::Integer(-1),
                Expr::Keyword(Keyword::Dot),
                Expr::Integer(1),
            ]),
        ]);
        assert_eq!(
            expr.to_string(),
            "(define (f x) \"hello, world!\" (quote a) (* x 2) (-1 . 1))"
        );
    }
}
