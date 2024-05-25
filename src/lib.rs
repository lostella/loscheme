fn tokenize(input: &str) -> Vec<String> {
    input.replace('(', " ( ").replace(')', " ) ").split_whitespace().map(|s| s.to_string()).collect()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Symbol(String),
    Number(f64),
    List(Vec<Expr>),
}

fn parse_tokens(tokens: &mut Vec<String>) -> Expr {
    let token = tokens.remove(0);
    match token.as_str() {
        "(" => {
            let mut list = Vec::new();
            while tokens[0] != ")" {
                list.push(parse_tokens(tokens));
            }
            tokens.remove(0); // Remove the closing ')'
            Expr::List(list)
        }
        ")" => panic!("Unexpected ')'"),
        _ => {
            if let Ok(num) = token.parse::<f64>() {
                Expr::Number(num)
            } else {
                Expr::Symbol(token)
            }
        }
    }
}

pub fn parse(input: &str) -> Expr {
    let mut tokens = tokenize(input);
    parse_tokens(&mut tokens)
}

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Function(fn(&[Value]) -> Value),
    List(Vec<Value>),
}

type Env = HashMap<String, Value>;

fn define(env: &mut Env, args: &[Expr]) -> Value {
    if args.len() != 2 {
        panic!("define expects exactly two arguments");
    }
    let symbol = match &args[0] {
        Expr::Symbol(s) => s.clone(),
        _ => panic!("First argument to define must be a symbol"),
    };
    let value = eval(args[1].clone(), env);
    env.insert(symbol, value.clone());
    value
}

pub fn eval(expr: Expr, env: &mut Env) -> Value {
    match expr {
        Expr::Number(num) => Value::Number(num),
        Expr::Symbol(sym) => env.get(&sym).cloned().expect("Undefined symbol"),
        Expr::List(list) => {
            let first = &list[0];
            let args = &list[1..];
            match first {
                Expr::Symbol(s) if s == "define" => define(env, args),
                _ => match eval(first.clone(), env) {
                    Value::Function(f) => {
                        let eval_args: Vec<Value> = args.iter().map(|x| eval(x.clone(), env)).collect();
                        f(&eval_args)
                    }
                    _ => panic!("First element in list is not a function"),
                },
            }
        }
    }
}

fn add(args: &[Value]) -> Value {
    let sum = args.iter().map(|x| match x {
        Value::Number(n) => *n,
        _ => panic!("Expected number"),
    }).sum();
    Value::Number(sum)
}

fn mul(args: &[Value]) -> Value {
    let numbers = args.iter().map(|x| match x {
        Value::Number(n) => *n,
        _ => panic!("Expected number"),
    });
    let mut prod: f64 = 1.0;
    for num in numbers {
        prod *= num
    }
    Value::Number(prod)
}

pub fn standard_env() -> Env {
    let mut env = HashMap::new();
    env.insert("+".to_string(), Value::Function(add));
    env.insert("*".to_string(), Value::Function(mul));
    env
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_tokenize() {
        let result = tokenize("(define (a x) (* x 2))");
        assert_eq!(result, vec!["(", "define", "(", "a", "x", ")", "(", "*", "x", "2", ")", ")"]);
    }

    #[test]
    fn test_parse() {
        let result = parse("(define (a x) (* x 2))");
        assert_eq!(
            result,
            Expr::List(vec![
                Expr::Symbol(String::from_str("define").unwrap()),
                Expr::List(vec![Expr::Symbol(String::from_str("a").unwrap()), Expr::Symbol(String::from_str("x").unwrap())]),
                Expr::List(vec![Expr::Symbol(String::from_str("*").unwrap()), Expr::Symbol(String::from_str("x").unwrap()), Expr::Number(2.0)]),
            ])
        )
    }

    fn eval_expression(input: &str, env: &mut Env) -> Value {
        let expr = parse(input);
        eval(expr, env)
    }

    #[test]
    fn test_sequence_of_expressions() {
        let mut env = basic_env();
        
        // Test define
        assert_eq!(eval_expression("(define x 10)", &mut env), Value::Number(10.0));
        
        // Test usage of defined variable
        assert_eq!(eval_expression("(+ x 5)", &mut env), Value::Number(15.0));
        
        // Test redefining the variable
        assert_eq!(eval_expression("(define x 20)", &mut env), Value::Number(20.0));
        
        // Test usage of redefined variable
        assert_eq!(eval_expression("(+ x 5)", &mut env), Value::Number(25.0));
        
        // Test a new variable and a more complex expression
        assert_eq!(eval_expression("(define y 30)", &mut env), Value::Number(30.0));
        assert_eq!(eval_expression("(+ x y 10)", &mut env), Value::Number(60.0));
    }
}
