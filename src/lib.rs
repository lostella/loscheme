use std::collections::HashMap;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Function(fn(&[Value]) -> Value),
    List(Vec<Value>),
}

type Env = HashMap<String, Value>;

fn define(args: &[Expr], env: &mut Env) -> Value {
    if args.len() != 2 {
        panic!("define expects exactly two arguments");
    }
    let symbol = match &args[0] {
        Expr::Symbol(s) => s.clone(),
        _ => panic!("First argument to define must be a symbol"),
    };
    let value = eval_expr(args[1].clone(), env);
    env.insert(symbol, value.clone());
    value
}

pub fn eval_expr(expr: Expr, env: &mut Env) -> Value {
    match expr {
        Expr::Number(num) => Value::Number(num),
        Expr::Symbol(sym) => env.get(&sym).cloned().expect("Undefined symbol"),
        Expr::List(list) => {
            let first = &list[0];
            let args = &list[1..];
            match first {
                Expr::Symbol(s) if s == "define" => define(args, env),
                Expr::Symbol(s) if s == "if" => builtin_if(args, env),
                _ => match eval_expr(first.clone(), env) {
                    Value::Function(f) => {
                        let eval_args: Vec<Value> = args.iter().map(|x| eval_expr(x.clone(), env)).collect();
                        f(&eval_args)
                    }
                    _ => panic!("First element in list is not a function"),
                },
            }
        }
    }
}

pub fn eval(input: &str, env: &mut Env) -> Value {
    let expr = parse(input);
    eval_expr(expr, env)
}

fn builtin_add(args: &[Value]) -> Value {
    let sum = args.iter().map(|x| match x {
        Value::Number(n) => *n,
        _ => panic!("Expected number"),
    }).sum();
    Value::Number(sum)
}

fn builtin_subtract(args: &[Value]) -> Value {
    let mut iter = args.iter();
    if let Some(Value::Number(first)) = iter.next() {
        let result = iter.fold(*first, |acc, x| match x {
            Value::Number(n) => acc - n,
            _ => panic!("Expected number"),
        });
        Value::Number(result)
    } else {
        panic!("Expected at least one argument");
    }
}

fn builtin_multiply(args: &[Value]) -> Value {
    let product = args.iter().map(|x| match x {
        Value::Number(n) => *n,
        _ => panic!("Expected number"),
    }).product();
    Value::Number(product)
}

fn builtin_divide(args: &[Value]) -> Value {
    let mut iter = args.iter();
    if let Some(Value::Number(first)) = iter.next() {
        let result = iter.fold(*first, |acc, x| match x {
            Value::Number(n) => acc / n,
            _ => panic!("Expected number"),
        });
        Value::Number(result)
    } else {
        panic!("Expected at least one argument");
    }
}

fn builtin_equal(args: &[Value]) -> Value {
    if args.len() != 2 {
        panic!("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Value::Number(if a == b { 1.0 } else { 0.0 }),
        _ => panic!("Expected numbers"),
    }
}

fn builtin_greater_than(args: &[Value]) -> Value {
    if args.len() != 2 {
        panic!("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Value::Number(if a > b { 1.0 } else { 0.0 }),
        _ => panic!("Expected numbers"),
    }
}

fn builtin_less_than(args: &[Value]) -> Value {
    if args.len() != 2 {
        panic!("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Value::Number(if a < b { 1.0 } else { 0.0 }),
        _ => panic!("Expected numbers"),
    }
}

fn builtin_and(args: &[Value]) -> Value {
    for arg in args {
        match arg {
            Value::Number(0.0) => return Value::Number(0.0),
            Value::Number(_) => continue,
            _ => panic!("Expected number"),
        }
    }
    Value::Number(1.0)
}

fn builtin_or(args: &[Value]) -> Value {
    for arg in args {
        match arg {
            Value::Number(0.0) => continue,
            Value::Number(_) => return Value::Number(1.0),
            _ => panic!("Expected number"),
        }
    }
    Value::Number(0.0)
}

fn builtin_not(args: &[Value]) -> Value {
    if args.len() != 1 {
        panic!("Expected exactly one argument");
    }
    match &args[0] {
        Value::Number(0.0) => Value::Number(1.0),
        Value::Number(_) => Value::Number(0.0),
        _ => panic!("Expected number"),
    }
}

fn builtin_if(args: &[Expr], env: &mut Env) -> Value {
    if args.len() != 3 {
        panic!("if expects exactly three arguments");
    }
    let cond = eval_expr(args[0].clone(), env);
    match cond {
        Value::Number(n) if n != 0.0 => eval_expr(args[1].clone(), env),
        Value::Number(_) => eval_expr(args[2].clone(), env),
        _ => panic!("Condition should be a number"),
    }
}

fn builtin_car(args: &[Value]) -> Value {
    if args.len() != 1 {
        panic!("car expects exactly one argument");
    }
    match &args[0] {
        Value::List(lst) => lst.first().cloned().expect("List is empty"),
        _ => panic!("Expected list"),
    }
}

fn builtin_cdr(args: &[Value]) -> Value {
    if args.len() != 1 {
        panic!("cdr expects exactly one argument");
    }
    match &args[0] {
        Value::List(lst) => Value::List(lst[1..].to_vec()),
        _ => panic!("Expected list"),
    }
}

fn builtin_cons(args: &[Value]) -> Value {
    if args.len() != 2 {
        panic!("cons expects exactly two arguments");
    }
    match &args[1] {
        Value::List(lst) => {
            let mut new_lst = vec![args[0].clone()];
            new_lst.extend(lst.clone());
            Value::List(new_lst)
        }
        _ => panic!("Expected list as second argument"),
    }
}

fn builtin_list(args: &[Value]) -> Value {
    Value::List(args.to_vec())
}

pub fn standard_env() -> Env {
    let mut env = HashMap::new();
    env.insert("+".to_string(), Value::Function(builtin_add));
    env.insert("-".to_string(), Value::Function(builtin_subtract));
    env.insert("*".to_string(), Value::Function(builtin_multiply));
    env.insert("/".to_string(), Value::Function(builtin_divide));
    env.insert("=".to_string(), Value::Function(builtin_equal));
    env.insert(">".to_string(), Value::Function(builtin_greater_than));
    env.insert("<".to_string(), Value::Function(builtin_less_than));
    env.insert("and".to_string(), Value::Function(builtin_and));
    env.insert("or".to_string(), Value::Function(builtin_or));
    env.insert("not".to_string(), Value::Function(builtin_not));
    env.insert("car".to_string(), Value::Function(builtin_car));
    env.insert("cdr".to_string(), Value::Function(builtin_cdr));
    env.insert("cons".to_string(), Value::Function(builtin_cons));
    env.insert("list".to_string(), Value::Function(builtin_list));
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

    #[test]
    fn test_sequence_of_expressions() {
        let mut env = standard_env();
        
        // Test define
        assert_eq!(eval("(define x 10)", &mut env), Value::Number(10.0));
        
        // Test usage of defined variable
        assert_eq!(eval("(+ x 5)", &mut env), Value::Number(15.0));
        
        // Test redefining the variable
        assert_eq!(eval("(define x 20)", &mut env), Value::Number(20.0));
        
        // Test usage of redefined variable
        assert_eq!(eval("(+ x 5)", &mut env), Value::Number(25.0));
        
        // Test a new variable and a more complex expression
        assert_eq!(eval("(define y 30)", &mut env), Value::Number(30.0));
        assert_eq!(eval("(+ x y 10)", &mut env), Value::Number(60.0));

        // Test subtraction
        assert_eq!(eval("(- x 5)", &mut env), Value::Number(15.0));

        // Test multiplication
        assert_eq!(eval("(* x 2)", &mut env), Value::Number(40.0));

        // Test division
        assert_eq!(eval("(/ x 4)", &mut env), Value::Number(5.0));

        // Test equality
        assert_eq!(eval("(= x 20)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(= x 10)", &mut env), Value::Number(0.0));

        // Test greater than
        assert_eq!(eval("(> x 10)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(> x 20)", &mut env), Value::Number(0.0));

        // Test less than
        assert_eq!(eval("(< x 30)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(< x 20)", &mut env), Value::Number(0.0));

        // Test and
        assert_eq!(eval("(and 1 1)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(and 1 0)", &mut env), Value::Number(0.0));

        // Test or
        assert_eq!(eval("(or 1 0)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(or 0 0)", &mut env), Value::Number(0.0));

        // Test not
        assert_eq!(eval("(not 0)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(not 1)", &mut env), Value::Number(0.0));

        // Test if expression
        assert_eq!(eval("(if (= x 20) 1 0)", &mut env), Value::Number(1.0));
        assert_eq!(eval("(if (= x 10) 1 0)", &mut env), Value::Number(0.0));

        // Test car
        assert_eq!(eval("(car (list 1 2 3))", &mut env), Value::Number(1.0));

        // Test cdr
        assert_eq!(eval("(cdr (list 1 2 3))", &mut env), Value::List(vec![Value::Number(2.0), Value::Number(3.0)]));

        // Test cons
        assert_eq!(eval("(cons 1 (list 2 3))", &mut env), Value::List(vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]));

        // Test list
        assert_eq!(eval("(list 1 2 3)", &mut env), Value::List(vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]));
    }
}
