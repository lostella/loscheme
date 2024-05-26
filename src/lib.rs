use std::collections::HashMap;

fn tokenize(input: &str) -> Vec<String> {
    input
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Symbol(String),
    Number(f64),
    List(Vec<Expr>),
}

fn parse_tokens(tokens: &mut Vec<String>) -> Result<Expr, &'static str> {
    let token = tokens.remove(0);
    match token.as_str() {
        "(" => {
            let mut list = Vec::new();
            while tokens[0] != ")" {
                let res = parse_tokens(tokens);
                match res {
                    Ok(expr) => list.push(expr),
                    _ => return res,
                }
            }
            tokens.remove(0); // Remove the closing ')'
            Ok(Expr::List(list))
        }
        ")" => Err("Unexpected ')'"),
        _ => {
            if let Ok(num) = token.parse::<f64>() {
                Ok(Expr::Number(num))
            } else {
                Ok(Expr::Symbol(token))
            }
        }
    }
}

pub fn parse(input: &str) -> Result<Expr, &'static str> {
    let mut tokens = tokenize(input);
    parse_tokens(&mut tokens)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Function(fn(&[Value]) -> Result<Value, &'static str>),
    List(Vec<Value>),
}

#[derive(Debug)]
struct Env<'a> {
    map: HashMap<String, Value>,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    // Create a new Env
    fn new() -> Self {
        Env {
            map: HashMap::new(),
            parent: None,
        }
    }

    // Create a new Env with the current Env as its parent Env
    fn create_child(&'a self) -> Env<'a> {
        Env {
            map: HashMap::new(),
            parent: Some(self),
        }
    }

    // Get a value from the Env, searching recursively in parent Envs if necessary
    fn get(&self, key: &Key) -> Option<&Value> {
        if let Some(value) = self.map.get(key) {
            Some(value)
        } else {
            match self.parent {
                Some(parent_env) => parent_env.get(key),
                None => None,
            }
        }
    }

    // Insert a value into the Env
    fn insert(&mut self, key: Key, value: Value) {
        self.map.insert(key, value);
    }

    // Remove a value from the Env
    fn remove(&mut self, key: &Key) -> Option<Value> {
        self.map.remove(key)
    }
}

fn define(args: &[Expr], env: &mut Env) -> Result<Value, &'static str> {
    if args.len() != 2 {
        return Err("define expects exactly two arguments");
    }
    let symbol = match &args[0] {
        Expr::Symbol(s) => s.clone(),
        _ => return Err("First argument to define must be a symbol"),
    };
    let res = eval_expr(args[1].clone(), env);
    env.insert(symbol, res.clone()?);
    res
}

pub fn eval_expr(expr: Expr, env: &mut Env) -> Result<Value, &'static str> {
    match expr {
        Expr::Number(num) => Ok(Value::Number(num)),
        Expr::Symbol(sym) => match env.get(&sym).cloned() {
            Some(value) => Ok(value),
            _ => Err("Undefined symbol"),
        },
        Expr::List(list) => {
            let first = &list[0];
            let args = &list[1..];
            match first {
                Expr::Symbol(s) if s == "define" => define(args, env),
                Expr::Symbol(s) if s == "if" => builtin_if(args, env),
                _ => match eval_expr(first.clone(), env) {
                    Ok(Value::Function(f)) => {
                        let res: Result<Vec<_>, &str> =
                            args.iter().map(|x| eval_expr(x.clone(), env)).collect();
                        res.and_then(|values| f(&values))
                    }
                    _ => Err("First element in list is not a function"),
                },
            }
        }
    }
}

pub fn eval(input: &str, env: &mut Env) -> Result<Value, &'static str> {
    let res = parse(input);
    match res {
        Ok(expr) => eval_expr(expr, env),
        Err(msg) => Err(msg),
    }
}

fn values_to_f64s(args: &[Value]) -> Result<Vec<&f64>, &'static str> {
    args.iter()
        .map(|x| match x {
            Value::Number(n) => Ok(n),
            _ => Err("Expected number"),
        })
        .collect()
}

fn builtin_add(args: &[Value]) -> Result<Value, &'static str> {
    let numbers = values_to_f64s(args);
    Ok(Value::Number(numbers?.into_iter().sum()))
}

fn builtin_multiply(args: &[Value]) -> Result<Value, &'static str> {
    let numbers = values_to_f64s(args);
    Ok(Value::Number(numbers?.into_iter().product()))
}

fn builtin_subtract(args: &[Value]) -> Result<Value, &'static str> {
    let numbers = values_to_f64s(args)?;
    if numbers.is_empty() {
        return Err("Expected at least one argument");
    }
    let mut acc = *numbers[0];
    for n in &numbers[1..] {
        acc -= *n;
    }
    Ok(Value::Number(acc))
}

fn builtin_divide(args: &[Value]) -> Result<Value, &'static str> {
    let numbers = values_to_f64s(args)?;
    if numbers.is_empty() {
        return Err("Expected at least one argument");
    }
    let mut acc = *numbers[0];
    for n in &numbers[1..] {
        acc /= *n;
    }
    Ok(Value::Number(acc))
}

fn builtin_equal(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 2 {
        return Err("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(if a == b { 1.0 } else { 0.0 })),
        _ => Err("Expected numbers"),
    }
}

fn builtin_greater_than(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 2 {
        return Err("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(if a > b { 1.0 } else { 0.0 })),
        _ => Err("Expected numbers"),
    }
}

fn builtin_less_than(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 2 {
        return Err("Expected exactly two arguments");
    }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(if a < b { 1.0 } else { 0.0 })),
        _ => Err("Expected numbers"),
    }
}

fn builtin_and(args: &[Value]) -> Result<Value, &'static str> {
    for arg in args {
        match arg {
            Value::Number(0.0) => return Ok(Value::Number(0.0)),
            Value::Number(_) => continue,
            _ => return Err("Expected number"),
        }
    }
    Ok(Value::Number(1.0))
}

fn builtin_or(args: &[Value]) -> Result<Value, &'static str> {
    for arg in args {
        match arg {
            Value::Number(0.0) => continue,
            Value::Number(_) => return Ok(Value::Number(1.0)),
            _ => return Err("Expected number"),
        }
    }
    Ok(Value::Number(0.0))
}

fn builtin_not(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 1 {
        return Err("Expected exactly one argument");
    }
    match &args[0] {
        Value::Number(0.0) => Ok(Value::Number(1.0)),
        Value::Number(_) => Ok(Value::Number(0.0)),
        _ => Err("Expected number"),
    }
}

fn builtin_if(args: &[Expr], env: &mut Env) -> Result<Value, &'static str> {
    if args.len() != 3 {
        return Err("if expects exactly three arguments");
    }
    let cond = eval_expr(args[0].clone(), env);
    match cond {
        Ok(Value::Number(n)) if n != 0.0 => eval_expr(args[1].clone(), env),
        Ok(Value::Number(_)) => eval_expr(args[2].clone(), env),
        _ => Err("Condition should be a number"),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 1 {
        return Err("car expects exactly one argument");
    }
    match &args[0] {
        Value::List(lst) => Ok(lst.first().cloned().expect("List is empty")),
        _ => Err("Expected list"),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 1 {
        return Err("cdr expects exactly one argument");
    }
    match &args[0] {
        Value::List(lst) => Ok(Value::List(lst[1..].to_vec())),
        _ => Err("Expected list"),
    }
}

fn builtin_cons(args: &[Value]) -> Result<Value, &'static str> {
    if args.len() != 2 {
        return Err("cons expects exactly two arguments");
    }
    match &args[1] {
        Value::List(lst) => {
            let mut new_lst = vec![args[0].clone()];
            new_lst.extend(lst.clone());
            Ok(Value::List(new_lst))
        }
        _ => Err("Expected list as second argument"),
    }
}

fn builtin_list(args: &[Value]) -> Result<Value, &'static str> {
    Ok(Value::List(args.to_vec()))
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
        assert_eq!(
            result,
            vec!["(", "define", "(", "a", "x", ")", "(", "*", "x", "2", ")", ")"]
        );
    }

    #[test]
    fn test_parse() {
        let result = parse("(define (a x) (* x 2))");
        assert_eq!(
            result,
            Ok(Expr::List(vec![
                Expr::Symbol(String::from_str("define").unwrap()),
                Expr::List(vec![
                    Expr::Symbol(String::from_str("a").unwrap()),
                    Expr::Symbol(String::from_str("x").unwrap())
                ]),
                Expr::List(vec![
                    Expr::Symbol(String::from_str("*").unwrap()),
                    Expr::Symbol(String::from_str("x").unwrap()),
                    Expr::Number(2.0)
                ]),
            ]))
        )
    }

    #[test]
    fn test_sequence_of_expressions() {
        let mut env = standard_env();

        // Test define
        assert_eq!(eval("(define x 10)", &mut env), Ok(Value::Number(10.0)));

        // Test usage of defined variable
        assert_eq!(eval("(+ x 5)", &mut env), Ok(Value::Number(15.0)));

        // Test redefining the variable
        assert_eq!(eval("(define x 20)", &mut env), Ok(Value::Number(20.0)));

        // Test usage of redefined variable
        assert_eq!(eval("(+ x 5)", &mut env), Ok(Value::Number(25.0)));

        // Test a new variable and a more complex expression
        assert_eq!(eval("(define y 30)", &mut env), Ok(Value::Number(30.0)));
        assert_eq!(eval("(+ x y 10)", &mut env), Ok(Value::Number(60.0)));

        // Test subtraction
        assert_eq!(eval("(- x 5)", &mut env), Ok(Value::Number(15.0)));

        // Test multiplication
        assert_eq!(eval("(* x 2)", &mut env), Ok(Value::Number(40.0)));

        // Test division
        assert_eq!(eval("(/ x 4)", &mut env), Ok(Value::Number(5.0)));

        // Test equality
        assert_eq!(eval("(= x 20)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(= x 10)", &mut env), Ok(Value::Number(0.0)));

        // Test greater than
        assert_eq!(eval("(> x 10)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(> x 20)", &mut env), Ok(Value::Number(0.0)));

        // Test less than
        assert_eq!(eval("(< x 30)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(< x 20)", &mut env), Ok(Value::Number(0.0)));

        // Test and
        assert_eq!(eval("(and 1 1)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(and 1 0)", &mut env), Ok(Value::Number(0.0)));

        // Test or
        assert_eq!(eval("(or 1 0)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(or 0 0)", &mut env), Ok(Value::Number(0.0)));

        // Test not
        assert_eq!(eval("(not 0)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(not 1)", &mut env), Ok(Value::Number(0.0)));

        // Test if expression
        assert_eq!(eval("(if (= x 20) 1 0)", &mut env), Ok(Value::Number(1.0)));
        assert_eq!(eval("(if (= x 10) 1 0)", &mut env), Ok(Value::Number(0.0)));

        // Test car
        assert_eq!(eval("(car (list 1 2 3))", &mut env), Ok(Value::Number(1.0)));

        // Test cdr
        assert_eq!(
            eval("(cdr (list 1 2 3))", &mut env),
            Ok(Value::List(vec![Value::Number(2.0), Value::Number(3.0)]))
        );

        // Test cons
        assert_eq!(
            eval("(cons 1 (list 2 3))", &mut env),
            Ok(Value::List(vec![
                Value::Number(1.0),
                Value::Number(2.0),
                Value::Number(3.0)
            ]))
        );

        // Test list
        assert_eq!(
            eval("(list 1 2 3)", &mut env),
            Ok(Value::List(vec![
                Value::Number(1.0),
                Value::Number(2.0),
                Value::Number(3.0)
            ]))
        );
    }
}
