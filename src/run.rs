use crate::errors::MyError;
use crate::treewalk::{parse, Environment, Expr};

pub fn run(code: &str, env: &mut Environment) -> Result<Expr, MyError> {
    let mut val: Expr = Expr::Unspecified;

    match parse(code) {
        Ok(exprs) => {
            for expr in exprs {
                match env.evaluate(&expr) {
                    Err(err) => return Err(MyError::RuntimeError(err.to_string())),
                    Ok(expr) => val = expr,
                }
            }
            Ok(val)
        }
        Err(err) => Err(MyError::ParseError(err.to_string())),
    }
}

pub fn run_standard(code: &str) -> Result<Expr, MyError> {
    let mut env = Environment::standard().child();
    run(code, &mut env)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_factorial() {
        let code = r#"
            (define fact
                (lambda (n) (
                    if (<= n 1)
                    1
                    (* n (fact (- n 1))))))

            (fact 12)
        "#;
        assert_eq!(run_standard(code), Ok(Expr::Integer(479_001_600)));
    }

    #[test]
    fn test_fibonacci() {
        let code = r#"
            (define (fib n)
              (define (fib-tail-rec n a b)
                (if (= n 0)
                    a
                    (fib-tail-rec (- n 1) b (+ a b))))
              (fib-tail-rec n 0 1))

            (fib 50)
        "#;
        assert_eq!(run_standard(code), Ok(Expr::Integer(12_586_269_025)));
    }

    #[test]
    fn test_ackermann() {
        let code = r#"
            (define (ackermann m n)
              (cond
                ((= m 0) (+ n 1))
                ((= n 0) (ackermann (- m 1) 1))
                (else (ackermann (- m 1) (ackermann m (- n 1))))))

            (ackermann 2 3)
        "#;
        assert_eq!(run_standard(code), Ok(Expr::Integer(9)));
    }

    #[test]
    fn test_quicksort() {
        let code = r#"
            (define (quicksort lst)
              (if (null? lst)
                  '()
                  (let ((pivot (car lst))
                        (rest (cdr lst)))
                    (append
                      (quicksort (filter (lambda (x) (< x pivot)) rest))
                      (list pivot)
                      (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

            (quicksort '(34 7 23 32 5 62 32 2 1 6 45 78 99 3))
        "#;
        assert_eq!(
            format!("{}", run_standard(code).unwrap()),
            "(1 2 3 5 6 7 23 32 32 34 45 62 78 99)"
        );
    }
}
