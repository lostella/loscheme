use loscheme::run::{run, run_standard};
use loscheme::treewalk::Environment;

#[test]
fn test_language_features() {
    let steps = vec![
        ("13", "13"),
        ("-25", "-25"),
        ("42.42", "42.42"),
        ("-12.34", "-12.34"),
        ("5/4", "5/4"),
        ("-7/3", "-7/3"),
        ("6/4", "3/2"),
        ("-6/4", "-3/2"),
        ("15/3", "5"),
        ("-15/3", "-5"),
        ("#t", "#t"),
        ("#f", "#f"),
        ("\"hello, world!\"", "\"hello, world!\""),
        ("(define a 42)", ""),
        ("a", "42"),
        ("(+ 3 2)", "5"),
        ("(* 3 2)", "6"),
        ("(+ 3 2.0)", "5"),
        ("(* 3.0 2)", "6"),
        ("(- 10 2 3)", "5"),
        ("(/ 24 3 2)", "4"),
        ("(+ 3/4 7/3)", "37/12"),
        ("(- 3/4 7/3)", "-19/12"),
        ("(* 3/4 7/3)", "7/4"),
        ("(/ 3/4 7/3)", "9/28"),
        ("(< 3/4 3/2)", "#t"),
        ("(> 9/2 9/3)", "#t"),
        ("(< 5/4 5/6)", "#f"),
        ("(> 9/19 9/18)", "#f"),
        ("(= 3/5 6/10)", "#t"),
        ("(= 16/5 33/10)", "#f"),
        ("(+ 3/4 2)", "11/4"),
        ("(- 3/4 2)", "-5/4"),
        ("(* 3/4 2)", "3/2"),
        ("(/ 3/4 2)", "3/8"),
        ("(< 3/4 1)", "#t"),
        ("(> 9/2 4)", "#t"),
        ("(< 5/4 1)", "#f"),
        ("(> 9/19 4)", "#f"),
        ("(= 16/4 4)", "#t"),
        ("(= 16/5 4)", "#f"),
        ("(< 3/4 0.8)", "#t"),
        ("(> 9/2 3.4)", "#t"),
        ("(< 5/4 1.2)", "#f"),
        ("(> 9/19 0.5)", "#f"),
        ("(= 16/4 4.0)", "#t"),
        ("(= 16/5 3.9)", "#f"),
        ("(abs -5)", "5"),
        ("(abs 5)", "5"),
        ("(abs -5.0)", "5"),
        ("(abs 5.0)", "5"),
        ("(< 1 2 3)", "#t"),
        ("(< 1 3 2)", "#f"),
        ("(<= 1 1 1)", "#t"),
        ("(<= 1 0 1)", "#f"),
        ("(> 3 2 1)", "#t"),
        ("(> 1 3 2)", "#f"),
        ("(>= 1 1 1)", "#t"),
        ("(>= 1 1 2)", "#f"),
        ("(= -1 -1 -1)", "#t"),
        ("(= -1 -1 -2)", "#f"),
        ("(cons 1 2)", "(1 . 2)"),
        ("'(1 . 2)", "(1 . 2)"),
        ("'(1 2 . 3)", "(1 2 . 3)"),
        ("'(1 . (2 . 3))", "(1 2 . 3)"),
        ("(list 1 2 3)", "(1 2 3)"),
        ("'(1 2 3)", "(1 2 3)"),
        ("'(1 . (2 . (3 . ())))", "(1 2 3)"),
        ("(define a 13)", ""),
        ("(+ 8 a)", "21"),
        ("(define f (lambda (a b) (+ (* 3 a) b)))", ""),
        ("(f 7 a)", "34"),
        ("(f 7.0 a)", "34"),
        ("(if (> 3 7) (- 3 7) (- 7 3))", "4"),
        ("(if (< 3 7) (- 3 7) (- 7 3))", "-4"),
        ("(cond ((> 3 5) 'greater) ((< 3 5) 'smaller))", "smaller"),
        (
            "(cond ((> 3 5) 'greater) (else 'not-greater))",
            "not-greater",
        ),
        ("(begin (+ 4 7) (- 5 2) (* 7 3))", "21"),
        ("(let ((a 14) (b 7)) (+ a b) (- a b))", "7"),
        ("(length '())", "0"),
        ("(length '(4 5 6))", "3"),
        ("(append '(1 2) '(3) '() '(4))", "(1 2 3 4)"),
        ("(set! a -1)", ""),
        ("a", "-1"),
        ("(and)", "#t"),
        ("(and #t #t #f)", "#f"),
        ("(and #t #t #t)", "#t"),
        ("(or)", "#f"),
        ("(or #f #f #f)", "#f"),
        ("(or #f #t #f)", "#t"),
        ("(not #t)", "#f"),
        ("(not #f)", "#t"),
        ("(not 3)", "#f"),
        ("(not (list 3))", "#f"),
        ("(not '())", "#f"),
        ("(not 'nil)", "#f"),
        ("(quote ())", "()"),
        ("(quote (#t #f))", "(#t #f)"),
        ("(quote 42.0)", "42"),
        ("(quote (* 3 4))", "(* 3 4)"),
        ("'()", "()"),
        ("'(#t #f)", "(#t #f)"),
        ("'42.0", "42"),
        ("'(* 3 4)", "(* 3 4)"),
        ("(list? '())", "#t"),
        ("(list? '(1 2 3))", "#t"),
        ("(list? (list 1 2 3))", "#t"),
        ("(list? 42)", "#f"),
        ("(list? (cons 17 18))", "#f"),
        ("(pair? '())", "#f"),
        ("(pair? '(1 2 3))", "#t"),
        ("(pair? (list 1 2 3))", "#t"),
        ("(pair? 42)", "#f"),
        ("(pair? (cons 17 18))", "#t"),
        ("(null? 0)", "#f"),
        ("(null? #f)", "#f"),
        ("(null? '())", "#t"),
        ("(null? '(1))", "#f"),
        ("(number? 42)", "#t"),
        ("(number? 42.0)", "#t"),
        ("(number? \"hello\")", "#f"),
        ("(number? 'a)", "#f"),
        ("(number? '())", "#f"),
        ("(number? '(1 2 3))", "#f"),
        ("(number? #t)", "#f"),
        ("(symbol? 42)", "#f"),
        ("(symbol? 42.0)", "#f"),
        ("(symbol? \"hello\")", "#f"),
        ("(symbol? 'a)", "#t"),
        ("(symbol? '())", "#f"),
        ("(symbol? '(1 2 3))", "#f"),
        ("(symbol? #t)", "#f"),
        ("(string? \"hello\")", "#t"),
        ("(string? 3.14)", "#f"),
        ("(string? '())", "#f"),
        ("(boolean? #t)", "#t"),
        ("(boolean? #f)", "#t"),
        ("(boolean? \"hello\")", "#f"),
        ("(boolean? 3.14)", "#f"),
        ("(boolean? '())", "#f"),
        ("(procedure? (lambda (x) (* 2 x)))", "#t"),
        ("(procedure? #f)", "#f"),
        ("(procedure? \"hello\")", "#f"),
        ("(procedure? 3.14)", "#f"),
        ("(procedure? '())", "#f"),
        ("(even? 2)", "#t"),
        ("(even? 3)", "#f"),
        ("(even? -2)", "#t"),
        ("(even? -3)", "#f"),
        ("(odd? 2)", "#f"),
        ("(odd? 3)", "#t"),
        ("(odd? -2)", "#f"),
        ("(odd? -3)", "#t"),
        ("(positive? 2)", "#t"),
        ("(positive? 2.0)", "#t"),
        ("(positive? -2)", "#f"),
        ("(positive? -2.0)", "#f"),
        ("(negative? 2)", "#f"),
        ("(negative? 2.0)", "#f"),
        ("(negative? -2)", "#t"),
        ("(negative? -2.0)", "#t"),
        ("(zero? 0)", "#t"),
        ("(zero? 0.0)", "#t"),
        ("(zero? -0)", "#t"),
        ("(zero? -0.0)", "#t"),
        ("(zero? 1)", "#f"),
        ("(zero? 0.0001)", "#f"),
        ("(zero? -1)", "#f"),
        ("(zero? -0.0001)", "#f"),
        ("(apply + '(3 4))", "7"),
        ("(apply * (list -5 4))", "-20"),
        ("(define (twoxplusy x y) (+ (* 2 x) y))", ""),
        ("(apply twoxplusy '(-4 2))", "-6"),
        // ("`(a ,(+ 1 2) c)", "(a 3 c)"),
        // (
        //     "(case 2 ((1) 'one) ((2) 'two) (else 'other))",
        //     symbol_from_str("two"),
        // ),
        // (
        //     "(case 3 ((1) 'one) ((2) 'two) (else 'other))",
        //     symbol_from_str("other"),
        // ),
        // ("(let* ((x 2) (y (+ x 3))) y)", "5"),
        // (
        //     "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 4))",
        //     "24",
        // ),
        // ("(quotient 7 3)", "2"),
        // ("(remainder 7 3)", "1"),
        // ("(modulo 7 3)", "1"),
        // ("(eq? 'a 'a)", "#t"),
        // ("(eq? '(1 2) '(1 2))", "#f"),
        // ("(eqv? 1 1)", "#t"),
        // ("(eqv? 0 0.0)", "#f"),
        // ("(equal? '(1 2) '(1 2))", "#t"),
        // ("(equal? '(a . b) '(a b))", "#f"),
    ];

    let mut env = Environment::standard().child();
    for (code, exp) in steps {
        let got = format!("{}", run(code, &mut env).unwrap());
        assert_eq!(
            exp, got,
            "we are testing that {} gives {}, but got {}",
            code, exp, got
        );
    }
}

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
    assert_eq!("479001600", format!("{}", run_standard(code).unwrap()));
}

#[test]
fn test_fibonacci() {
    let code = r#"
        (define (fib n)
          (define (fib-tail-rec n a b)
            (if (= n 0)
                a
                ; the following call is in tail position
                (fib-tail-rec (- n 1) b (+ a b))))
          (fib-tail-rec n 0 1))

        (fib 50)
    "#;
    assert_eq!("12586269025", format!("{}", run_standard(code).unwrap()));
}

#[test]
fn test_ackermann() {
    let code = r#"
        (define (ackermann m n)
          (cond
            ((= m 0) (+ n 1))
            ((= n 0) (ackermann (- m 1) 1))
            (else (ackermann (- m 1) (ackermann m (- n 1))))))

        (ackermann 2 3) ; keep the arguments small!
    "#;
    assert_eq!("9", format!("{}", run_standard(code).unwrap()));
}

#[test]
fn test_quicksort() {
    let code = r#"
        (define (quicksort lst)
          (if (null? lst) ; the first commandment
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
