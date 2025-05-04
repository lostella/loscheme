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
        ("((lambda () 1 2 3 42))", "42"),
        ("(if (> 3 7) (- 3 7) (- 7 3))", "4"),
        ("(if (< 3 7) (- 3 7) (- 7 3))", "-4"),
        (
            "(cond
                ((> 3 5) 'greater)
                ((< 3 5) 'smaller))",
            "smaller",
        ),
        (
            "(cond
                ((> 3 5) 'greater)
                (else 'not-greater))",
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
        ("(define (forty-two) 42)", ""),
        ("(forty-two)", "42"),
        ("(quotient 7 3)", "2"),
        ("(quotient -7 3)", "-2"),
        ("(quotient -7 -3)", "2"),
        ("(quotient 7 -3)", "-2"),
        ("(remainder 7 3)", "1"),
        ("(remainder -7 3)", "-1"),
        ("(remainder -7 -3)", "-1"),
        ("(remainder 7 -3)", "1"),
        ("(modulo 7 3)", "1"),
        ("(modulo -7 3)", "2"),
        ("(modulo -7 -3)", "-1"),
        ("(modulo 7 -3)", "-2"),
        ("`,42", "42"),
        ("`(a ,(+ 1 2) c)", "(a 3 c)"),
        ("`(a (+ 1 2) c)", "(a (+ 1 2) c)"),
        ("(define (double x) (* x 2))", ""),
        ("(map double '(1 2 3))", "(2 4 6)"),
        ("(reverse '(1 2 3))", "(3 2 1)"),
        ("(reverse '())", "()"),
        (
            "(define (sum xs)
                (if (null? xs)
                    0
                    (+ (car xs) (sum (cdr xs)))))",
            "",
        ),
        ("(sum '(1 2 3))", "6"),
        ("(define p '(1 2 3))", ""),
        ("(set-car! p 4)", ""),
        ("(car p)", "4"),
        ("(set-cdr! p '(5 6 7))", ""),
        ("(cdr p)", "(5 6 7)"),
        ("(set-cdr! p 42)", ""),
        ("p", "(4 . 42)"),
        ("(define p '(1 2 3))", ""),
        ("(define q '(4 5))", ""),
        ("(define pq (append p q))", ""),
        ("(set-car! (cdr (cdr pq)) 42)", ""),
        ("(set-car! (cdr (cdr (cdr pq))) 43)", ""),
        ("(set-cdr! (cdr (cdr (cdr pq))) '(44 45))", ""),
        ("pq", "(1 2 42 43 44 45)"),
        ("p", "(1 2 3)"),
        ("q", "(4 5)"),
        ("(list-ref pq 0)", "1"),
        ("(list-ref pq 4)", "44"),
        ("(list-ref pq 5)", "45"),
        ("(list-tail pq 0)", "(1 2 42 43 44 45)"),
        ("(list-tail pq 4)", "(44 45)"),
        ("(list-tail pq 5)", "(45)"),
        ("(list-tail pq 6)", "()"),
        ("(define endless '(1))", ""),
        ("(set-cdr! endless endless)", ""),
        ("(car endless)", "1"),
        ("(car (cdr endless))", "1"),
        ("(car (cdr (cdr endless)))", "1"),
        ("(car (cdr (cdr (cdr endless))))", "1"),
        ("(define n 42)", ""),
        ("(define (inc-n) (set! n (+ n 1)))", ""),
        ("(inc-n)", ""),
        ("n", "43"),
        (
            "(define counter
                (let
                    ((n 0))
                    (lambda () (set! n (+ n 1))
                    n)))",
            "",
        ),
        ("(counter)", "1"),
        ("(counter)", "2"),
        (
            "(define counter2
                (begin
                    (define n 0)
                    (lambda () (set! n (+ n 1)) n)))",
            "",
        ),
        ("(counter2)", "1"),
        ("(counter2)", "2"),
        (
            "(define (make-counter) (define n 0) (lambda () (set! n (+ n 1)) n))",
            "",
        ),
        ("(define c1 (make-counter))", ""),
        ("(define c2 (make-counter))", ""),
        ("(c1)", "1"),
        ("(c2)", "1"),
        ("(c2)", "2"),
        ("(c2)", "3"),
        ("(c1)", "2"),
        ("(define a '(2 3))", ""),
        ("(define b (cons 1 a))", ""),
        ("(set! a 42)", ""),
        ("b", "(1 2 3)"),
        // Equality predicates
        // Booleans
        ("(eq? #t #t)", "#t"),
        ("(eqv? #t #t)", "#t"),
        ("(equal? #t #t)", "#t"),
        ("(eq? #f #f)", "#t"),
        ("(eqv? #f #f)", "#t"),
        ("(equal? #f #f)", "#t"),
        ("(eq? #t #f)", "#f"),
        ("(eqv? #t #f)", "#f"),
        ("(equal? #t #f)", "#f"),
        // Symbols
        ("(eq? 'a 'a)", "#t"),
        ("(eqv? 'a 'a)", "#t"),
        ("(equal? 'a 'a)", "#t"),
        ("(eq? 'a 'b)", "#f"),
        ("(eqv? 'a 'b)", "#f"),
        ("(equal? 'a 'b)", "#f"),
        // Strings
        ("(eq? \"abc\" \"abc\")", "#f"),
        ("(eqv? \"abc\" \"abc\")", "#f"),
        ("(equal? \"abc\" \"abc\")", "#t"),
        // Numbers
        ("(eq? 1 1)", "#t"),
        ("(eqv? 1 1)", "#t"),
        ("(equal? 1 1)", "#t"),
        ("(eq? 1 1.0)", "#f"),
        ("(eqv? 1 1.0)", "#f"),
        ("(equal? 1 1.0)", "#f"),
        ("(eq? 1.0 1.0)", "#t"),
        ("(eqv? 1.0 1.0)", "#t"),
        ("(equal? 1.0 1.0)", "#t"),
        ("(eq? 1/2 0.5)", "#f"),
        ("(eqv? 1/2 0.5)", "#f"),
        ("(equal? 1/2 0.5)", "#f"),
        ("(eq? 100000000 100000000)", "#t"),
        ("(eqv? 100000000 100000000)", "#t"),
        ("(equal? 100000000 100000000)", "#t"),
        // Pairs and lists
        ("(let ((x (cons 1 2))) (eq? x x))", "#t"),
        ("(let ((x (cons 1 2))) (eq? x (cons 1 2)))", "#f"),
        ("(let ((x (cons 1 2))) (eqv? x x))", "#t"),
        ("(let ((x (cons 1 2))) (eqv? x (cons 1 2)))", "#f"),
        ("(let ((x (cons 1 2))) (equal? x x))", "#t"),
        ("(let ((x (cons 1 2))) (equal? x (cons 1 2)))", "#t"),
        ("(eq? (cons 1 2) (cons 1 2))", "#f"),
        ("(eqv? (cons 1 2) (cons 1 2))", "#f"),
        ("(equal? (cons 1 2) (cons 1 2))", "#t"),
        ("(eq? '(1 2 3) '(1 2 3))", "#f"),
        ("(eqv? '(1 2 3) '(1 2 3))", "#f"),
        ("(equal? '(1 2 3) '(1 2 3))", "#t"),
        // Empty list
        ("(eq? '() '())", "#t"),
        ("(eqv? '() '())", "#t"),
        ("(equal? '() '())", "#t"),
        // Procedures (opaque identity)
        ("(let ((f (lambda (x) x))) (eq? f f))", "#t"),
        ("(let ((f (lambda (x) x))) (eq? f (lambda (x) x)))", "#f"),
        ("(let ((f (lambda (x) x))) (eqv? f f))", "#t"),
        ("(let ((f (lambda (x) x))) (eqv? f (lambda (x) x)))", "#f"),
        ("(let ((f (lambda (x) x))) (equal? f f))", "#t"),
        ("(let ((f (lambda (x) x))) (equal? f (lambda (x) x)))", "#f"),
        // Nested structures
        ("(eq? '(1 (2 3)) '(1 (2 3)))", "#f"),
        ("(eqv? '(1 (2 3)) '(1 (2 3)))", "#f"),
        ("(equal? '(1 (2 3)) '(1 (2 3)))", "#t"),
        ("(let ((x '(2 3))) (eq? (list 1 x) (list 1 '(2 3))))", "#f"),
        ("(let ((x '(2 3))) (eqv? (list 1 x) (list 1 '(2 3))))", "#f"),
        (
            "(let ((x '(2 3))) (equal? (list 1 x) (list 1 '(2 3))))",
            "#t",
        ),
        // Aliasing test
        ("(let ((x '(1 2))) (eq? x x))", "#t"),
        ("(let ((x '(1 2))) (eqv? x x))", "#t"),
        ("(let ((x '(1 2))) (equal? x x))", "#t"),
        ("(let ((x (cons 1 2))) (let ((y x)) (eq? x y)))", "#t"),
        ("(let ((x (cons 1 2))) (let ((y x)) (eqv? x y)))", "#t"),
        ("(let ((x (cons 1 2))) (let ((y x)) (equal? x y)))", "#t"),
        // ("(eq? 'a 'a)", "#t"),
        // ("(eq? '(1 2) '(1 2))", "#f"),
        // ("(eqv? 0 0.0)", "#f"),
        // ("(eqv? '(a) '(a))", "#f"),
        // ("(equal? '(1 2) '(1 2))", "#t"),
        // ("(equal? '(a . b) '(a b))", "#f"),
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
        // ("(define (f . a) (* 2 (sum a)))", ""),
        // ("(f . (1 2 3))", "12"),
        // ("(define g (lambda a (* 3 (sum a))))", ""),
        // ("(g . (1 2 3))", "18"),
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
fn test_countdown() {
    let code = r#"
        (define (countdown n)
          (if (= n 0) 'done (countdown (- n 1))))
        (countdown 100000)
    "#;
    assert_eq!("done", format!("{}", run_standard(code).unwrap()))
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

#[test]
fn test_sqrt_newton_1() {
    let code = r#"
        (define (sqrt x)
            (define (square x) (* x x))
            (define (average x y) (/ (+ x y) 2))
            (define (good-enough? guess)
                (< (abs (- (square guess) x)) 0.001))
            (define (improve guess)
                (average guess (/ x guess)))
            (define (sqrt-iter guess)
                (if (good-enough? guess)
                    guess
                    (sqrt-iter (improve guess))))
            (sqrt-iter 1.0))

        (sqrt 2)
    "#;
    assert_eq!(
        format!("{}", run_standard(code).unwrap()),
        "1.4142156862745097"
    );
}

#[test]
fn test_sqrt_newton_2() {
    let code = r#"
        (define (square x) (* x x))

        (define (average x y) (/ (+ x y) 2))

        (define (good-enough? guess x)
            (< (abs (- (square guess) x)) 0.001))

        (define (improve guess x)
            (average guess (/ x guess)))

        (define (sqrt-iter guess x)
            (if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x) x)))

        (define (sqrt x)
            (sqrt-iter 1.0 x))

        (sqrt 2)
    "#;
    assert_eq!(
        format!("{}", run_standard(code).unwrap()),
        "1.4142156862745097"
    );
}
