import pytest
from typing import Any, List, Tuple

from loscheme import Environment, parse, external_repr


TEST_CASES = [
    [
        ("42", "42"),
        ("23.10", "23.1"),
        ("#t", "#t"),
        ("#f", "#f"),
    ],
    [
        ("(quote 3)", "3"),
        ("(quote hello)", "hello"),
        ("(quote (1 2 3))", "(1 2 3)"),
        ("(quote #f)", "#f"),
        ("(quote (f a b 42))", "(f a b 42)"),
        ("(quote ())", "()"),
    ],
    [
        ("(define (f x) (* 3 x))", None),
        ("(quote (f 4))", "(f 4)"),
        ("(eval (quote (f 4)))", "12"),
    ],
    [
        ("(atom? 42)", "#t"),
        ("(atom? (quote hello))", "#t"),
        ("(atom? (quote (1 2 3)))", "#f"),
        ("(atom? #f)", "#t"),
        ("(atom? (quote ()))", "#f"),
        ("(atom? (list 1 2 3))", "#f"),
        ("(atom? (+ 1 2 3))", "#t"),
        ("(atom? (quote (+ 1 2 3)))", "#f"),
    ],
    [
        ("(define a (list 2 3))", None),
        ("(define b (cons 1 a))", None),
        ("(car b)", "1"),
        ("(cdr b)", "(2 3)"),
        ("(cdr (cons 1 2))", "2"),
        ("(cdr (list 1 2))", "(2)"),
        ("(length b)", "3"),
        ("(length (cdr b))", "2"),
        ("(reverse (list 1 2 3))", "(3 2 1)"),
    ],
    [
        ("(eq? 1 1)", "#t"),
        ("(eq? (quote 1) (quote 1))", "#t"),
        ("(eq? (list 1 2 3) (list 1 2 3))", "#f"),
        ("(eq? 1 2)", "#f"),
        ("(eq? (quote 1) (quote 2))", "#f"),
        ("(equal? 1 1)", "#t"),
        ("(equal? (quote 1) (quote 1))", "#t"),
        ("(equal? (list 1 2 3) (list 1 2 3))", "#t"),
        ("(equal? 1 2)", "#f"),
        ("(equal? (quote 1) (quote 2))", "#f"),
        ("(equal? (list 1 2 3) (list 1 2 42))", "#f"),
    ],
    [
        ("(integer? 42)", "#t"),
        ("(integer? 42.0)", "#t"),
        ("(integer? 42.1)", "#f"),
        ("(integer? #t)", "#f"),
        ("(number? 42)", "#t"),
        ("(number? 42.0)", "#t"),
        ("(number? (list 1 2 3))", "#f"),
        ("(null? (list))", "#t"),
        ("(null? 7)", "#f"),
        ("(null? #f)", "#f"),
        ("(boolean? #t)", "#t"),
        ("(boolean? #f)", "#t"),
        ("(boolean? 13)", "#f"),
        ("(symbol? (quote hello))", "#t"),
        ("(symbol? +)", "#f"),
        ("(symbol? 13)", "#f"),
        ("(list? (list 1 2 3))", "#t"),
        ("(list? (quote (1 2 3)))", "#t"),
        ("(list? #f)", "#f"),
        ("(list? (quote hello))", "#f"),
        ("(list? 13.9)", "#f"),
        ("(list? (cons 1 2))", "#f"),
        ("(list? (cons 1 (list 2)))", "#t"),
        ("(pair? 42)", "#f"),
        ("(pair? (cons 1 2))", "#t"),
        ("(pair? (list 1 2))", "#t"),
        ("(pair? (quote ()))", "#f"),
    ],
    [
        ("(zero? 0)", "#t"),
        ("(zero? 0.0)", "#t"),
        ("(zero? 1e-11)", "#f"),
        ("(zero? 3)", "#f"),
        ("(positive? 1e-11)", "#t"),
        ("(positive? 4)", "#t"),
        ("(positive? -2)", "#f"),
        ("(positive? -1e-11)", "#f"),
        ("(negative? 1e-11)", "#f"),
        ("(negative? 4)", "#f"),
        ("(negative? -2)", "#t"),
        ("(negative? -1e-11)", "#t"),
        ("(odd? 0)", "#f"),
        ("(odd? 1)", "#t"),
        ("(odd? -1)", "#t"),
        ("(odd? 42)", "#f"),
        ("(even? 0)", "#t"),
        ("(even? 1)", "#f"),
        ("(even? -42)", "#t"),
        ("(even? -3)", "#f"),
    ],
    [
        ("(< 1 2 3)", "#t"),
        ("(< 1 2 2)", "#f"),
        ("(<= 1 2 2)", "#t"),
        ("(<= 1 2 1)", "#f"),
        ("(> 2 1 0)", "#t"),
        ("(> 2 1 1)", "#f"),
        ("(>= 2 1 1)", "#t"),
        ("(>= 2 1 2)", "#f"),
    ],
    [
        ("(define a 3)", None),
        ("(define b 4)", None),
        ("(+ a b)", "7"),
        ("(define a 5)", None),
        ("a", "5"),
        ("(if (< a 6) 42 27)", "42"),
        ("(if (> a 13) 65 99)", "99"),
        ("(if (= a 5) 0 1)", "0"),
    ],
    [
        ("(define a 42.1)", None),
        ("(define b 37)", None),
        ("(- a b)", "5.100000000000001"),
        ("(list 5 b 7.3 (- b a))", "(5 37 7.3 -5.100000000000001)"),
    ],
    [
        ("(define x 42)", None),
        ("(define f (lambda (x y) (+ (* 2 x) y)))", None),
        ("(f 4 5)", "13"),
        ("(define a 5)", None),
        ("(f (f 4 a) (+ 3 2))", "31"),
        ("x", "42"),
    ],
    [
        ("(define a 4)", None),
        ("(define x 100)", None),
        ("(define f (lambda (x) (+ 3 5) (+ a x)))", None),
        ("(f 3)", "7"),
        ("(define a 21)", None),
        ("(f 4)", "25"),
    ],
    [
        ("(define a 3)", None),
        ("(begin (define a 5) (+ a 9))", "14"),
        ("a", "5"),
    ],
    [
        ("(define a 3)", None),
        ("(let ((a 17) (b 4)) (+ a b) (* a b))", "68"),
        ("a", "3"),
        ("(let ((b 42)) (define a b) a)", "42"),
        ("a", "3"),
    ],
    [
        (
            """
            (define fact
                (lambda (n) (
                    if (< n 2)
                    1
                    (* n (fact (- n 1))))))
            """,
            None,
        ),
        ("(fact 11)", "39916800"),
    ],
    [
        (
            """
            (define (fib n) (
                if (< n 2)
                n
                (+ (fib (- n 1)) (fib (- n 2)))))
            """,
            None,
        ),
        ("(fib 20)", "6765"),
    ],
    [
        (
            """
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
            """,
            None,
        ),
        ("(sqrt 2)", "1.4142156862745097"),
    ],
    [
        ("(define (square x) (* x x))", None),
        ("(define (average x y) (/ (+ x y) 2))", None),
        (
            """
            (define (good-enough? guess x)
                (< (abs (- (square guess) x)) 0.001))
            """,
            None,
        ),
        (
            """
            (define (improve guess x)
                (average guess (/ x guess)))
            """,
            None,
        ),
        (
            """
            (define (sqrt-iter guess x)
                (if (good-enough? guess x)
                    guess
                    (sqrt-iter (improve guess x) x)))
            """,
            None,
        ),
        (
            """
            (define (sqrt x)
                (sqrt-iter 1.0 x))
            """,
            None,
        ),
        ("(sqrt 2)", "1.4142156862745097"),
    ],
    [
        ("(define a (list 1 2 3 4))", None),
        ("(map (lambda (x) (* 2 x)) a)", "(2 4 6 8)"),
        ("(filter odd? a)", "(1 3)"),
        ("(apply + (filter even? a))", "6"),
        ("(define (f x y) (+ (* 2 x) y))", None),
        ("(reduce f -1 a)", "10"),
    ],
    [
        ("(min 4 8 -3)", "-3"),
        ("(max 4 8 -3)", "8"),
        ("(min 4 8 -3.2)", "-3.2"),
        ("(max 4 8.1 -3)", "8.1"),
    ],
    [
        ("(not #t)", "#f"),
        ("(not #f)", "#t"),
        ("(or #t #f)", "#t"),
        ("(or #f #f)", "#f"),
        ("(and #t #f)", "#f"),
        ("(and #t #t)", "#t"),
    ],
]


@pytest.mark.parametrize("code_value_pairs", TEST_CASES)
def test_eval(code_value_pairs: List[Tuple[str, Any]]):
    env = Environment.create_standard().create_child()

    for code, expected in code_value_pairs:
        exprs = parse(code)
        assert len(exprs) == 1, f"Parsing '{code}' should result in a single expression"
        val = env.eval(exprs[0])
        got = external_repr(val) if val is not None else None
        assert (got is None and expected is None) or (
            got == expected
        ), f"Evaluating '{code}': expected {expected}, got {got}"
