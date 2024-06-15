import pytest
from typing import Any, List, Tuple

from loscheme import Environment, parse, Symbol


TEST_CASES = [
    [
        ("42", 42),
        ("23.10", 23.10),
        ("#t", True),
        ("#f", False),
    ],
    [
        ("(quote 3)", 3),
        ("(quote hello)", Symbol("hello")),
        ("(quote (1 2 3))", [1, 2, 3]),
        ("(quote #f)", False),
        ("(quote (f a b 42))", [Symbol("f"), Symbol("a"), Symbol("b"), 42]),
        ("(quote ())", []),
    ],
    [
        ("(define a (list 2 3))", None),
        ("(define b (cons 1 a))", None),
        ("(car b)", 1),
        ("(cdr b)", [2, 3]),
    ],
    [
        ("(eq? 1 1)", True),
        ("(eq? (quote 1) (quote 1))", True),
        ("(eq? (list 1 2 3) (list 1 2 3))", False),
        ("(eq? 1 2)", False),
        ("(eq? (quote 1) (quote 2))", False),
        ("(equal? 1 1)", True),
        ("(equal? (quote 1) (quote 1))", True),
        ("(equal? (list 1 2 3) (list 1 2 3))", True),
        ("(equal? 1 2)", False),
        ("(equal? (quote 1) (quote 2))", False),
        ("(equal? (list 1 2 3) (list 1 2 42))", False),
    ],
    [
        ("(integer? 42)", True),
        ("(integer? 42.0)", True),
        ("(integer? 42.1)", False),
        ("(number? 42)", True),
        ("(number? 42.0)", True),
        ("(number? (list 1 2 3))", False),
        ("(null? (list))", True),
        ("(null? 7)", False),
        ("(null? #f)", False),
        ("(boolean? #t)", True),
        ("(boolean? #f)", True),
        ("(boolean? 13)", False),
        ("(symbol? (quote hello))", True),
        ("(symbol? +)", False),
        ("(symbol? 13)", False),
        ("(list? (list 1 2 3))", True),
        ("(list? (quote (1 2 3)))", True),
        ("(list? #f)", False),
        ("(list? (quote hello))", False),
        ("(list? 13.9)", False),
    ],
    [
        ("(zero? 0)", True),
        ("(zero? 0.0)", True),
        ("(zero? 1e-11)", False),
        ("(zero? 3)", False),
        ("(positive? 1e-11)", True),
        ("(positive? 4)", True),
        ("(positive? -2)", False),
        ("(positive? -1e-11)", False),
        ("(negative? 1e-11)", False),
        ("(negative? 4)", False),
        ("(negative? -2)", True),
        ("(negative? -1e-11)", True),
        ("(odd? 0)", False),
        ("(odd? 1)", True),
        ("(odd? -1)", True),
        ("(odd? 42)", False),
        ("(even? 0)", True),
        ("(even? 1)", False),
        ("(even? -42)", True),
        ("(even? -3)", False),
    ],
    [
        ("(< 1 2 3)", True),
        ("(< 1 2 2)", False),
        ("(<= 1 2 2)", True),
        ("(<= 1 2 1)", False),
        ("(> 2 1 0)", True),
        ("(> 2 1 1)", False),
        ("(>= 2 1 1)", True),
        ("(>= 2 1 2)", False),
    ],
    [
        ("(define a 3)", None),
        ("(define b 4)", None),
        ("(+ a b)", 7),
        ("(define a 5)", None),
        ("a", 5),
        ("(if (< a 6) 42 27)", 42),
        ("(if (> a 13) 65 99)", 99),
        ("(if (= a 5) 0 1)", 0),
    ],
    [
        ("(define a 42.1)", None),
        ("(define b 37)", None),
        ("(- a b)", 5.100000000000001),
        ("(list 5 b 7.3 (- b a))", [5, 37, 7.3, -5.100000000000001]),
    ],
    [
        ("(define x 42)", None),
        ("(define f (lambda (x y) (+ (* 2 x) y)))", None),
        ("(f 4 5)", 13),
        ("(define a 5)", None),
        ("(f (f 4 a) (+ 3 2))", 31),
        ("x", 42),
    ],
    [
        ("(define a 4)", None),
        ("(define x 100)", None),
        ("(define f (lambda (x) (+ 3 5) (+ a x)))", None),
        ("(f 3)", 7),
        ("(define a 21)", None),
        ("(f 4)", 25),
    ],
    [
        ("(define a 3)", None),
        ("(begin (define a 5) (+ a 9))", 14),
        ("a", 5),
    ],
    [
        ("(define a 3)", None),
        ("(let ((a 17) (b 4)) (+ a b) (* a b))", 68),
        ("a", 3),
        ("(let ((b 42)) (define a b) a)", 42),
        ("a", 3),
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
        ("(fact 11)", 39916800),
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
        ("(fib 20)", 6765),
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
        ("(sqrt 2)", 1.4142156862745097),
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
        ("(sqrt 2)", 1.4142156862745097),
    ],
]


@pytest.mark.parametrize("code_value_pairs", TEST_CASES)
def test_eval(code_value_pairs: List[Tuple[str, Any]]):
    env = Environment.create_standard().create_child()

    for code, expected in code_value_pairs:
        exprs = parse(code)
        assert len(exprs) == 1, f"Parsing '{code}' should result in a single expression"
        got = env.eval(exprs[0])
        assert got == expected, f"Evaluating '{expr}': expected {expected}, got {got}"
