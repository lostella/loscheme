import pytest

from loscheme import Environment, parse, Symbol


CODE_VALUES = [
    (
        """
        42
        23.10
        #t
        #f
        """,
        [42, 23.1, True, False],
    ),
    (
        """
        (quote 3)
        (quote hello)
        (quote (1 2 3))
        (quote #f)
        (quote (f a b 42))
        """,
        [
            3,
            Symbol("hello"),
            [1, 2, 3],
            False,
            [Symbol("f"), Symbol("a"), Symbol("b"), 42],
        ],
    ),
    (
        """
        (define a (list 2 3))
        (define b (cons 1 a))
        (car b)
        (cdr b)
        """,
        [None, None, 1, [2, 3]],
    ),
    (
        """
        (eq? 1 1)
        (eq? (quote 1) (quote 1))
        (eq? (list 1 2 3) (list 1 2 3))
        (eq? 1 2)
        (eq? (quote 1) (quote 2))
        (equal? 1 1)
        (equal? (quote 1) (quote 1))
        (equal? (list 1 2 3) (list 1 2 3))
        (equal? 1 2)
        (equal? (quote 1) (quote 2))
        (equal? (list 1 2 3) (list 1 2 42))
        """,
        [True, True, False, False, False, True, True, True, False, False, False],
    ),
    (
        """
        (define a 3)
        (define b 4)
        (+ a b)
        (define a 5)
        a
        (if (< a 6) 42 27)
        (if (> a 13) 65 99)
        (if (= a 5) 0 1)
        """,
        [None, None, 7, None, 5, 42, 99, 0],
    ),
    (
        """
        (define a 42.1)
        (define b 37)
        (- a b)
        (list 5 b 7.3 (- b a))
        """,
        [None, None, 5.100000000000001, [5, 37, 7.3, -5.100000000000001]],
    ),
    (
        """
        (define x 42)
        (define f (lambda (x y) (+ (* 2 x) y)))
        (f 4 5)
        (define a 5)
        (f (f 4 a) (+ 3 2))
        x
        """,
        [None, None, 13, None, 31, 42],
    ),
    (
        """
        (define a 4)
        (define x 100)
        (define f (lambda (x) (+ 3 5) (+ a x)))
        (f 3)
        (define a 21)
        (f 4)
        """,
        [None, None, None, 7, None, 25],
    ),
    (
        """
        (define a 3)
        (begin (define a 5) (+ a 9))
        a
        """,
        [None, 14, 5],
    ),
    (
        """
        (define a 3)
        (let ((a 17) (b 4)) (+ a b) (* a b))
        a
        (let ((b 42)) (define a b) a)
        a
        """,
        [None, 68, 3, 42, 3],
    ),
    (
        """
        (define fact
            (lambda (n) (
                if (< n 2)
                   1
                   (* n (fact (- n 1))))))
        (fact 11)
        """,
        [None, 39916800],
    ),
    (
        """
        (define (fib n) (
            if (< n 2)
               n
               (+ (fib (- n 1)) (fib (- n 2)))))
        (fib 20)
        """,
        [None, 6765],
    ),
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
        (sqrt 2)
        """,
        [None, 1.4142156862745097],
    ),
    (
        """
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
        """,
        [None, None, None, None, None, None, 1.4142156862745097],
    ),
]


@pytest.mark.parametrize("code, values", CODE_VALUES)
def test_eval(code: str, values: list):
    env = Environment.create_standard().create_child()
    expressions = parse(code)
    assert len(expressions) == len(values)

    for expr, expected in zip(expressions, values):
        got = env.eval(expr)
        assert got == expected, f"Expected {expected}, got {got}"
