import pytest

from loscheme import Environment, parse


CODE_VALUES = [
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
        """,
        [None, None, 5.100000000000001],
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
        (define (fact n) (
            if (< n 2)
                1
                (* n (fact (- n 1)))))
        (fact 6)
        """,
        [None, 720],
    ),
]


@pytest.mark.parametrize("code, values", CODE_VALUES)
def test_eval(code: str, values: list):
    env = Environment.create_standard().create_child()
    expressions = parse(code)
    assert len(expressions) == len(values)

    for expr, value in zip(expressions, values):
        assert env.eval(expr) == value
