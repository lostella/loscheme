import textwrap

import pytest

from loscheme import Environment, parse, evaluate_expression


@pytest.mark.parametrize(
    "code, values",
    [
        (
            textwrap.dedent(
            """
            (define a 3)
            (define b 4)
            (+ a b)
            (define a 5)
            a
            """
            ),
            [None, None, 7, None, 5],
        ),
        (
            textwrap.dedent(
            """
            (define f (lambda (x y) (+ (* 2 x) y)))
            (f 4 5)
            (define a 5)
            (f (f 4 a) (+ 3 2))
            """
            ),
            [None, 13, 31],
        ),
    ],
)
def test_eval(code: str, values: list):
    env = Environment.standard_environment().create_child()
    expressions = parse(code)
    assert len(expressions) == len(values)

    for expression, value in zip(expressions, values):
        result = evaluate_expression(expression, env)
        assert result == value
