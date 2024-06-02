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
            """
            ),
            [None, None, 7],
        ),
        (
            textwrap.dedent(
                """
                (define f (lambda (x y) (+ (2 * x) y)))
                (f 4 5)
            """
            ),
            [None, 13],
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
