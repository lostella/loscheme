import argparse
import math
from ast import literal_eval
from typing import Union


def tokenize(code: str):
    """
    Break the given Scheme code into a sequence of tokens to be parsed.
    """
    return code.replace("(", " ( ").replace(")", " ) ").split()


Expression = Union[str, int, float, list]


simple_literals = {
    "#t": True,
    "#f": False,
}


def parse_symbol_or_literal(token):
    if token in simple_literals:
        return simple_literals[token]
    try:
        value = literal_eval(token)
        return value
    except Exception:
        return token  # it's just a symbol


def parse_tokens_single(tokens: list) -> Expression:
    """
    Parse a sequence of tokens into an expression.
    """
    if len(tokens) == 0:
        raise SyntaxError("Unexpected EOF")
    token = tokens.pop(0)
    if token == "(":
        expression = []
        while tokens[0] != ")":
            expression.append(parse_tokens_single(tokens))
        tokens.pop(0)  # Discard the closing ')'
        return expression
    elif token == ")":
        raise SyntaxError("Unexpected closing parenthesis")
    else:
        return parse_symbol_or_literal(token)


def parse_tokens_multi(tokens: list) -> list:
    """
    Parse a sequence of tokens into a list of expressions.
    """
    expressions = []
    while tokens:
        expressions.append(parse_tokens_single(tokens))
    return expressions


def parse(code: str) -> list:
    """
    Parse the given Scheme code into a list of expressions.
    """
    tokens = tokenize(code)
    return parse_tokens_multi(tokens)


class Procedure:
    def __init__(self, params, body, env):
        self.params = params
        self.body = body
        self.env = env

    def __call__(self, args):
        local_env = self.env.create_child()
        for param, arg in zip(self.params, args):
            local_env.set(param, arg)
        value = local_env.eval(self.body[0])
        for expr in self.body[1:]:
            value = local_env.eval(expr)
        return value


def builtin_add(args):
    return sum(args)


def builtin_mult(args):
    return math.prod(args)


def builtin_sub(args):
    res = args[0]
    for arg in args[1:]:
        res -= arg
    return res


def builtin_div(args):
    res = args[0]
    for arg in args[1:]:
        res /= arg
    return res


def builtin_lt(args):
    return args[0] < args[1]


def builtin_gt(args):
    return args[0] > args[1]


def builtin_eq(args):
    return args[0] == args[1]


def builtin_abs(args):
    return abs(args[0])


def builtin_square(args):
    return args[0] ** 2


class Environment:
    def __init__(self, parent=None):
        self.parent = parent
        self.variables = {}

    def get(self, name):
        if name in self.variables:
            return self.variables[name]
        if self.parent is not None:
            return self.parent.get(name)
        raise ValueError(f"Undefined variable: {name}")

    def set(self, name, value):
        self.variables[name] = value

    def create_child(self) -> "Environment":
        return Environment(self)

    @classmethod
    def create_standard(cls) -> "Environment":
        env = cls()
        env.set("+", builtin_add)
        env.set("*", builtin_mult)
        env.set("-", builtin_sub)
        env.set("/", builtin_div)
        env.set("<", builtin_lt)
        env.set(">", builtin_gt)
        env.set("=", builtin_eq)
        env.set("abs", builtin_abs)
        return env

    def eval(self, expr: Expression):
        """
        Evaluate an expression in the given environment.
        """
        if isinstance(expr, str):
            return self.get(expr)

        if isinstance(expr, (int, float)):
            return expr

        assert isinstance(expr, list)

        if expr[0] == "define":
            _, binding, *body = expr
            if isinstance(binding, str):
                assert len(body) == 1
                self.set(binding, self.eval(body[0]))
            else:
                assert isinstance(binding, list)
                name, *params = binding
                self.set(name, Procedure(params, body, self))
            return None

        if expr[0] == "lambda":
            _, params, *body = expr
            return Procedure(params, body, self)

        if expr[0] == "if":
            _, cond, branch_true, branch_false = expr
            if self.eval(cond):
                return self.eval(branch_true)
            return self.eval(branch_false)

        if expr[0] == "let":
            _, inits, *body = expr
            local_env = self.create_child()
            for symbol, subexpr in inits:
                local_env.set(symbol, local_env.eval(subexpr))
            value = local_env.eval(body[0])
            for subexpr in body[1:]:
                value = local_env.eval(subexpr)
            return value

        if expr[0] == "begin":
            value = self.eval(expr[1])
            for subexpr in expr[2:]:
                value = self.eval(subexpr)
            return value

        procedure = self.eval(expr[0])
        args = [self.eval(arg) for arg in expr[1:]]
        return procedure(args)


def repl():
    env = Environment.create_standard().create_child()

    while True:
        try:
            code = input("λscm> ")
        except KeyboardInterrupt:
            print()
            continue
        except EOFError:
            break

        expressions = parse(code)

        for expr in expressions:
            try:
                result = env.eval(expr)
                print(result)
            except Exception as err:
                print(err)
                break


def run(path: str):
    env = Environment.create_standard().create_child()

    with open(path, "r") as f:
        code = f.read()

    expressions = parse(code)

    for expr in expressions:
        try:
            result = env.eval(expr)
            print(result)
        except Exception as err:
            print(err)
            break


def main():
    parser = argparse.ArgumentParser(prog="loscheme")
    parser.add_argument("--path", help="path to the script to execute", default=None)
    args = parser.parse_args()

    if args.path is None:
        repl()
    else:
        run(args.path)
