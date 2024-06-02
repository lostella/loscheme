import math
from ast import literal_eval
from typing import Union


def tokenize(code: str):
    """
    Break the given Scheme code into a sequence of tokens to be parsed.
    """
    return code.replace("(", " ( ").replace(")", " ) ").split()


Expression = Union[str, int, list]


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
        try:
            value = literal_eval(token)
            return value
        except:
            return token  # it's just a symbol


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
        env = Environment(self.env)
        for param, arg in zip(self.params, args):
            env.set(param, arg)
        return evaluate_expression(self.body, env)


class Environment:
    def __init__(self, parent=None):
        self.parent = parent
        self.variables = {}

    def get(self, name):
        if name in self.variables:
            return self.variables[name]
        elif self.parent is not None:
            return self.parent.get(name)
        else:
            raise ValueError(f"Undefined variable: {name}")

    def set(self, name, value):
        self.variables[name] = value

    def create_child(self) -> "Environment":
        return Environment(self)

    @classmethod
    def standard_environment(cls) -> "Environment":
        env = cls()
        env.set("+", lambda args: sum(args))
        env.set("*", lambda args: math.prod(args))
        env.set("<", lambda args: args[0] < args[1])
        env.set(">", lambda args: args[0] > args[1])
        env.set("=", lambda args: args[0] == args[1])
        return env


def evaluate_expression(expression: Expression, env: Environment):
    """
    Evaluate an expression in the given environment.
    """
    if isinstance(expression, str):
        return env.get(expression)
    elif isinstance(expression, int):
        return expression
    elif expression[0] == "define":
        _, var, value = expression
        env.set(var, evaluate_expression(value, env))
    elif expression[0] == "lambda":
        _, params, body = expression
        return Procedure(params, body, env)
    elif expression[0] == "if":
        _, cond, branch_true, branch_false = expression
        if evaluate_expression(cond, env):
            return evaluate_expression(branch_true, env)
        return evaluate_expression(branch_false, env)
    else:
        procedure = evaluate_expression(expression[0], env)
        args = [evaluate_expression(arg, env) for arg in expression[1:]]
        return procedure(args)


def repl():
    env = Environment.standard_environment().create_child()
    while True:
        code = input(">>> ")
        if code == "(exit)":
            break
        expressions = parse(code)
        for expression in expressions:
            try:
                result = evaluate_expression(expression, env)
                print(result)
            except Exception as err:
                print(err)
                break
