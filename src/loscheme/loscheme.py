import argparse
import math
from ast import literal_eval
from dataclasses import dataclass
from typing import Union


def tokenize(code: str):
    """
    Break the given Scheme code into a sequence of tokens to be parsed.
    """
    return code.replace("(", " ( ").replace(")", " ) ").split()


@dataclass
class Symbol:
    name: str

    def __repr__(self):
        return self.name


Expression = Union[Symbol, int, float, list]


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
        return Symbol(token)  # it's just a symbol


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
        assert all(isinstance(param, Symbol) for param in params)
        self.params = params
        self.body = body
        self.env = env

    def __call__(self, args):
        assert len(args) == len(self.params), (
            f"Procedure takes {len(self.params)} parameters ("
            f"{', '.join(param.name for param in self.params)}), "
            f"but {len(args)} arguments were given"
        )
        local_env = self.env.create_child()
        for param, arg in zip(self.params, args):
            local_env.set(param.name, arg)
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
    return all(a < b for a, b in zip(args, args[1:]))


def builtin_gt(args):
    return all(a > b for a, b in zip(args, args[1:]))


def builtin_leq(args):
    return all(a <= b for a, b in zip(args, args[1:]))


def builtin_geq(args):
    return all(a >= b for a, b in zip(args, args[1:]))


def builtin_iseq(args):
    return id(args[0]) == id(args[1])


def builtin_isequal(args):
    return args[0] == args[1]


def builtin_abs(args):
    return abs(args[0])


def builtin_list(args):
    return list(args)


def builtin_cons(args):
    return [args[0], *args[1]]


def builtin_car(args):
    return args[0][0]


def builtin_cdr(args):
    return args[0][1:]


def builtin_isnull(args):
    return args[0] == []


def builtin_issymbol(args):
    return isinstance(args[0], Symbol)


def builtin_isatom(args):
    return not isinstance(args[0], list)


def builtin_isnumber(args):
    return isinstance(args[0], (int, float))


def builtin_isboolean(args):
    return isinstance(args[0], bool)


def builtin_islist(args):
    return isinstance(args[0], list)


def builtin_isinteger(args):
    if type(args[0]) is int:
        return True
    if isinstance(args[0], float):
        return args[0].is_integer()
    return False


def builtin_iszero(args):
    return args[0] == 0


def builtin_ispositive(args):
    return args[0] > 0


def builtin_isnegative(args):
    return args[0] < 0


def builtin_isodd(args):
    return args[0] % 2 == 1


def builtin_iseven(args):
    return args[0] % 2 == 0


def builtin_length(args):
    return len(args[0])


class Environment:
    def __init__(self, parent=None):
        self.parent = parent
        self.variables = {}

    def get(self, name: str):
        if self.is_special_form(name):
            raise ValueError(f"Cannot use special form as variable: {name}")
        if name in self.variables:
            return self.variables[name]
        if self.parent is not None:
            return self.parent.get(name)
        raise ValueError(f"Undefined variable: {name}")

    def set(self, name: str, value):
        if self.is_special_form(name):
            raise ValueError(f"Cannot use special form as variable: {name}")
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
        env.set("<=", builtin_leq)
        env.set(">=", builtin_geq)
        env.set("=", builtin_isequal)
        env.set("abs", builtin_abs)
        env.set("list", builtin_list)
        env.set("cons", builtin_cons)
        env.set("car", builtin_car)
        env.set("cdr", builtin_cdr)
        env.set("eq?", builtin_iseq)
        env.set("equal?", builtin_isequal)
        env.set("null?", builtin_isnull)
        env.set("atom?", builtin_isatom)
        env.set("symbol?", builtin_issymbol)
        env.set("number?", builtin_isnumber)
        env.set("boolean?", builtin_isboolean)
        env.set("list?", builtin_islist)
        env.set("integer?", builtin_isinteger)
        env.set("zero?", builtin_iszero)
        env.set("positive?", builtin_ispositive)
        env.set("negative?", builtin_isnegative)
        env.set("odd?", builtin_isodd)
        env.set("even?", builtin_iseven)
        env.set("length", builtin_length)
        return env

    @classmethod
    def is_special_form(cls, name: str) -> bool:
        return name in [
            "quote",
            "define",
            "lambda",
            "if",
            "let",
            "begin",
        ]

    def eval(self, expr: Expression):
        """
        Evaluate an expression in the given environment.
        """
        if isinstance(expr, Symbol):
            return self.get(expr.name)

        if isinstance(expr, (int, float, str)):
            return expr

        assert isinstance(expr, list)
        assert isinstance(expr[0], Symbol)

        if not self.is_special_form(expr[0].name):
            procedure = self.eval(expr[0])
            args = [self.eval(arg) for arg in expr[1:]]
            return procedure(args)

        if expr[0].name == "quote":
            _, value = expr
            return value

        if expr[0].name == "define":
            _, binding, *body = expr
            if isinstance(binding, Symbol):
                assert len(body) == 1
                self.set(binding.name, self.eval(body[0]))
            else:
                assert isinstance(binding, list) and isinstance(binding[0], Symbol)
                name, *params = binding
                self.set(name.name, Procedure(params, body, self))
            return None

        if expr[0].name == "lambda":
            _, params, *body = expr
            return Procedure(params, body, self)

        if expr[0].name == "if":
            _, cond, branch_true, branch_false = expr
            if self.eval(cond):
                return self.eval(branch_true)
            return self.eval(branch_false)

        if expr[0].name == "let":
            _, inits, *body = expr
            local_env = self.create_child()
            for symbol, subexpr in inits:
                assert isinstance(symbol, Symbol)
                local_env.set(symbol.name, local_env.eval(subexpr))
            value = local_env.eval(body[0])
            for subexpr in body[1:]:
                value = local_env.eval(subexpr)
            return value

        if expr[0].name == "begin":
            value = self.eval(expr[1])
            for subexpr in expr[2:]:
                value = self.eval(subexpr)
            return value


def external_repr(value) -> str:
    if isinstance(value, bool):
        return "#t" if value else "#f"
    if isinstance(value, (int, float)):
        return str(value)
    if isinstance(value, str):
        return f'"{value}"'
    if isinstance(value, list):
        return "(" + " ".join(external_repr(v) for v in value) + ")"
    return str(value)


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
                value = env.eval(expr)
                if value is not None:
                    print(external_repr(value))
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
            _ = env.eval(expr)
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
