module LoScheme

function tokenize(code::String)
    pattern = r""""(?:[^"\\]|\\.)*"|[()]|'|[^\s()'"]+"""
    [m.match for m in eachmatch(pattern, code)]
end

const simple_literals = Dict("#t" => true, "#f" => false)

function try_parse(tpe, token)
    try
        val = parse(tpe, token)
        return val
    catch
        return nothing
    end
end

function parse_symbol_or_literal(token)
    token in keys(simple_literals) && return simple_literals[token]
    if token[1] == token[end] == '"'
        return token[2:end-1]
    end
    for tpe in [Int, Float64]
        val = try_parse(tpe, token)
        val !== nothing && return val
    end
    return Symbol(token)
end

function parse_tokens_single(tokens)
    if length(tokens) == 0
        throw(ErrorException("Unexpected: EOF"))
    end
    token = popfirst!(tokens)
    if token == "("
        expression = []
        while tokens[1] != ")"
            push!(expression, parse_tokens_single(tokens))
        end
        popfirst!(tokens)
        return expression
    elseif token == ")"
        throw(ErrorException("Unexpected: )"))
    elseif token == "'"
        return [Symbol("quote"), parse_tokens_single(tokens)]
    end
    parse_symbol_or_literal(token)
end

function parse_tokens_multi(tokens)
    expressions = []
    while length(tokens) > 0
        push!(expressions, parse_tokens_single(tokens))
    end
    expressions
end

parse_code(code) = parse_tokens_multi(tokenize(code))

external_repr(sym::Symbol) = String(sym)

external_repr(value::Bool) = value ? "#t" : "#f"

external_repr(value::Number) = string(value)

external_repr(value::AbstractString) = "\"$(value)\""

external_repr(value::Vector) = string("(", join(map(external_repr, value), " "), ")")

external_repr(::Nothing) = nothing

struct Environment
    parent::Union{Environment,Nothing}
    variables::Dict
end

Environment() = Environment(nothing, Dict())

Environment(parent) = Environment(parent, Dict())

is_special_form(sym::Symbol) = sym in [:quote, :eval, :lambda, :define, :if, :let, :begin]

function get(environment::Environment, key::Symbol)
    @assert !is_special_form(key)
    key in keys(environment.variables) && return environment.variables[key]
    environment.parent !== nothing && return get(environment.parent, key)
    throw(ErrorException("Undefined: $(key)"))
end

get(env::Environment, key::String) = get(env, Symbol(key))

function set(environment::Environment, key::Symbol, value)
    @assert !is_special_form(key)
    environment.variables[key] = value
end

set(env::Environment, key::String, value) = set(env, Symbol(key), value)

evaluate(env::Environment, sym::Symbol) = get(env, sym)

evaluate(env::Environment, val::Union{Number,AbstractString}) = val

function evaluate(env::Environment, expr::Vector)
    if !isa(expr[1], Symbol) || !is_special_form(expr[1])
        proc = evaluate(env, expr[1])
        args = [evaluate(env, arg) for arg in expr[2:end]]
        return proc(args...)
    end

    if expr[1] == :quote
        @assert length(expr) == 2
        return expr[2]
    end

    if expr[1] == :eval
        @assert length(expr) == 2
        return evaluate(env, evaluate(env, expr[2]))
    end

    if expr[1] == :lambda
        _, params, body... = expr
        return Procedure(params, body, env)
    end

    if expr[1] == :define
        _, binding, body... = expr
        if isa(binding, Symbol)
            @assert length(body) == 1
            set(env, binding, evaluate(env, body[1]))
        else
            @assert isa(binding, Vector) && isa(binding[1], Symbol)
            sym, params... = binding
            set(env, sym, Procedure(params, body, env))
        end
        return nothing
    end

    if expr[1] == :if
        _, cond, branch_true, branch_false = expr
        if evaluate(env, cond)
            return evaluate(env, branch_true)
        end
        return evaluate(env, branch_false)
    end

    if expr[1] == :let
        _, inits, body... = expr
        local_env = Environment(env)
        for (sym, subexpr) in inits
            @assert isa(sym, Symbol)
            set(local_env, sym, evaluate(local_env, subexpr))
        end
        val = evaluate(local_env, body[1])
        for subexpr in body[2:end]
            val = evaluate(local_env, subexpr)
        end
        return val
    end

    if expr[1] == :begin
        val = evaluate(env, expr[2])
        for subexpr in expr[3:end]
            val = evaluate(env, subexpr)
        end
        return val
    end

    throw(ErrorException("Not implemented: $(expr[1])"))
end

struct Procedure
    params::Vector{Symbol}
    body::Vector
    env::Environment

    function Procedure(params, body, env)
        @assert all(map(x -> isa(x, Symbol), params))
        new(params, body, env)
    end
end

function (proc::Procedure)(args...)
    @assert length(args) == length(proc.params)
    local_env = Environment(proc.env)
    for (param, arg) in zip(proc.params, args)
        set(local_env, param, arg)
    end
    value = evaluate(local_env, proc.body[1])
    for expr in proc.body[2:end]
        value = evaluate(local_env, expr)
    end
    return value
end


function repl() # TODO fix
    env = Environment(standard_env())

    while true
        print("λscm> ")
        code = readline()
        expressions = parse_code(code)
        val = nothing
        for expr in expressions
            val = evaluate(env, expr)
        end
        if val !== nothing
            println(external_repr(val))
        end
    end
end


function run(path)
    env = Environment(standard_env())
    code = readlines(path)
    expressions = parse_code(code)

    for expr in expressions
        evaluate(env, expr)
    end
end


function standard_env()
    env = Environment()
    set(env, "+", +)
    set(env, "*", *)
    set(env, "-", -)
    set(env, "/", /)
    set(env, "<", builtin_lt)
    set(env, ">", builtin_gt)
    set(env, "<=", builtin_leq)
    set(env, ">=", builtin_geq)
    set(env, "=", ==)
    set(env, "abs", abs)
    set(env, "list", builtin_list)
    set(env, "cons", builtin_cons)
    set(env, "car", builtin_car)
    set(env, "cdr", builtin_cdr)
    set(env, "eq?", ===)
    set(env, "equal?", ==)
    set(env, "null?", builtin_isnull)
    set(env, "atom?", builtin_isatom)
    set(env, "symbol?", builtin_issymbol)
    set(env, "number?", builtin_isnumber)
    set(env, "boolean?", builtin_isboolean)
    set(env, "list?", builtin_islist)
    set(env, "pair?", builtin_ispair)
    set(env, "integer?", builtin_isinteger)
    set(env, "zero?", iszero)
    set(env, "positive?", builtin_ispositive)
    set(env, "negative?", builtin_isnegative)
    set(env, "odd?", isodd)
    set(env, "even?", iseven)
    set(env, "length", builtin_length)
    set(env, "reverse", builtin_reverse)
    set(env, "map", map)
    set(env, "filter", filter)
    set(env, "apply", builtin_apply)
    set(env, "reduce", builtin_reduce)
    set(env, "append", builtin_append)
    set(env, "min", min)
    set(env, "max", max)
    set(env, "not", !)
    set(env, "or", builtin_or)
    set(env, "and", builtin_and)
    env
end

builtin_lt(args...) = all(a < b for (a, b) in zip(args, args[2:end]))

builtin_gt(args...) = all(a > b for (a, b) in zip(args, args[2:end]))

builtin_leq(args...) = all(a <= b for (a, b) in zip(args, args[2:end]))

builtin_geq(args...) = all(a >= b for (a, b) in zip(args, args[2:end]))

builtin_list(args...) = collect(args)

builtin_cons(arg1, arg2) = (arg1, arg2)
builtin_cons(arg1, arg2::Vector) = cat([arg1], arg2, dims = 1)

builtin_car(args...) = args[1][1]

builtin_cdr(arg::Vector) = arg[2:end]
function builtin_cdr(arg::Tuple)
    @assert length(arg) == 2
    arg[2]
end

builtin_isnull(_) = false
builtin_isnull(v::Vector) = length(v) == 0

builtin_issymbol(_) = false
builtin_issymbol(::Symbol) = true

builtin_isatom(args...) = !isa(args[1], Vector)

builtin_isnumber(_) = false
builtin_isnumber(::Union{Int,AbstractFloat,Complex}) = true

builtin_isboolean(_) = false
builtin_isboolean(::Bool) = true

builtin_islist(_) = false
builtin_islist(::Vector) = true

builtin_ispair(_) = false
builtin_ispair(t::Tuple) = length(t) == 2
builtin_ispair(v::Vector) = length(v) > 0

builtin_isinteger(_) = false
builtin_isinteger(x::Union{Int,AbstractFloat,Complex}) = isinteger(x)

builtin_ispositive(x) = x > 0

builtin_isnegative(x) = x < 0

builtin_length(args...) = length(args[1])

builtin_reverse(args...) = reverse(args[1])

builtin_apply(f, args) = f(args...)

builtin_reduce(f, init, coll) = reduce(f, coll, init = init)

builtin_append(args...) = cat(args..., dims = 1)

builtin_or(args...) = any(args)
builtin_and(args...) = all(args)

end # module
