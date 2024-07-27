using LoScheme: tokenize, Symbol, parse_symbol_or_literal, parse_code, external_repr
using LoScheme: Environment, get, set, evaluate, standard_env
using Test

@testset "Tokenizer" begin
    @test tokenize("(define a 3)") == ["(", "define", "a", "3", ")"]
    @test tokenize("(define a \"hello\")") == ["(", "define", "a", "\"hello\"", ")"]
end

@testset "Parse" begin
    @test parse_symbol_or_literal("42") == 42
    @test parse_symbol_or_literal("#t") == true
    @test parse_symbol_or_literal("#f") == false
    @test parse_symbol_or_literal("3.14") == 3.14
    @test parse_symbol_or_literal("\"a\"") == "a"
    @test parse_symbol_or_literal("a") == Symbol("a")

    @test parse_code("(define a 3)") == [[Symbol("define"), Symbol("a"), 3]]
    @test parse_code("(+ 2 (* a 6))") == [[Symbol("+"), 2, [Symbol("*"), Symbol("a"), 6]]]
end

@testset "External repr" begin
    @test external_repr(42) == "42"
    @test external_repr(3.14) == "3.14"
    @test external_repr(true) == "#t"
    @test external_repr(false) == "#f"
    @test external_repr("hello") == "\"hello\""
    @test external_repr([3.14, 42, "hello"]) == "(3.14 42 \"hello\")"
end

@testset "Environment" begin
    env1 = Environment()
    set(env1, :a, 42)
    env2 = Environment(env1)
    set(env2, :b, 13)

    @test get(env1, :a) == 42
    @test_throws ErrorException get(env1, :b)
    @test get(env2, :a) == 42
    @test get(env2, :b) == 13
end

const test_cases = [
    [
        ("42", "42"),
        ("23.10", "23.1"),
        ("#t", "#t"),
        ("#f", "#f"),
        ("\"hello world!\"", "\"hello world!\""),
    ],
    [
        ("(quote 3)", "3"),
        ("(quote hello)", "hello"),
        ("(quote (1 2 3))", "(1 2 3)"),
        ("(quote #f)", "#f"),
        ("(quote (f a b 42))", "(f a b 42)"),
        ("(quote ())", "()"),
        ("(quote \"hello world!\")", "\"hello world!\""),
    ],
    [
        ("'3", "3"),
        ("'hello", "hello"),
        ("'(1 2 3)", "(1 2 3)"),
        ("'#f", "#f"),
        ("'(f a b 42)", "(f a b 42)"),
        ("'()", "()"),
        ("'\"hello world!\"", "\"hello world!\""),
    ],
    [("(define (f x) (* 3 x))", nothing), ("'(f 4)", "(f 4)"), ("(eval '(f 4))", "12")],
    [
        ("(atom? 42)", "#t"),
        ("(atom? 'hello)", "#t"),
        ("(atom? '(1 2 3))", "#f"),
        ("(atom? '42)", "#t"),
        ("(atom? #f)", "#t"),
        ("(atom? '())", "#f"),
        ("(atom? (list 1 2 3))", "#f"),
        ("(atom? (+ 1 2 3))", "#t"),
        ("(atom? '(+ 1 2 3))", "#f"),
        ("(atom? \"hello world!\")", "#t"),
        ("(atom? \'\"hello world!\")", "#t"),
    ],
    [
        ("(define a (list 2 3))", nothing),
        ("(define b (cons 1 a))", nothing),
        ("(car b)", "1"),
        ("(cdr b)", "(2 3)"),
        ("(cdr (cons 1 2))", "2"),
        ("(cdr (list 1 2))", "(2)"),
        ("b", "(1 2 3)"),
        ("(length b)", "3"),
        ("(length (cdr b))", "2"),
        ("(reverse (list 1 2 3))", "(3 2 1)"),
        ("(append '(1 2 3) '(4) '() '(5 6))", "(1 2 3 4 5 6)"),
    ],
    [
        ("(eq? 1 1)", "#t"),
        ("(eq? '1 '1)", "#t"),
        ("(eq? (list 1 2 3) (list 1 2 3))", "#f"),
        ("(eq? 1 2)", "#f"),
        ("(eq? '1 '2)", "#f"),
        ("(equal? 1 1)", "#t"),
        ("(equal? '1 '1)", "#t"),
        ("(equal? (list 1 2 3) (list 1 2 3))", "#t"),
        ("(equal? 1 2)", "#f"),
        ("(equal? '1 '2)", "#f"),
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
        ("(symbol? 'hello)", "#t"),
        ("(symbol? +)", "#f"),
        ("(symbol? 13)", "#f"),
        ("(list? (list 1 2 3))", "#t"),
        ("(list? '(1 2 3))", "#t"),
        ("(list? #f)", "#f"),
        ("(list? 'hello)", "#f"),
        ("(list? 13.9)", "#f"),
        ("(list? (cons 1 2))", "#f"),
        ("(list? (cons 1 (list 2)))", "#t"),
        ("(pair? 42)", "#f"),
        ("(pair? (cons 1 2))", "#t"),
        ("(pair? (list 1 2))", "#t"),
        ("(pair? '())", "#f"),
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
        ("(define a 3)", nothing),
        ("(define b 4)", nothing),
        ("(+ a b)", "7"),
        ("(define a 5)", nothing),
        ("a", "5"),
        ("(if (< a 6) 42 27)", "42"),
        ("(if (> a 13) 65 99)", "99"),
        ("(if (= a 5) 0 1)", "0"),
    ],
    [
        ("(define a 42.1)", nothing),
        ("(define b 37)", nothing),
        ("(- a b)", "5.100000000000001"),
        ("(list 5 b 7.3 (- b a))", "(5 37 7.3 -5.100000000000001)"),
    ],
    [
        ("(define x 42)", nothing),
        ("(define f (lambda (x y) (+ (* 2 x) y)))", nothing),
        ("(f 4 5)", "13"),
        ("(define a 5)", nothing),
        ("(f (f 4 a) (+ 3 2))", "31"),
        ("x", "42"),
    ],
    [
        ("(define a 4)", nothing),
        ("(define x 100)", nothing),
        ("(define f (lambda (x) (+ 3 5) (+ a x)))", nothing),
        ("(f 3)", "7"),
        ("(define a 21)", nothing),
        ("(f 4)", "25"),
    ],
    [("(define a 3)", nothing), ("(begin (define a 5) (+ a 9))", "14"), ("a", "5")],
    [
        ("(define a 3)", nothing),
        ("(let ((a 17) (b 4)) (+ a b) (* a b))", "68"),
        ("a", "3"),
        ("(let ((b 42)) (define a b) a)", "42"),
        ("a", "3"),
    ],
    [(
        """
        (define fact
        	(lambda (n) (
        		if (< n 2)
        		1
        		(* n (fact (- n 1))))))
        """,
        nothing,
    ), ("(fact 11)", "39916800")],
    [(
        """
        (define (fib n) (
        	if (< n 2)
        	n
        	(+ (fib (- n 1)) (fib (- n 2)))))
        """,
        nothing,
    ), ("(fib 20)", "6765")],
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
            nothing,
        ),
        ("(sqrt 2)", "1.4142156862745097"),
    ],
    [
        ("(define (square x) (* x x))", nothing),
        ("(define (average x y) (/ (+ x y) 2))", nothing),
        (
            """
            (define (good-enough? guess x)
            	(< (abs (- (square guess) x)) 0.001))
            """,
            nothing,
        ),
        (
            """
            (define (improve guess x)
            	(average guess (/ x guess)))
            """,
            nothing,
        ),
        (
            """
            (define (sqrt-iter guess x)
            	(if (good-enough? guess x)
            		guess
            		(sqrt-iter (improve guess x) x)))
            """,
            nothing,
        ),
        (
            """
            (define (sqrt x)
            	(sqrt-iter 1.0 x))
            """,
            nothing,
        ),
        ("(sqrt 2)", "1.4142156862745097"),
    ],
    [
        ("(define a (list 1 2 3 4))", nothing),
        ("(map (lambda (x) (* 2 x)) a)", "(2 4 6 8)"),
        ("(filter odd? a)", "(1 3)"),
        ("(apply + (filter even? a))", "6"),
        ("(define (f x y) (+ (* 2 x) y))", nothing),
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

@testset "Evaluation" for code_value_pairs in test_cases
    env = standard_env()

    for (code, expected) in code_value_pairs
        exprs = parse_code(code)
        @test length(exprs) == 1
        val = evaluate(env, exprs[1])
        got = val === nothing ? nothing : external_repr(val)
        @test (got === nothing && expected === nothing) || got == expected
    end
end
