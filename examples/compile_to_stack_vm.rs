use loscheme::parser::parse;
use loscheme::stack_vm::compiler::Compiler;

fn run_example(code: &str) {
    let exprs = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let mut vm = compiler.compile(&exprs).unwrap();
    vm.debug();
}

fn main() {
    run_example("#t");
    run_example("#f");
    run_example("42");
    run_example("(if #t 1 0)");
    run_example("(if (< 3 2) 1 0)");
    run_example("(if (< 3 2) (+ 1 4) (- 3 7))");
    run_example("(begin (+ 3 4) (- 7 (if (< 4 5) 1 0)))");
    run_example("(begin (define x 3) x)");
    run_example("(let ((a 3)) (+ a 1))");
    run_example("(let ((a 3) (b 4)) (+ a b))");
    run_example("(lambda (x) (+ x 1))");
    run_example("(define a 3)");
    run_example("(define a 3) (define plus-a (lambda (x) (+ a 3)))");
    run_example("(define a 3) (define plus-a (lambda (x) (+ a x))) (plus-a 9)");
    run_example("(define a 3) (define plus-a (lambda (x) (+ a 3)))");
    run_example("(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)");
}
