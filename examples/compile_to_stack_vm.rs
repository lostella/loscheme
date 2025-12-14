use loscheme::parser::parse;
use loscheme::stack_vm::Compiler;

fn run_example(name: &str, code: &str) {
    println!("-------------------------- {name}");
    println!(" code: {code:?}");
    let exprs = parse(code).unwrap();
    println!("exprs: {exprs:?}");
    let mut compiler = Compiler::new();
    let mut vm = compiler.compile(&exprs).unwrap();
    vm.run().unwrap();
    println!("stack: {:?}", vm.clone_stack());
    println!("  res: {:?}", vm.clone_stack_top());
}

fn main() {
    run_example("#t", "#t");
    run_example("#f", "#f");
    run_example("42", "42");
    run_example("if", "(if #t 1 0)");
    run_example("if_cmp", "(if (< 3 2) 1 0)");
    run_example("if_cmp_add_sub", "(if (< 3 2) (+ 1 4) (- 3 7))");
    run_example("begin_add_sub_if", "(begin (+ 3 4) (- 7 (if (< 4 5) 1 0)))");
    run_example("begin_define", "(begin (define x 3) x)");
    run_example("let_1", "(let ((a 3)) (+ a 1))");
    run_example("let_2", "(let ((a 3) (b 4)) (+ a b))");
    run_example("lambda", "(lambda (x) (+ x 1))");
    run_example(
        "fib",
        "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
    );
}
