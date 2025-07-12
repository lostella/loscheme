use loscheme::parser::parse;
use loscheme::stack_vm::{compile, VM};

fn run_example(name: &str, code: &str) {
    println!("-------------------------- {name}");
    println!(" code: {code:?}");
    let exprs = parse(code).unwrap();
    println!("exprs: {exprs:?}");
    let instr = compile(&exprs).unwrap();
    println!("instr: {instr:?}");
    let mut vm = VM::new(instr);
    let res = vm.run();
    println!("stack: {:?}", vm.clone_stack());
    println!("  res: {res:?}");
}

fn main() {
    run_example("#t", "#t");
    run_example("#f", "#f");
    run_example("42", "42");
    run_example("if", "(if #t 1 0)");
    run_example("if_cmp", "(if (< 3 2) 1 0)");
    run_example("if_cmp_add_sub", "(if (< 3 2) (+ 1 4) (- 3 7))");
    run_example("begin_define", "(begin (define x 3) x)");
    run_example("lambda", "(lambda (x) (+ x 1))");
    run_example(
        "fib",
        "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)",
    );
}
