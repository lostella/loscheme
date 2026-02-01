use loscheme::parser::parse;
use loscheme::stack_vm::compiler::Compiler;

fn main() {
    let exprs = parse(
        r#"
        (define fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 40)
    "#,
    )
    .unwrap();
    let mut compiler = Compiler::new();
    let mut vm = compiler.compile(&exprs).unwrap();
    // println!("{}", vm);
    vm.run();
    println!("{}", vm.clone_stack_top().unwrap());
}
