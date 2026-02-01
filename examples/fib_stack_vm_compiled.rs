use loscheme::parser::parse;
use loscheme::stack_vm::compiler::Compiler;
use loscheme::stack_vm::vm::VM;

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
    let program = compiler.compile(&exprs).unwrap();
    let mut vm = VM::new(program, 1024);
    vm.run();
    println!("{}", vm.represent_stack_top().unwrap());
}
