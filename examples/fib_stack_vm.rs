use loscheme::stack_vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        // main:
        Push { value: Int(30) },    // argument for fib
        Call { addr: 3, nargs: 1 }, // call fib(20)
        Halt,                       // halt
        // fib:
        Push { value: Int(1) }, // push 1
        LoadLocal { idx: 0 },   // put n on the stack
        LessThan,
        JumpIfTrue { offset: 3 }, // if 1 < n, jump to recursive case
        LoadLocal { idx: 0 },     // put n on the stack
        Ret,                      // return n
        LoadLocal { idx: 0 },
        Push { value: Int(1) },
        Sub,
        Call { addr: 3, nargs: 1 }, // fib(n - 1)
        LoadLocal { idx: 0 },
        Push { value: Int(2) },
        Sub,
        Call { addr: 3, nargs: 1 }, // fib(n - 2)
        Add,
        Ret,
    ];

    let mut vm = VM::new(code);
    let res = vm.run();
    println!("{:?}", res);
}
