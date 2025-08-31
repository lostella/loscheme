use loscheme::stack_vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        // main:
        Push { value: Int(30) }, // argument for fib
        Call { addr: 3 },        // call fib(20)
        Halt,                    // halt
        // fib:
        StackAlloc { size: 1 },
        Push { value: Int(1) },   // push 1
        LoadLocal { offset: -3 }, // put n on the stack
        LessThan,
        JumpIfTrue { offset: 3 }, // if 1 < n, jump to recursive case
        LoadLocal { offset: -3 }, // put n on the stack
        Ret,                      // return n
        LoadLocal { offset: -3 },
        Push { value: Int(1) },
        Sub,
        Call { addr: 3 }, // fib(n - 1)
        StoreLocal { offset: 0 },
        LoadLocal { offset: -3 },
        Push { value: Int(2) },
        Sub,
        Call { addr: 3 }, // fib(n - 2)
        LoadLocal { offset: 0 },
        Add,
        Ret,
    ];

    let mut vm = VM::new(code);
    vm.run().unwrap();
    println!("{:?}", vm.clone_stack_top().unwrap());
}
