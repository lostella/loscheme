use loscheme::stack_vm::vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        Jump { addr: 20 },
        // fib:
        StackAlloc { size: 1 },   // make room for 1 local variable
        LoadLocal { offset: -3 }, // put n on the stack
        Push { value: Int(1) },   // push 1
        LessThan,
        JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
        LoadLocal { offset: -3 },
        Ret,                    // otherwise return n
        Push { value: Int(1) }, // recursive case
        LoadLocal { offset: -3 },
        Sub,
        Call { addr: 1 },
        StoreLocal { offset: 0 },
        Push { value: Int(2) },
        LoadLocal { offset: -3 },
        Sub,
        Call { addr: 1 },
        LoadLocal { offset: 0 },
        Add,
        Ret,
        // main:
        Push { value: Int(35) }, // argument for fib
        Call { addr: 1 },
    ];

    let mut vm = VM::new(code, 0);
    vm.run().unwrap();
    println!("{:?}", vm.clone_stack_top().unwrap());
}
