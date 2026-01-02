use loscheme::stack_vm::vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        // main:
        Push { value: Int(35) }, // argument for fib
        Call { addr: 3 },
        Halt,
        // fib:
        StackAlloc { size: 1 },   // make room for 1 local variable
        Push { value: Int(1) },   // push 1
        LoadLocal { offset: -3 }, // put n on the stack
        LessThan,
        JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
        LoadLocal { offset: -3 },
        Ret, // otherwise return n
        LoadLocal { offset: -3 },
        Push { value: Int(1) }, // recursive case
        Sub,
        Call { addr: 3 },
        StoreLocal { offset: 0 },
        LoadLocal { offset: -3 },
        Push { value: Int(2) },
        Sub,
        Call { addr: 3 },
        LoadLocal { offset: 0 },
        Add,
        Ret,
    ];

    let mut vm = VM::new(code);
    vm.run().unwrap();
    println!("{:?}", vm.clone_stack_top().unwrap());
}
