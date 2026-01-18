use loscheme::stack_vm::vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        // main:
        LoadConst { offset: 0 }, // argument for fib
        Call { addr: 3 },
        Halt,
        // fib:
        StackAlloc { size: 1 },   // make room for 1 local variable
        LoadConst { offset: 1 },  // push 1
        LoadLocal { offset: -3 }, // put n on the stack
        LessThan,
        JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
        LoadLocal { offset: -3 },
        Ret, // otherwise return n
        LoadLocal { offset: -3 },
        LoadConst { offset: 1 }, // recursive case
        Sub,
        Call { addr: 3 },
        StoreLocal { offset: 0 },
        LoadLocal { offset: -3 },
        LoadConst { offset: 2 },
        Sub,
        Call { addr: 3 },
        LoadLocal { offset: 0 },
        Add,
        Ret,
    ];

    let mut vm = VM::new(code, vec![Int(40), Int(1), Int(2)]);
    vm.run();
    println!("{:?}", vm.clone_stack_top().unwrap());
}
