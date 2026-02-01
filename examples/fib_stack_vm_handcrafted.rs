use loscheme::stack_vm::vm::{Instruction::*, Value::*, VM};

fn main() {
    let code = vec![
        // main:
        LoadConst { offset: 0 }, // load argument
        Call { addr: 3 },        // call fib
        Halt,
        // fib(n):  n is at LoadLocal { offset: -3 }
        StackAlloc { size: 1 },   // local[0] for fib(n-1) result
        PushOne,                  // push 1
        LoadLocal { offset: -3 }, // push n
        LessThan,                 // 1 < n ?
        JumpIfTrue { offset: 2 }, // if true, jump to recursive case
        LoadLocal { offset: -3 }, // push n
        Ret,                      // return n (base case)
        // recursive case:
        LoadLocal { offset: -3 }, // push n
        PushOne,                  // push 1
        Sub,                      // n - 1
        Call { addr: 3 },         // fib(n-1)
        StoreLocal { offset: 0 }, // local[0] = fib(n-1)
        LoadLocal { offset: -3 }, // push n
        LoadConst { offset: 1 },  // push 2
        Sub,                      // n - 2
        Call { addr: 3 },         // fib(n-2)
        LoadLocal { offset: 0 },  // push fib(n-1)
        Add,                      // fib(n-1) + fib(n-2)
        Ret,
    ];

    let mut vm = VM::new(code, vec![Int(40), Int(2)]);
    vm.run();
    println!("{}", vm.clone_stack_top().unwrap());
}
