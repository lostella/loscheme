use loscheme::stack_vm::{Instruction, Value, VM};

fn main() {
    let code = vec![
        // main:
        Instruction::Push(Value::Int(30)), // argument for fib
        Instruction::Call(3, 1),           // call fib
        Instruction::Halt,                 // halt
        // fib:
        Instruction::Push(Value::Int(1)), // push 1
        Instruction::LoadArg(0),          // put n on the stack
        Instruction::JumpLessThan(3),     // if 1 < n, jump to recursive case
        Instruction::LoadArg(0),          // put n on the stack
        Instruction::Ret,                 // return n
        Instruction::LoadArg(0),
        Instruction::Push(Value::Int(1)),
        Instruction::Sub,
        Instruction::Call(3, 1), // fib(n - 1)
        Instruction::LoadArg(0),
        Instruction::Push(Value::Int(2)),
        Instruction::Sub,
        Instruction::Call(3, 1), // fib(n - 2)
        Instruction::Add,
        Instruction::Ret,
    ];

    let mut vm = VM::new(code);
    let _ = vm.run(false);
    // println!("{:?}", res);
}
