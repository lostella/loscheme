use loscheme::vm::*;

fn main() {
    let code = vec![
        Instruction::LoadLocal(0),
        Instruction::LoadConstant(1),
        Instruction::Lt,
        Instruction::JumpIfFalse(2),
        Instruction::LoadLocal(0),
        Instruction::Return,
        Instruction::LoadConstant(0),
        Instruction::LoadLocal(0),
        Instruction::Subtract(2),
        Instruction::CallClone(1),
        Instruction::LoadConstant(1),
        Instruction::LoadLocal(0),
        Instruction::Subtract(2),
        Instruction::CallClone(1),
        Instruction::Add(2),
        Instruction::Return,
    ];
    let constants = vec![Value::Integer(1), Value::Integer(2)];
    let locals = vec![Value::Integer(30)];
    let mut frame = StackFrame::new(&code, &constants, locals);
    let res = frame.run();
    println!("{:?}", res);
}
