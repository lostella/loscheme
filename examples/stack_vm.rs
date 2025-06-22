#[derive(Debug, Clone)]
pub enum Value {
    Pointer(usize),
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Push(Value),
    Pop,
    Add,
    Sub,
    LessThan,
    Jump(usize),
    JumpIfTrue(usize),
    Call(usize, usize), // target address, number of arguments
    LoadArg(usize),
    Ret,
    Print,
    Halt,
}

struct VM {
    code: Vec<Instruction>,
    stack: Vec<Value>,
    sp: usize,
    fp: usize,
    ip: usize,
}

impl VM {
    fn new(code: Vec<Instruction>) -> Self {
        Self {
            code,
            stack: Vec::with_capacity(1024),
            sp: 0,
            fp: 0,
            ip: 0,
        }
    }

    fn push(&mut self, val: Value) {
        if self.sp < self.stack.len() {
            self.stack[self.sp] = val;
        } else {
            self.stack.push(val);
        }
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp].clone()
    }

    fn run(&mut self) {
        while self.ip < self.code.len() {
            let instr = self.code[self.ip].clone();
            // println!(
            //     "fp: {}, stack: {:?}, instr: {:?}",
            //     self.fp,
            //     self.stack[..self.sp].to_vec(),
            //     instr,
            // );
            self.ip += 1;
            match instr {
                Instruction::Halt => break,
                Instruction::Push(v) => self.push(v),
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x + y)),
                        _ => panic!("Invalid operands for Add"),
                    }
                }
                Instruction::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x - y)),
                        _ => panic!("Invalid operands for Sub"),
                    }
                }
                Instruction::LessThan => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Int(x), Value::Int(y)) => {
                            self.push(Value::Bool(x < y));
                        }
                        _ => panic!("Invalid operands for LessThan"),
                    }
                }
                Instruction::Jump(addr) => {
                    self.ip = addr;
                }
                Instruction::JumpIfTrue(addr) => {
                    let cond = self.pop();
                    match cond {
                        Value::Bool(true) => self.ip = addr,
                        Value::Bool(false) => {}
                        _ => panic!("Invalid condition for JumpIfTrue"),
                    }
                }
                Instruction::Call(addr, nargs) => {
                    self.push(Value::Pointer(self.fp));
                    self.push(Value::Pointer(self.ip));
                    self.fp = self.sp - nargs - 2;
                    self.ip = addr;
                }
                Instruction::LoadArg(offset) => {
                    self.push(self.stack[self.fp + offset].clone());
                }
                Instruction::Ret => {
                    let ret = self.pop();
                    let ret_ip = match self.stack[self.fp + 1] {
                        Value::Pointer(i) => i,
                        _ => panic!("Invalid return address"),
                    };
                    let ret_fp = match self.stack[self.fp + 0] {
                        Value::Pointer(i) => i,
                        _ => panic!("Invalid frame pointer"),
                    };
                    self.sp = self.fp;
                    self.fp = ret_fp;
                    self.ip = ret_ip;
                    self.push(ret);
                }
                Instruction::Print => {
                    println!("{:?}", self.pop());
                }
            }
        }
    }
}

fn main() {
    let code = vec![
        // main:
        Instruction::Push(Value::Int(30)), // argument for fib
        Instruction::Call(4, 1),           // call fib(20)
        Instruction::Print,                // print result
        Instruction::Halt,                 // halt
        // fib:
        Instruction::Push(Value::Int(2)), // push 2
        Instruction::LoadArg(0),
        Instruction::LessThan,       // 2 < n
        Instruction::JumpIfTrue(10), // if yes, jump to recursive case
        Instruction::LoadArg(0),     // return n
        Instruction::Ret,
        Instruction::LoadArg(0),
        Instruction::Push(Value::Int(1)),
        Instruction::Sub,
        Instruction::Call(4, 1), // fib(n - 1)
        Instruction::LoadArg(0),
        Instruction::Push(Value::Int(2)),
        Instruction::Sub,
        Instruction::Call(4, 1), // fib(n - 2)
        Instruction::Add,
        Instruction::Ret,
    ];

    let mut vm = VM::new(code);
    vm.run();
}
