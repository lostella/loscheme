use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Pointer(usize),
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Push(Value),
    Pop,
    Add,
    Sub,
    JumpLessThan(i16),
    Call(usize, usize),
    LoadArg(usize),
    Ret,
    Print,
    Halt,
}

pub struct VM {
    code: Vec<Instruction>,
    stack: Vec<Value>,
    sp: usize,
    fp: usize,
    ip: usize,
}

impl fmt::Display for VM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ip: {}, fp: {}, stack: {:?}, instr: {:?}",
            self.ip,
            self.fp,
            self.stack[..self.sp].to_vec(),
            self.code[self.ip],
        )
    }
}

impl VM {
    pub fn new(code: Vec<Instruction>) -> Self {
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

    pub fn step(&mut self) -> Result<(), &'static str> {
        let instr = self.code[self.ip].clone();
        self.ip += 1;
        match instr {
            Instruction::Halt => self.ip = self.code.len(),
            Instruction::Push(v) => self.push(v),
            Instruction::Pop => {
                self.pop();
            }
            Instruction::Add => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x + y)),
                    _ => return Err("Invalid operands for Add"),
                }
            }
            Instruction::Sub => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x - y)),
                    _ => return Err("Invalid operands for Sub"),
                }
            }
            Instruction::JumpLessThan(offset) => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => {
                        if x < y {
                            if offset >= 0 {
                                self.ip = self.ip.wrapping_add(offset as usize) - 1;
                            } else {
                                self.ip = self.ip.wrapping_add((-offset) as usize) - 1;
                            }
                        }
                    }
                    _ => return Err("Invalid operands for JumpLessThan"),
                }
            }
            Instruction::Call(addr, nargs) => {
                let mut args = vec![];
                for _ in 0..nargs {
                    args.push(self.pop());
                }
                self.push(Value::Pointer(self.fp));
                self.push(Value::Pointer(self.ip));
                while let Some(value) = args.pop() {
                    self.push(value);
                }
                // the 2 here accounts for fp and ip on the stack
                self.fp = self.sp - nargs - 2;
                self.ip = addr;
            }
            Instruction::LoadArg(offset) => {
                // the 2 here accounts for fp and ip on the stack
                self.push(self.stack[self.fp + offset + 2].clone());
            }
            Instruction::Ret => {
                let ret = self.pop();
                let ret_ip = match self.stack[self.fp + 1] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid return address"),
                };
                let ret_fp = match self.stack[self.fp + 0] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid frame pointer"),
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
        Ok(())
    }

    pub fn run(&mut self, debug: bool) -> Result<Value, &'static str> {
        while self.ip < self.code.len() {
            if debug {
                println!("{}", self)
            }
            self.step()?
        }
        Ok(self.stack[self.sp - 1].clone())
    }
}

#[cfg(test)]
mod tests {
    use super::{Instruction::*, Value::*, VM};

    #[test]
    fn test_fib() {
        let code = vec![
            // main:
            Push(Int(6)), // argument for fib
            Call(3, 1),   // call fib(20)
            Halt,         // halt
            // fib:
            Push(Int(1)),    // push 1
            LoadArg(0),      // put n on the stack
            JumpLessThan(3), // if 1 < n, jump to recursive case
            LoadArg(0),      // put n on the stack
            Ret,             // return n
            LoadArg(0),
            Push(Int(1)),
            Sub,
            Call(3, 1), // fib(n - 1)
            LoadArg(0),
            Push(Int(2)),
            Sub,
            Call(3, 1), // fib(n - 2)
            Add,
            Ret,
        ];

        let mut vm = VM::new(code);
        assert_eq!(vm.run(false), Ok(Int(8)));
    }
}
