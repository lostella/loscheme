use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Pointer(usize),
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Push { value: Value },
    Pop,
    Add,
    Sub,
    LessThan,
    Jump { offset: i16 },
    JumpIfTrue { offset: i16 },
    Call { addr: usize, nargs: u8 },
    LoadArg { narg: u8 },
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
            Instruction::Push { value } => self.push(value),
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
            Instruction::LessThan => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x < y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::Jump { offset } => {
                if offset >= 0 {
                    self.ip = self.ip.wrapping_add(offset as usize) - 1;
                } else {
                    self.ip = self.ip.wrapping_add((-offset) as usize) - 1;
                }
            }
            Instruction::JumpIfTrue { offset } => {
                let cond = self.pop();
                match cond {
                    Value::Bool(b) => {
                        if b {
                            if offset >= 0 {
                                self.ip = self.ip.wrapping_add(offset as usize) - 1;
                            } else {
                                self.ip = self.ip.wrapping_add((-offset) as usize) - 1;
                            }
                        }
                    }
                    _ => return Err("Invalid operand for JumpIfTrue"),
                }
            }
            Instruction::Call { addr, nargs } => {
                self.push(Value::Pointer(self.fp));
                self.push(Value::Pointer(self.ip));
                // the 2 here accounts for fp and ip on the stack
                self.fp = self.sp - (nargs as usize) - 2;
                self.ip = addr;
                self.stack[self.fp..self.sp].rotate_right(2);
            }
            Instruction::LoadArg { narg } => {
                // the 2 here accounts for fp and ip on the stack
                self.push(self.stack[self.fp + (narg as usize) + 2].clone());
            }
            Instruction::Ret => {
                let ret = self.pop();
                self.sp = self.fp;
                self.ip = match self.stack[self.fp + 1] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid return address"),
                };
                self.fp = match self.stack[self.fp] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid frame pointer"),
                };
                self.push(ret);
            }
            Instruction::Print => {
                println!("{:?}", self.pop());
            }
        }
        Ok(())
    }

    fn _run(&mut self, debug: bool) -> Result<Value, &'static str> {
        while self.ip < self.code.len() {
            if debug {
                println!("{self}")
            }
            self.step()?
        }
        Ok(self.stack[self.sp - 1].clone())
    }

    pub fn run(&mut self) -> Result<Value, &'static str> {
        self._run(false)
    }

    pub fn debug(&mut self) -> Result<Value, &'static str> {
        self._run(true)
    }
}

#[cfg(test)]
mod tests {
    use super::{Instruction::*, Value::*, VM};

    #[test]
    fn test_fib() {
        let code = vec![
            // main:
            Push { value: Int(6) },     // argument for fib
            Call { addr: 3, nargs: 1 }, // call fib
            Halt,                       // halt
            // fib:
            Push { value: Int(1) }, // push 1
            LoadArg { narg: 0 },    // put n on the stack
            LessThan,
            JumpIfTrue { offset: 3 }, // if 1 < n, jump to recursive case
            LoadArg { narg: 0 },      // put n on the stack
            Ret,                      // return n
            LoadArg { narg: 0 },
            Push { value: Int(1) },
            Sub,
            Call { addr: 3, nargs: 1 }, // fib(n - 1)
            LoadArg { narg: 0 },
            Push { value: Int(2) },
            Sub,
            Call { addr: 3, nargs: 1 }, // fib(n - 2)
            Add,
            Ret,
        ];

        let mut vm = VM::new(code);
        assert_eq!(vm.run(), Ok(Int(8)));
    }
}
