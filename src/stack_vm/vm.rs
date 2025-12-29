use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Pointer(usize),
    Bool(bool),
    Int(i64),
    Procedure { addr: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // stack manipulation
    StackAlloc { size: u8 },
    Push { value: Value },
    LoadLocal { offset: i8 },
    StoreLocal { offset: i8 },
    LoadGlobal { offset: u8 },
    StoreGlobal { offset: u8 },
    // operations
    Add,
    Sub,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    // control flow
    Jump { addr: usize },
    JumpOffset { offset: i16 },
    JumpIfTrue { offset: i16 },
    // procedure calling
    Call { addr: usize },
    CallStack,
    Ret,
    Slide { n: usize },
    // others
    Halt,
}

#[derive(Debug, PartialEq)]
pub struct VM {
    code: Vec<Instruction>,
    stack: Vec<Value>,
    globals: Vec<Value>,
    sp: usize,
    fp: usize,
    ip: usize,
}

impl VM {
    pub fn new(code: Vec<Instruction>, ip: usize) -> Self {
        Self {
            code,
            globals: vec![Value::Int(0); 1024],
            stack: vec![Value::Int(0); 1024],
            sp: 0,
            fp: 0,
            ip,
        }
    }

    pub fn clone_stack(&self) -> Vec<Value> {
        // pop does not actually shrink the stack
        // we clone only up to sp, to omit the "garbage"
        self.stack[..self.sp].to_vec()
    }

    pub fn clone_stack_top(&self) -> Option<Value> {
        if self.sp == 0 {
            None
        } else {
            Some(self.stack[self.sp - 1].clone())
        }
    }

    fn push(&mut self, val: Value) {
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(&mut self) -> Result<Value, &'static str> {
        if self.sp == 0 {
            return Err("Stack is empty");
        }
        self.sp -= 1;
        Ok(self.stack[self.sp].clone())
    }

    fn drop(&mut self, n: usize) -> Result<(), &'static str> {
        if self.sp < n {
            return Err("Stack is too small");
        }
        self.sp -= n;
        Ok(())
    }

    fn call(&mut self, addr: usize) {
        self.push(Value::Pointer(self.fp));
        self.push(Value::Pointer(self.ip));
        self.fp = self.sp;
        self.ip = addr;
    }

    pub fn step(&mut self) -> Result<(), &'static str> {
        let instr = self.code[self.ip].clone();
        self.ip += 1;
        match instr {
            Instruction::StackAlloc { size } => self.sp += size as usize,
            Instruction::Halt => self.ip = self.code.len(),
            Instruction::Push { value } => self.push(value),
            Instruction::Add => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x + y)),
                    _ => return Err("Invalid operands for Add"),
                }
            }
            Instruction::Sub => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x - y)),
                    _ => return Err("Invalid operands for Sub"),
                }
            }
            Instruction::LessThan => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x < y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::LessThanEqual => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x <= y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThan => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x > y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThanEqual => {
                let a = self.pop()?;
                let b = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x >= y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::LoadLocal { offset } => {
                let src = if offset >= 0 {
                    self.fp + offset as usize
                } else {
                    self.fp - (-offset as usize)
                };
                self.push(self.stack[src].clone());
            }
            Instruction::StoreLocal { offset } => {
                let dest = self.fp + offset as usize;
                self.stack[dest] = self.pop()?;
                if self.sp <= dest {
                    self.sp = dest + 1
                }
            }
            Instruction::LoadGlobal { offset } => {
                self.push(self.globals[offset as usize].clone());
            }
            Instruction::StoreGlobal { offset } => {
                self.globals[offset as usize] = self.pop()?;
            }
            Instruction::Jump { addr } => self.ip = addr,
            Instruction::JumpOffset { offset } => {
                if offset >= 0 {
                    self.ip = self.ip.wrapping_add(offset as usize);
                } else {
                    self.ip = self.ip.wrapping_sub((-offset) as usize);
                }
            }
            Instruction::JumpIfTrue { offset } => {
                let cond = self.pop()?;
                match cond {
                    Value::Bool(true) => {
                        if offset >= 0 {
                            self.ip = self.ip.wrapping_add(offset as usize);
                        } else {
                            self.ip = self.ip.wrapping_sub((-offset) as usize);
                        }
                    }
                    Value::Bool(false) => (),
                    _ => return Err("Invalid operand for JumpIfTrue"),
                }
            }
            Instruction::Call { addr } => self.call(addr),
            Instruction::CallStack => {
                let value = self.pop()?;
                match value {
                    Value::Procedure { addr } => self.call(addr),
                    _ => return Err("Invalid operand for CallStack"),
                }
            }
            Instruction::Ret => {
                let ret = self.pop()?;
                self.sp = self.fp - 2;
                self.ip = match self.stack[self.fp - 1] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid return address"),
                };
                self.fp = match self.stack[self.fp - 2] {
                    Value::Pointer(i) => i,
                    _ => return Err("Invalid frame pointer"),
                };
                self.push(ret);
            }
            Instruction::Slide { n } => {
                let ret = self.pop()?;
                self.drop(n)?;
                self.push(ret);
            }
        }
        Ok(())
    }

    fn _run(&mut self, debug: bool) -> Result<(), &'static str> {
        if debug {
            println!("========================================");
            for (idx, instr) in self.code.iter().enumerate() {
                println!("{idx:03}: {instr:?}")
            }
            println!("----------------------------------------");
            println!("{self}")
        }
        while self.ip < self.code.len() {
            self.step()?;
            if debug {
                println!("----------------------------------------");
                println!("{self}")
            }
        }
        if debug {
            println!("========================================");
        }
        Ok(())
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        self._run(false)
    }

    pub fn debug(&mut self) -> Result<(), &'static str> {
        self._run(true)
    }
}

impl fmt::Display for VM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "fp: {}, sp: {}, ip: {} ==> {:?}",
            self.fp,
            self.sp,
            self.ip,
            self.code.get(self.ip),
        )?;
        write!(f, "stack: {:?}", self.clone_stack())
    }
}

#[cfg(test)]
mod tests {
    use super::{Instruction::*, Value::*, VM};

    #[test]
    fn test_fib() {
        let code = vec![
            // main:
            Push { value: Int(6) }, // argument for fib
            Call { addr: 3 },       // call fib
            Halt,                   // halt
            // fib:
            LoadLocal { offset: -3 }, // put n on the stack
            Push { value: Int(1) },   // push 1
            LessThan,
            JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
            LoadLocal { offset: -3 }, // put n on the stack
            Ret,                      // return n
            Push { value: Int(1) },
            LoadLocal { offset: -3 },
            Sub,
            Call { addr: 3 }, // fib(n - 1)
            StoreLocal { offset: 0 },
            Push { value: Int(2) },
            LoadLocal { offset: -3 },
            Sub,
            Call { addr: 3 }, // fib(n - 2)
            LoadLocal { offset: 0 },
            Add,
            Ret,
        ];

        let mut vm = VM::new(code, 0);
        vm.run().unwrap();
        assert_eq!(vm.clone_stack_top(), Some(Int(8)));
    }
}
