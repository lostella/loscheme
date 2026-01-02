use crate::parser::{Expr, Keyword};
use internment::Intern;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Symbol(Intern<String>),
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Pointer(usize),
    Procedure { addr: usize },
    Pair(Rc<RefCell<(Value, Value)>>),
}

impl From<Expr> for Value {
    fn from(expr: Expr) -> Self {
        match expr {
            Expr::Symbol(x) => Value::Symbol(x),
            Expr::Bool(x) => Value::Bool(x),
            Expr::Integer(x) => Value::Int(x),
            Expr::Float(x) => Value::Float(x),
            Expr::List(v) => {
                let Some((first, rest)) = v.split_first() else {
                    return Value::Null;
                };
                match first {
                    Expr::Keyword(Keyword::Dot) => {
                        if let Some(last) = rest.first() {
                            Value::from(last.clone())
                        } else {
                            Value::Null
                        }
                    }
                    _ => Value::Pair(Rc::new(RefCell::new((
                        Value::from(first.clone()),
                        Value::from(Expr::List(rest.to_vec())),
                    )))),
                }
            }
            _ => todo!("{expr:?}"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Null => write!(f, "()"),
            Value::Bool(v) => write!(f, "{}", if *v { "#t" } else { "#f" }),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => {
                if v.fract() == 0.0 {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }
            Value::Procedure { addr } => write!(f, "$Procedure@{addr}"),
            Value::Pointer(addr) => write!(f, "$Pointer->{addr}"),
            Value::Pair(_) => {
                write!(f, "(")?;
                let mut current = self.clone();
                let mut first = true;

                loop {
                    match current {
                        Value::Pair(pair_rc) => {
                            let pair = pair_rc.borrow();
                            if !first {
                                write!(f, " ")?;
                            }
                            write!(f, "{}", pair.0)?;
                            current = pair.1.clone();
                            first = false;
                        }
                        Value::Null => {
                            write!(f, ")")?;
                            break;
                        }
                        other => {
                            write!(f, " . {other})")?;
                            break;
                        }
                    }
                }
                Ok(())
            }
        }
    }
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
    // numerical operations
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    // predicates
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    IsNull,
    // pairs
    Cons,
    Car,
    Cdr,
    // control flow
    Jump { offset: i16 },
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
    pub fn new(code: Vec<Instruction>) -> Self {
        Self {
            code,
            globals: vec![Value::Int(0); 1024],
            stack: vec![Value::Int(0); 1024],
            sp: 0,
            fp: 0,
            ip: 0,
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
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x + y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) + y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x + (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x + y)),
                    _ => return Err("Invalid operands for Add"),
                }
            }
            Instruction::Sub => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x - y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) - y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x - (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x - y)),
                    _ => return Err("Invalid operands for Sub"),
                }
            }
            Instruction::Mul => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x * y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) * y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x * (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x * y)),
                    _ => return Err("Invalid operands for Mul"),
                }
            }
            Instruction::Div => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x / y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) / y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x / (y as f64))),
                    _ => return Err("Invalid operands for Div"),
                }
            }
            Instruction::Abs => {
                let a = self.pop()?;
                match a {
                    Value::Int(x) => self.push(Value::Int(x.abs())),
                    Value::Float(x) => self.push(Value::Float(x.abs())),
                    _ => return Err("Invalid operand for Abs"),
                }
            }
            Instruction::LessThan => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x < y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x < y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::LessThanEqual => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x <= y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x <= y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThan => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x > y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x > y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThanEqual => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x >= y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x >= y)),
                    _ => return Err("Invalid operands for LessThan"),
                }
            }
            Instruction::IsNull => match self.pop()? {
                Value::Null => self.push(Value::Bool(true)),
                _ => self.push(Value::Bool(false)),
            },
            Instruction::Cons => {
                let cdr = self.pop()?;
                let car = self.pop()?;
                self.push(Value::Pair(Rc::new(RefCell::new((car, cdr)))));
            }
            Instruction::Car => {
                let Value::Pair(rc) = self.pop()? else {
                    return Err("Invalid operand for Car");
                };
                self.push(rc.borrow().0.clone());
            }
            Instruction::Cdr => {
                let Value::Pair(rc) = self.pop()? else {
                    return Err("Invalid operand for Cdr");
                };
                self.push(rc.borrow().1.clone());
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
            Instruction::Jump { offset } => {
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
            Push { value: Int(1) },   // push 1
            LoadLocal { offset: -3 }, // put n on the stack
            LessThan,
            JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
            LoadLocal { offset: -3 }, // put n on the stack
            Ret,                      // return n
            LoadLocal { offset: -3 },
            Push { value: Int(1) },
            Sub,
            Call { addr: 3 }, // fib(n - 1)
            StoreLocal { offset: 0 },
            LoadLocal { offset: -3 },
            Push { value: Int(2) },
            Sub,
            Call { addr: 3 }, // fib(n - 2)
            LoadLocal { offset: 0 },
            Add,
            Ret,
        ];

        let mut vm = VM::new(code);
        vm.run().unwrap();
        assert_eq!(vm.clone_stack_top(), Some(Int(8)));
    }
}
