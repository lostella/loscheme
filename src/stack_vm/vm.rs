use crate::parser::{Expr, Keyword};
use internment::Intern;
use std::cell::RefCell;
use std::fmt;
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Value {
    #[default]
    Null,
    Symbol(Intern<String>),
    Bool(bool),
    Int(i64),
    Float(f64),
    Pointer(usize),
    Procedure {
        addr: usize,
    },
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
    LoadConst { offset: u8 },
    LoadLocal { offset: i8 },
    StoreLocal { offset: i8 },
    LoadGlobal { offset: u8 },
    StoreGlobal { offset: u8 },
    // numerical operations
    PushZero,
    PushOne,
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
    constants: Vec<Value>,
    stack: Vec<Value>,
    globals: Vec<Value>,
    sp: usize,
    fp: usize,
    ip: usize,
}

impl VM {
    pub fn new(code: Vec<Instruction>, constants: Vec<Value>) -> Self {
        Self {
            code,
            constants,
            globals: vec![Value::default(); 1024],
            stack: vec![Value::default(); 1024],
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

    #[inline]
    fn push(&mut self, val: Value) {
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    #[inline]
    fn pop(&mut self) -> Value {
        if self.sp == 0 {
            panic!("Stack is empty");
        }
        self.sp -= 1;
        mem::take(&mut self.stack[self.sp])
    }

    #[inline]
    fn drop(&mut self, n: usize) {
        if self.sp < n {
            panic!("Stack is too small");
        }
        self.sp -= n;
    }

    #[inline]
    fn call(&mut self, addr: usize) {
        self.push(Value::Pointer(self.fp));
        self.push(Value::Pointer(self.ip));
        self.fp = self.sp;
        self.ip = addr;
    }

    fn step(&mut self) {
        let instr = &self.code[self.ip];
        self.ip += 1;
        match instr {
            Instruction::StackAlloc { size } => self.sp += *size as usize,
            Instruction::Halt => self.ip = self.code.len(),
            Instruction::LoadConst { offset } => {
                self.push(self.constants[*offset as usize].clone())
            }
            Instruction::PushZero => self.push(Value::Int(0)),
            Instruction::PushOne => self.push(Value::Int(1)),
            Instruction::Add => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x + y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) + y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x + (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x + y)),
                    _ => panic!("Invalid operands for Add"),
                }
            }
            Instruction::Sub => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x - y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) - y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x - (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x - y)),
                    _ => panic!("Invalid operands for Sub"),
                }
            }
            Instruction::Mul => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Int(x * y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) * y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x * (y as f64))),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x * y)),
                    _ => panic!("Invalid operands for Mul"),
                }
            }
            Instruction::Div => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Float(x / y)),
                    (Value::Int(x), Value::Float(y)) => self.push(Value::Float((x as f64) / y)),
                    (Value::Float(x), Value::Int(y)) => self.push(Value::Float(x / (y as f64))),
                    _ => panic!("Invalid operands for Div"),
                }
            }
            Instruction::Abs => {
                let a = self.pop();
                match a {
                    Value::Int(x) => self.push(Value::Int(x.abs())),
                    Value::Float(x) => self.push(Value::Float(x.abs())),
                    _ => panic!("Invalid operand for Abs"),
                }
            }
            Instruction::LessThan => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x < y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x < y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::LessThanEqual => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x <= y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x <= y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThan => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x > y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x > y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThanEqual => {
                let b = self.pop();
                let a = self.pop();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push(Value::Bool(x >= y)),
                    (Value::Float(x), Value::Float(y)) => self.push(Value::Bool(x >= y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::IsNull => match self.pop() {
                Value::Null => self.push(Value::Bool(true)),
                _ => self.push(Value::Bool(false)),
            },
            Instruction::Cons => {
                let cdr = self.pop();
                let car = self.pop();
                self.push(Value::Pair(Rc::new(RefCell::new((car, cdr)))));
            }
            Instruction::Car => {
                let Value::Pair(rc) = self.pop() else {
                    panic!("Invalid operand for Car");
                };
                self.push(rc.borrow().0.clone());
            }
            Instruction::Cdr => {
                let Value::Pair(rc) = self.pop() else {
                    panic!("Invalid operand for Cdr");
                };
                self.push(rc.borrow().1.clone());
            }
            Instruction::LoadLocal { offset } => {
                let src = self.fp.wrapping_add(*offset as usize);
                self.push(self.stack[src].clone());
            }
            Instruction::StoreLocal { offset } => {
                let dest = self.fp.wrapping_add(*offset as usize);
                self.stack[dest] = self.pop();
                if self.sp <= dest {
                    self.sp = dest + 1
                }
            }
            Instruction::LoadGlobal { offset } => {
                self.push(self.globals[*offset as usize].clone());
            }
            Instruction::StoreGlobal { offset } => {
                let offset = *offset;
                self.globals[offset as usize] = self.pop();
            }
            Instruction::Jump { offset } => {
                self.ip = self.ip.wrapping_add(*offset as usize);
            }
            Instruction::JumpIfTrue { offset } => {
                let offset = *offset;
                let cond = self.pop();
                match cond {
                    Value::Bool(true) => {
                        self.ip = self.ip.wrapping_add(offset as usize);
                    }
                    Value::Bool(false) => (),
                    _ => panic!("Invalid operand for JumpIfTrue"),
                }
            }
            Instruction::Call { addr } => self.call(*addr),
            Instruction::CallStack => {
                let value = self.pop();
                match value {
                    Value::Procedure { addr } => self.call(addr),
                    _ => panic!("Invalid operand for CallStack"),
                }
            }
            Instruction::Ret => {
                let ret = self.pop();
                self.sp = self.fp - 2;
                self.ip = match self.stack[self.fp - 1] {
                    Value::Pointer(i) => i,
                    _ => panic!("Invalid return address"),
                };
                self.fp = match self.stack[self.fp - 2] {
                    Value::Pointer(i) => i,
                    _ => panic!("Invalid frame pointer"),
                };
                self.push(ret);
            }
            Instruction::Slide { n } => {
                let n = *n;
                let ret = self.pop();
                self.drop(n);
                self.push(ret);
            }
        }
    }

    fn _run(&mut self, debug: bool) {
        if debug {
            println!("========================================");
            for (idx, instr) in self.code.iter().enumerate() {
                println!("{idx:03}: {instr:?}")
            }
            println!("----------------------------------------");
            println!("{self}")
        }
        while self.ip < self.code.len() {
            self.step();
            if debug {
                println!("----------------------------------------");
                println!("{self}")
            }
        }
        if debug {
            println!("========================================");
        }
    }

    pub fn run(&mut self) {
        self._run(false)
    }

    pub fn debug(&mut self) {
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
            LoadConst { offset: 0 }, // argument for fib
            Call { addr: 3 },        // call fib
            Halt,                    // halt
            // fib:
            LoadConst { offset: 1 },  // push 1
            LoadLocal { offset: -3 }, // put n on the stack
            LessThan,
            JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
            LoadLocal { offset: -3 }, // put n on the stack
            Ret,                      // return n
            LoadLocal { offset: -3 },
            LoadConst { offset: 1 },
            Sub,
            Call { addr: 3 }, // fib(n - 1)
            StoreLocal { offset: 0 },
            LoadLocal { offset: -3 },
            LoadConst { offset: 2 },
            Sub,
            Call { addr: 3 }, // fib(n - 2)
            LoadLocal { offset: 0 },
            Add,
            Ret,
        ];

        let mut vm = VM::new(code, vec![Int(6), Int(1), Int(2)]);
        vm.debug();
        assert_eq!(vm.clone_stack_top(), Some(Int(8)));
    }
}
