use std::fmt;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum Value {
    #[default]
    Null,
    Symbol(usize),
    Bool(bool),
    Int(i64),
    Float(f64),
    Pointer(usize),
    Procedure(usize),
    Pair(usize, usize),
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    Drop { n: usize },
    // others
    Halt,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub code: Vec<Instruction>,
    pub data: Vec<Value>,
    pub heap: Vec<Value>,
    pub num_globals: usize,
    pub symbol_table: Vec<String>,
}

#[derive(PartialEq)]
pub struct VM {
    code: Vec<Instruction>,
    data: Vec<Value>,
    stack: Vec<Value>,
    globals: Vec<Value>,
    heap: Vec<Value>,
    symbol_table: Vec<String>,
    sp: usize,
    fp: usize,
    ip: usize,
}

impl VM {
    pub fn new(program: Program, stack_size: usize) -> Self {
        Self {
            code: program.code,
            data: program.data,
            heap: program.heap,
            symbol_table: program.symbol_table,
            stack: vec![Value::default(); stack_size],
            globals: vec![Value::default(); program.num_globals],
            sp: 0,
            fp: 0,
            ip: 0,
        }
    }

    pub fn value_to_string(&self, value: &Value) -> String {
        match value {
            Value::Symbol(idx) => self.symbol_table[*idx].clone(),
            Value::Null => "()".to_string(),
            Value::Bool(v) => (if *v { "#t" } else { "#f" }).to_string(),
            Value::Int(v) => format!("{v}"),
            Value::Float(v) => {
                if v.fract() == 0.0 {
                    format!("{v:.1}")
                } else {
                    format!("{v}")
                }
            }
            Value::Procedure(addr) => format!("$Procedure@{addr}"),
            Value::Pointer(addr) => format!("$Pointer->{addr}"),
            Value::Pair(addr_car, addr_cdr) => {
                let mut output = "(".to_string();
                let car_string = self.value_to_string(&self.heap[*addr_car]);
                output.push_str(&car_string);
                let mut next = &self.heap[*addr_cdr];

                loop {
                    match next {
                        Value::Null => {
                            break;
                        }
                        Value::Pair(addr_car, addr_cdr) => {
                            output.push(' ');
                            let car_string = self.value_to_string(&self.heap[*addr_car]);
                            output.push_str(&car_string);
                            next = &self.heap[*addr_cdr];
                        }
                        other => {
                            output.push_str(" . ");
                            output.push_str(&self.value_to_string(other));
                            break;
                        }
                    }
                }

                output.push(')');
                output
            }
        }
    }

    pub fn clone_stack(&self) -> Vec<Value> {
        // pop does not actually shrink the stack
        // we clone only up to sp, to omit the "garbage"
        self.stack[..self.sp].to_vec()
    }

    pub fn get_stack_top(&self) -> Option<Value> {
        if self.sp == 0 {
            None
        } else {
            Some(self.stack[self.sp - 1])
        }
    }

    pub fn represent_stack_top(&self) -> Option<String> {
        let stack_top = self.get_stack_top()?;
        Some(self.value_to_string(&stack_top))
    }

    pub fn heap_size(&self) -> usize {
        self.heap.len()
    }

    #[inline]
    fn push_to_stack(&mut self, val: Value) {
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    #[inline]
    fn pop_from_stack(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp]
    }

    #[inline]
    fn add_to_heap(&mut self, val: Value) -> usize {
        self.heap.push(val);
        self.heap.len() - 1
    }

    #[inline]
    fn call(&mut self, addr: usize) {
        self.push_to_stack(Value::Pointer(self.fp));
        self.push_to_stack(Value::Pointer(self.ip));
        self.fp = self.sp;
        self.ip = addr;
    }

    fn step(&mut self) {
        let instr = self.code[self.ip];
        self.ip += 1;
        match instr {
            Instruction::StackAlloc { size } => self.sp += size as usize,
            Instruction::Halt => self.ip = self.code.len(),
            Instruction::LoadConst { offset } => self.push_to_stack(self.data[offset as usize]),
            Instruction::PushZero => self.push_to_stack(Value::Int(0)),
            Instruction::PushOne => self.push_to_stack(Value::Int(1)),
            Instruction::Add => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Int(x + y)),
                    (Value::Int(x), Value::Float(y)) => {
                        self.push_to_stack(Value::Float((x as f64) + y))
                    }
                    (Value::Float(x), Value::Int(y)) => {
                        self.push_to_stack(Value::Float(x + (y as f64)))
                    }
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Float(x + y)),
                    _ => panic!("Invalid operands for Add"),
                }
            }
            Instruction::Sub => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Int(x - y)),
                    (Value::Int(x), Value::Float(y)) => {
                        self.push_to_stack(Value::Float((x as f64) - y))
                    }
                    (Value::Float(x), Value::Int(y)) => {
                        self.push_to_stack(Value::Float(x - (y as f64)))
                    }
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Float(x - y)),
                    _ => panic!("Invalid operands for Sub"),
                }
            }
            Instruction::Mul => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Int(x * y)),
                    (Value::Int(x), Value::Float(y)) => {
                        self.push_to_stack(Value::Float((x as f64) * y))
                    }
                    (Value::Float(x), Value::Int(y)) => {
                        self.push_to_stack(Value::Float(x * (y as f64)))
                    }
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Float(x * y)),
                    _ => panic!("Invalid operands for Mul"),
                }
            }
            Instruction::Div => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Float(x / y)),
                    (Value::Int(x), Value::Float(y)) => {
                        self.push_to_stack(Value::Float((x as f64) / y))
                    }
                    (Value::Float(x), Value::Int(y)) => {
                        self.push_to_stack(Value::Float(x / (y as f64)))
                    }
                    _ => panic!("Invalid operands for Div"),
                }
            }
            Instruction::Abs => {
                let a = self.pop_from_stack();
                match a {
                    Value::Int(x) => self.push_to_stack(Value::Int(x.abs())),
                    Value::Float(x) => self.push_to_stack(Value::Float(x.abs())),
                    _ => panic!("Invalid operand for Abs"),
                }
            }
            Instruction::LessThan => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x < y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x < y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::LessThanEqual => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x <= y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x <= y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThan => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x > y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x > y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::GreaterThanEqual => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x >= y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x >= y)),
                    _ => panic!("Invalid operands for LessThan"),
                }
            }
            Instruction::IsNull => match self.pop_from_stack() {
                Value::Null => self.push_to_stack(Value::Bool(true)),
                _ => self.push_to_stack(Value::Bool(false)),
            },
            Instruction::Cons => {
                let cdr = self.pop_from_stack();
                let cdr_addr = self.add_to_heap(cdr);
                let car = self.pop_from_stack();
                let car_addr = self.add_to_heap(car);
                self.push_to_stack(Value::Pair(car_addr, cdr_addr));
            }
            Instruction::Car => {
                let Value::Pair(car_addr, _) = self.pop_from_stack() else {
                    panic!("Invalid operand for Car");
                };
                self.push_to_stack(self.heap[car_addr]);
            }
            Instruction::Cdr => {
                let Value::Pair(_, cdr_addr) = self.pop_from_stack() else {
                    panic!("Invalid operand for Cdr");
                };
                self.push_to_stack(self.heap[cdr_addr]);
            }
            Instruction::LoadLocal { offset } => {
                let src = self.fp.wrapping_add(offset as usize);
                self.push_to_stack(self.stack[src]);
            }
            Instruction::StoreLocal { offset } => {
                let dest = self.fp.wrapping_add(offset as usize);
                self.stack[dest] = self.pop_from_stack();
                if self.sp <= dest {
                    self.sp = dest + 1
                }
            }
            Instruction::LoadGlobal { offset } => {
                self.push_to_stack(self.globals[offset as usize]);
            }
            Instruction::StoreGlobal { offset } => {
                self.globals[offset as usize] = self.pop_from_stack();
            }
            Instruction::Jump { offset } => {
                self.ip = self.ip.wrapping_add(offset as usize);
            }
            Instruction::JumpIfTrue { offset } => {
                let cond = self.pop_from_stack();
                match cond {
                    Value::Bool(true) => {
                        self.ip = self.ip.wrapping_add(offset as usize);
                    }
                    Value::Bool(false) => (),
                    _ => panic!("Invalid operand for JumpIfTrue"),
                }
            }
            Instruction::Call { addr } => self.call(addr),
            Instruction::CallStack => {
                let value = self.pop_from_stack();
                match value {
                    Value::Procedure(addr) => self.call(addr),
                    _ => panic!("Invalid operand for CallStack"),
                }
            }
            Instruction::Ret => {
                let ret = self.pop_from_stack();
                self.sp = self.fp - 2;
                self.ip = match self.stack[self.fp - 1] {
                    Value::Pointer(i) => i,
                    _ => panic!("Invalid return address"),
                };
                self.fp = match self.stack[self.fp - 2] {
                    Value::Pointer(i) => i,
                    _ => panic!("Invalid frame pointer"),
                };
                self.push_to_stack(ret);
            }
            Instruction::Slide { n } => {
                let ret = self.pop_from_stack();
                self.sp -= n;
                self.push_to_stack(ret);
            }
            Instruction::Drop { n } => {
                self.sp -= n;
            }
        }
    }

    fn print_state(&self) {
        println!(
            "fp: {}, sp: {}, ip: {} ==> {:?}",
            self.fp,
            self.sp,
            self.ip,
            self.code.get(self.ip),
        );
        println!("stack: {:?}", self.clone_stack())
    }

    fn execute<const DEBUG: bool>(&mut self) {
        if DEBUG {
            println!("========================================");
            print!("{self}");
            println!("----------------------------------------");
            self.print_state();
        }
        while self.ip < self.code.len() {
            self.step();
            if DEBUG {
                self.print_state();
            }
        }
        if DEBUG {
            println!("========================================");
        }
    }

    pub fn run(&mut self) {
        self.execute::<false>();
    }

    pub fn debug(&mut self) {
        self.execute::<true>();
    }
}

impl fmt::Display for VM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, value) in self.data.iter().enumerate() {
            writeln!(f, "D{idx:03}: {value:?}")?
        }
        for (idx, instr) in self.code.iter().enumerate() {
            writeln!(f, "C{idx:03}: {instr:?}")?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Instruction::*, Program, Value::*, VM};

    #[test]
    fn test_fib() {
        let program = Program {
            code: vec![
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
            ],
            data: vec![Int(6), Int(1), Int(2)],
            heap: vec![],
            num_globals: 0,
            symbol_table: vec![],
        };
        let mut vm = VM::new(program, 1024);
        vm.debug();
        assert_eq!(vm.get_stack_top(), Some(Int(8)));
    }
}
