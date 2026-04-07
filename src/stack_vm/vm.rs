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
    CallRec { addr: usize },
    CallStack,
    CallStackRec,
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
    constants: Vec<Value>,
    globals: Vec<Value>,
    memory: Vec<Value>,
    symbol_table: Vec<String>,
    stack_ptr: usize,
    heap_ptr: usize,
    frame_ptr: usize,
    instr_ptr: usize,
}

impl VM {
    pub fn new(program: Program, memory_size: usize) -> Self {
        let mut memory = program.heap.clone();
        memory.resize(memory_size, Value::default());
        let hp = program.heap.len();
        Self {
            code: program.code,
            constants: program.data,
            globals: vec![Value::default(); program.num_globals],
            memory,
            symbol_table: program.symbol_table,
            stack_ptr: memory_size - 1,
            frame_ptr: memory_size - 1,
            heap_ptr: hp,
            instr_ptr: 0,
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
                let car_string = self.value_to_string(&self.memory[*addr_car]);
                output.push_str(&car_string);
                let mut next = &self.memory[*addr_cdr];

                loop {
                    match next {
                        Value::Null => {
                            break;
                        }
                        Value::Pair(addr_car, addr_cdr) => {
                            output.push(' ');
                            let car_string = self.value_to_string(&self.memory[*addr_car]);
                            output.push_str(&car_string);
                            next = &self.memory[*addr_cdr];
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
        self.memory[self.stack_ptr + 1..].to_vec()
    }

    pub fn get_stack_top(&self) -> Option<Value> {
        if self.stack_ptr >= self.memory.len() - 1 {
            None
        } else {
            Some(self.memory[self.stack_ptr + 1])
        }
    }

    pub fn represent_stack_top(&self) -> Option<String> {
        let stack_top = self.get_stack_top()?;
        Some(self.value_to_string(&stack_top))
    }

    pub fn heap_size(&self) -> usize {
        self.heap_ptr
    }

    fn panic_if_oom(&self) {
        if self.stack_ptr < self.heap_ptr {
            panic!(
                "Out of memory: stack_ptr={} < heap_ptr={}",
                self.stack_ptr, self.heap_ptr
            )
        }
    }

    #[inline]
    fn push_to_stack(&mut self, val: Value) {
        self.panic_if_oom();
        self.memory[self.stack_ptr] = val;
        self.stack_ptr -= 1;
    }

    #[inline]
    fn pop_from_stack(&mut self) -> Value {
        self.stack_ptr += 1;
        self.memory[self.stack_ptr]
    }

    #[inline]
    fn add_to_heap(&mut self, val: Value) -> usize {
        self.panic_if_oom();
        self.memory[self.heap_ptr] = val;
        self.heap_ptr += 1;
        self.heap_ptr - 1
    }

    #[inline]
    fn call(&mut self, addr: usize) {
        self.push_to_stack(Value::Pointer(self.frame_ptr));
        self.push_to_stack(Value::Pointer(self.instr_ptr));
        self.frame_ptr = self.stack_ptr;
        self.instr_ptr = addr;
    }

    #[inline]
    fn call_recycle_frame(&mut self, addr: usize) {
        self.stack_ptr = self.frame_ptr;
        self.instr_ptr = addr;
    }

    fn step(&mut self) {
        let instr = self.code[self.instr_ptr];
        self.instr_ptr += 1;
        match instr {
            Instruction::StackAlloc { size } => self.stack_ptr -= size as usize,
            Instruction::Halt => self.instr_ptr = self.code.len(),
            Instruction::LoadConst { offset } => {
                self.push_to_stack(self.constants[offset as usize])
            }
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
                    _ => panic!("Invalid operands for Add: {a:?}, {b:?}"),
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
                    _ => panic!("Invalid operands for Sub: {a:?}, {b:?}"),
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
                    _ => panic!("Invalid operands for Mul: {a:?}, {b:?}"),
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
                    _ => panic!("Invalid operands for Div: {a:?}, {b:?}"),
                }
            }
            Instruction::Abs => {
                let a = self.pop_from_stack();
                match a {
                    Value::Int(x) => self.push_to_stack(Value::Int(x.abs())),
                    Value::Float(x) => self.push_to_stack(Value::Float(x.abs())),
                    _ => panic!("Invalid operand for Abs: {a:?}"),
                }
            }
            Instruction::LessThan => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x < y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x < y)),
                    _ => panic!("Invalid operands for LessThan: {a:?}, {b:?}"),
                }
            }
            Instruction::LessThanEqual => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x <= y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x <= y)),
                    _ => panic!("Invalid operands for LessThanEqual: {a:?}, {b:?}"),
                }
            }
            Instruction::GreaterThan => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x > y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x > y)),
                    _ => panic!("Invalid operands for GreaterThan: {a:?}, {b:?}"),
                }
            }
            Instruction::GreaterThanEqual => {
                let b = self.pop_from_stack();
                let a = self.pop_from_stack();
                match (a, b) {
                    (Value::Int(x), Value::Int(y)) => self.push_to_stack(Value::Bool(x >= y)),
                    (Value::Float(x), Value::Float(y)) => self.push_to_stack(Value::Bool(x >= y)),
                    _ => panic!("Invalid operands for GreaterThanEqual: {a:?}, {b:?}"),
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
                let top = self.pop_from_stack();
                let Value::Pair(car_addr, _) = top else {
                    panic!("Invalid operand for Car: {top:?}");
                };
                self.push_to_stack(self.memory[car_addr]);
            }
            Instruction::Cdr => {
                let top = self.pop_from_stack();
                let Value::Pair(_, cdr_addr) = top else {
                    panic!("Invalid operand for Cdr: {top:?}");
                };
                self.push_to_stack(self.memory[cdr_addr]);
            }
            Instruction::LoadLocal { offset } => {
                let src = self.frame_ptr.wrapping_add(offset as usize);
                self.push_to_stack(self.memory[src]);
            }
            Instruction::StoreLocal { offset } => {
                let dest = self.frame_ptr.wrapping_add(offset as usize);
                self.memory[dest] = self.pop_from_stack();
                if self.stack_ptr >= dest {
                    self.stack_ptr = dest - 1
                }
            }
            Instruction::LoadGlobal { offset } => {
                self.push_to_stack(self.globals[offset as usize]);
            }
            Instruction::StoreGlobal { offset } => {
                self.globals[offset as usize] = self.pop_from_stack();
            }
            Instruction::Jump { offset } => {
                self.instr_ptr = self.instr_ptr.wrapping_add(offset as usize);
            }
            Instruction::JumpIfTrue { offset } => {
                let cond = self.pop_from_stack();
                match cond {
                    Value::Bool(true) => {
                        self.instr_ptr = self.instr_ptr.wrapping_add(offset as usize);
                    }
                    Value::Bool(false) => (),
                    _ => panic!("Invalid operand for JumpIfTrue: {cond:?}"),
                }
            }
            Instruction::Call { addr } => self.call(addr),
            Instruction::CallRec { addr } => self.call_recycle_frame(addr),
            Instruction::CallStack => {
                let value = self.pop_from_stack();
                match value {
                    Value::Procedure(addr) => self.call(addr),
                    _ => panic!("Invalid operand for CallStack: {value:?}"),
                }
            }
            Instruction::CallStackRec => {
                let value = self.pop_from_stack();
                match value {
                    Value::Procedure(addr) => self.call_recycle_frame(addr),
                    _ => panic!("Invalid operand for CallStackRec: {value:?}"),
                }
            }
            Instruction::Ret => {
                let ret = self.pop_from_stack();
                self.stack_ptr = self.frame_ptr + 2;
                self.instr_ptr = match self.memory[self.frame_ptr + 1] {
                    Value::Pointer(i) => i,
                    _ => panic!(
                        "Invalid return address: {:?}",
                        self.memory[self.frame_ptr + 1]
                    ),
                };
                self.frame_ptr = match self.memory[self.frame_ptr + 2] {
                    Value::Pointer(i) => i,
                    _ => panic!(
                        "Invalid frame pointer: {:?}",
                        self.memory[self.frame_ptr + 2]
                    ),
                };
                self.push_to_stack(ret);
            }
            Instruction::Slide { n } => {
                let ret = self.pop_from_stack();
                self.stack_ptr += n;
                self.push_to_stack(ret);
            }
            Instruction::Drop { n } => {
                self.stack_ptr += n;
            }
        }
    }

    fn print_state(&self) {
        println!("stack: {:?}", self.clone_stack());
        println!(
            "fp: {}, sp: {}, ip: {} ==> {:?}",
            self.frame_ptr,
            self.stack_ptr,
            self.instr_ptr,
            self.code.get(self.instr_ptr),
        )
    }

    fn execute<const DEBUG: bool>(&mut self) {
        if DEBUG {
            println!("========================================");
            print!("{self}");
            println!("----------------------------------------");
            self.print_state();
        }
        while self.instr_ptr < self.code.len() {
            self.step();
            if DEBUG {
                println!("----------------------------------------");
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
        for (idx, value) in self.constants.iter().enumerate() {
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
                LoadConst { offset: 1 }, // push 1
                LoadLocal { offset: 3 }, // put n on the stack
                LessThan,
                JumpIfTrue { offset: 2 }, // if 1 < n, jump to recursive case
                LoadLocal { offset: 3 },  // put n on the stack
                Ret,                      // return n
                LoadLocal { offset: 3 },
                LoadConst { offset: 1 },
                Sub,
                Call { addr: 3 }, // fib(n - 1)
                StoreLocal { offset: 0 },
                LoadLocal { offset: 3 },
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
