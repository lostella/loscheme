use crate::parser::{Expr, Keyword};
use std::{collections::HashMap, fmt};

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
    Call { addr: usize },
    StackAlloc { size: u8 },
    LoadArg { idx: u8 },
    LoadLocal { offset: u8 },
    StoreLocal { offset: u8 },
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
            self.clone_stack(),
            self.code[self.ip],
        )
    }
}

impl VM {
    pub fn new(code: Vec<Instruction>) -> Self {
        Self {
            code,
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

    fn push(&mut self, val: Value) {
        self.stack[self.sp] = val;
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
            Instruction::Print => {
                println!("{:?}", self.pop());
            }
            Instruction::Call { addr } => {
                self.push(Value::Pointer(self.fp));
                self.push(Value::Pointer(self.ip));
                self.fp = self.sp;
                self.ip = addr;
            }
            Instruction::StackAlloc { size } => {
                self.sp += size as usize;
            }
            Instruction::LoadArg { idx } => {
                self.push(self.stack[self.fp - 3 - idx as usize].clone());
            }
            Instruction::LoadLocal { offset } => {
                self.push(self.stack[self.fp + offset as usize].clone());
            }
            Instruction::StoreLocal { offset } => {
                self.stack[self.fp + offset as usize] = self.pop();
            }
            Instruction::Ret => {
                let ret = self.pop();
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

struct Compiler {
    env_stack: Vec<HashMap<String, usize>>,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            env_stack: vec![HashMap::<String, usize>::new()],
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Vec<Instruction>, String> {
        match expr {
            Expr::List(v) => self.compile_sexpr(&v),
            Expr::Bool(b) => Ok(vec![Instruction::Push {
                value: Value::Bool(*b),
            }]),
            Expr::Integer(i) => Ok(vec![Instruction::Push {
                value: Value::Int(*i),
            }]),
            _ => todo!("{expr}"),
        }
    }

    fn compile_sexpr(&mut self, exprs: &[Expr]) -> Result<Vec<Instruction>, String> {
        let Some((first, rest)) = exprs.split_first() else {
            return Err("Cannot compile the empty list".to_string());
        };
        match first {
            Expr::Keyword(Keyword::If) => self.compile_if(rest),
            Expr::Keyword(Keyword::Begin) => self.compile_begin(rest),
            Expr::Keyword(Keyword::Define) => self.compile_define(rest),
            Expr::Keyword(Keyword::Lambda) => self.compile_lambda(rest),
            Expr::Symbol(s) => {
                let s = s.as_str();
                match s {
                    "<" | ">" | "<=" | ">=" => self.compile_cmp(s, rest),
                    "+" => self.compile_add(rest),
                    "-" => self.compile_sub(rest),
                    _ => todo!("list starting with: {first}"),
                }
            }
            _ => todo!("list starting with: {first}"),
        }
    }

    fn compile_if(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 3 {
            return Err("`if` takes exactly 3 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut branch_true = self.compile_expr(&args[1])?;
        let mut branch_false = self.compile_expr(&args[2])?;
        instr.push(Instruction::JumpIfTrue {
            offset: branch_false.len() as i16 + 2,
        });
        instr.append(&mut branch_false);
        instr.push(Instruction::Jump {
            offset: branch_true.len() as i16 + 1,
        });
        instr.append(&mut branch_true);
        Ok(instr)
    }

    fn compile_begin(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        let mut instr = vec![];
        for expr in args {
            instr.append(&mut self.compile_expr(expr)?);
        }
        Ok(instr)
    }

    fn compile_define(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() < 2 {
            return Err("`define` takes at least 2 arguments".to_string());
        }
        _ = self.env_stack;
        todo!("define")
    }

    fn compile_lambda(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() < 1 {
            return Err("`lambda` takes at least 1 argument".to_string());
        }
        todo!("lambda")
        // TODO:
        // - add stack of environments (symbols tables) to compiler
        // - each environment maps each symbol to where the associated datum is
        // - lambda pushes new environment onto the compiler stack
        // - compile each expression in the body of the lambda
        // - concatenate output code?
    }

    fn compile_cmp(&mut self, cmp: &str, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`{cmp}` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        match cmp {
            "<" => instr.push(Instruction::LessThan),
            _ => todo!("comparison operator: {cmp}"),
        }
        Ok(instr)
    }

    fn compile_add(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`+` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        instr.push(Instruction::Add);
        Ok(instr)
    }

    fn compile_sub(&mut self, args: &[Expr]) -> Result<Vec<Instruction>, String> {
        if args.len() != 2 {
            return Err("`-` takes exactly 2 arguments".to_string());
        }
        let mut instr = self.compile_expr(&args[0])?;
        let mut instr_op2 = self.compile_expr(&args[1])?;
        instr.append(&mut instr_op2);
        instr.push(Instruction::Sub);
        Ok(instr)
    }
}

pub fn compile(exprs: &[Expr]) -> Result<Vec<Instruction>, String> {
    if exprs.len() != 1 {
        return Err("Only one expression can be compiled (for now)".to_string());
    }
    let mut comp = Compiler::new();
    comp.compile_expr(&exprs[0])
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
            StackAlloc { size: 1 },
            Push { value: Int(1) }, // push 1
            LoadArg { idx: 0 },     // put n on the stack
            LessThan,
            JumpIfTrue { offset: 3 }, // if 1 < n, jump to recursive case
            LoadArg { idx: 0 },       // put n on the stack
            Ret,                      // return n
            LoadArg { idx: 0 },
            Push { value: Int(1) },
            Sub,
            Call { addr: 3 }, // fib(n - 1)
            StoreLocal { offset: 0 },
            LoadArg { idx: 0 },
            Push { value: Int(2) },
            Sub,
            Call { addr: 3 }, // fib(n - 2)
            LoadLocal { offset: 0 },
            Add,
            Ret,
        ];

        let mut vm = VM::new(code);
        assert_eq!(vm.run(), Ok(Int(8)));
    }
}
