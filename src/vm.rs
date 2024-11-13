#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Instruction {
    LoadConstant(usize),
    LoadLocal(usize),
    StoreLocal(usize),
    Return,
    Jump(usize),
    JumpIfFalse(usize),
    CallClone(usize),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Eq,
    Lt,
    Gt,
    Leq,
    Geq,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

impl Value {
    fn add(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a + *b as f64)),
            _ => Err("Cannot add types"),
        }
    }

    fn mul(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a * *b as f64)),
            _ => Err("Cannot multiply types"),
        }
    }

    fn sub(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err("Cannot subtract types"),
        }
    }

    fn div(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Float(*a as f64 / *b as f64)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a / *b as f64)),
            _ => Err("Cannot divide types"),
        }
    }

    fn lt(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) < *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a < (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn gt(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) > *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a > (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn leq(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) <= *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a <= (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn geq(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool((*a as f64) >= *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a >= (*b as f64))),
            _ => Err("Cannot compare types"),
        }
    }

    fn iseq(&self, other: &Value) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a == b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Bool(*a as f64 == *b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Bool(*a == *b as f64)),
            _ => Err("Cannot compare types"),
        }
    }

    fn neg(&self) -> Result<Value, &'static str> {
        match self {
            Value::Integer(a) => Ok(Value::Integer(-a)),
            Value::Float(a) => Ok(Value::Float(-a)),
            _ => Err("Cannot negate type"),
        }
    }
}

#[derive(PartialEq)]
enum Status {
    Continue,
    Return(Value),
}

pub struct StackFrame<'a> {
    code: &'a Vec<Instruction>,
    constants: &'a Vec<Value>,
    locals: Vec<Value>,
    stack: Vec<Value>,
    ip: usize,
}

impl<'a> StackFrame<'a> {
    pub fn new(code: &'a Vec<Instruction>, constants: &'a Vec<Value>, locals: Vec<Value>) -> Self {
        Self {
            code,
            constants,
            locals,
            stack: Vec::new(),
            ip: 0,
        }
    }

    fn fetch_instruction(&mut self) -> Result<Instruction, &'static str> {
        if self.ip >= self.code.len() {
            return Err("end of code");
        }
        let res = self.code[self.ip];
        self.ip += 1;
        Ok(res)
    }

    fn execute(&mut self, instruction: Instruction) -> Result<Status, &'static str> {
        match instruction {
            Instruction::LoadConstant(offset) => {
                self.stack.push(self.constants[offset]);
                Ok(Status::Continue)
            }
            Instruction::LoadLocal(offset) => {
                self.stack.push(self.locals[offset]);
                Ok(Status::Continue)
            }
            Instruction::StoreLocal(offset) => {
                self.locals[offset] = self.stack.pop().unwrap();
                Ok(Status::Continue)
            }
            Instruction::Return => Ok(Status::Return(self.stack.pop().unwrap())),
            Instruction::Jump(offset) => {
                self.ip += offset;
                Ok(Status::Continue)
            }
            Instruction::JumpIfFalse(offset) => {
                if let Value::Bool(false) = self.stack.pop().unwrap() {
                    self.ip += offset
                }
                Ok(Status::Continue)
            }
            Instruction::CallClone(n) => {
                let mut locals = vec![];
                for _ in 0..n {
                    locals.push(self.stack.pop().ok_or("empty stack")?);
                }
                let mut callee_frame = StackFrame::new(self.code, self.constants, locals);
                let ret = callee_frame.run();
                self.stack.push(ret?);
                Ok(Status::Continue)
            }
            Instruction::Negate => {
                let v = self.stack.pop().unwrap();
                self.stack.push(v.neg()?);
                Ok(Status::Continue)
            }
            Instruction::Add => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.add(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Subtract => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.sub(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Multiply => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.mul(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Divide => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.div(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Eq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.iseq(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Lt => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.lt(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Gt => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.gt(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Leq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.leq(&b)?);
                Ok(Status::Continue)
            }
            Instruction::Geq => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a.geq(&b)?);
                Ok(Status::Continue)
            }
        }
    }

    pub fn debug(&mut self) -> Result<Value, &'static str> {
        loop {
            println!("{:?}", self.stack);
            let instruction = self.fetch_instruction()?;
            println!("{}, {:?}", self.ip - 1, instruction);
            if let Status::Return(v) = self.execute(instruction)? {
                return Ok(v);
            }
        }
    }

    pub fn run(&mut self) -> Result<Value, &'static str> {
        loop {
            let instruction = self.fetch_instruction()?;
            if let Status::Return(v) = self.execute(instruction)? {
                return Ok(v);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetics() {
        let code = vec![
            Instruction::LoadConstant(0),
            Instruction::LoadConstant(1),
            Instruction::Add,
            Instruction::LoadConstant(2),
            Instruction::Divide,
            Instruction::Negate,
            Instruction::Return,
        ];
        let constants = vec![Value::Float(1.2), Value::Float(3.4), Value::Float(5.6)];
        let mut frame = StackFrame::new(&code, &constants, vec![]);
        assert_eq!(frame.run(), Ok(Value::Float(-0.8214285714285714)))
    }

    #[test]
    fn test_jump_if() {
        let code = vec![
            Instruction::LoadConstant(0),
            Instruction::LoadConstant(1),
            Instruction::Leq,
            Instruction::JumpIfFalse(2),
            Instruction::LoadConstant(2),
            Instruction::Jump(1),
            Instruction::LoadConstant(3),
            Instruction::Return,
        ];
        let constants = vec![
            Value::Float(1.2),
            Value::Float(3.4),
            Value::Integer(42),
            Value::Integer(13),
        ];
        let mut frame = StackFrame::new(&code, &constants, vec![]);
        assert_eq!(frame.run(), Ok(Value::Integer(42)))
    }

    #[test]
    fn test_jump_else() {
        let code = vec![
            Instruction::LoadConstant(0),
            Instruction::LoadConstant(1),
            Instruction::Geq,
            Instruction::JumpIfFalse(2),
            Instruction::LoadConstant(2),
            Instruction::Jump(1),
            Instruction::LoadConstant(3),
            Instruction::Return,
        ];
        let constants = vec![
            Value::Float(1.2),
            Value::Float(3.4),
            Value::Integer(42),
            Value::Integer(13),
        ];
        let mut frame = StackFrame::new(&code, &constants, vec![]);
        assert_eq!(frame.run(), Ok(Value::Integer(13)))
    }

    #[test]
    fn test_fibonacci() {
        let code = vec![
            Instruction::LoadLocal(0),
            Instruction::LoadConstant(1),
            Instruction::Lt,
            Instruction::JumpIfFalse(2),
            Instruction::LoadLocal(0),
            Instruction::Return,
            Instruction::LoadLocal(0),
            Instruction::LoadConstant(0),
            Instruction::Subtract,
            Instruction::CallClone(1),
            Instruction::LoadLocal(0),
            Instruction::LoadConstant(1),
            Instruction::Subtract,
            Instruction::CallClone(1),
            Instruction::Add,
            Instruction::Return,
        ];
        let constants = vec![Value::Integer(1), Value::Integer(2)];
        let locals = vec![Value::Integer(9)];
        let mut frame = StackFrame::new(&code, &constants, locals);
        assert_eq!(frame.run(), Ok(Value::Integer(34)))
    }
}
