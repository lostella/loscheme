#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    PushConstant(usize),
    Return,
    Jump(usize),
    JumpIfFalse(usize),
    Call(usize),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    name: String,
    arity: i32,
    code: Vec<Instruction>,
    constants: Vec<Value>,
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

pub struct StackFrame {
    code: Vec<Instruction>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    ip: usize,
}

impl StackFrame {
    pub fn new(code: Vec<Instruction>, constants: Vec<Value>) -> Self {
        StackFrame {
            code,
            constants,
            stack: Vec::new(),
            ip: 0,
        }
    }

    pub fn from_function(function: &Function) -> Self {
        Self::new(function.code.clone(), function.constants.clone())
    }

    fn fetch_instruction(&mut self) -> Result<Instruction, &'static str> {
        if self.ip >= self.code.len() {
            return Err("end of code");
        }
        let res = self.code[self.ip].clone();
        self.ip += 1;
        Ok(res)
    }

    fn step(&mut self) -> Result<Status, &'static str> {
        match self.fetch_instruction()? {
            Instruction::PushConstant(offset) => {
                self.stack.push(self.constants[offset]);
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
            Instruction::Call(n) => {
                // pop function object
                // create new frame (callee) for function
                // pop n values from stack, push onto callee frame
                // execute callee frame
                // on return: push return value onto stack
                todo!()
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
            if let Status::Return(v) = self.step()? {
                return Ok(v);
            }
        }
    }

    pub fn run(&mut self) -> Result<Value, &'static str> {
        loop {
            if let Status::Return(v) = self.step()? {
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
        let code = [
            Instruction::PushConstant(0),
            Instruction::PushConstant(1),
            Instruction::Add,
            Instruction::PushConstant(2),
            Instruction::Divide,
            Instruction::Negate,
            Instruction::Return,
        ];
        let constants = [Value::Float(1.2), Value::Float(3.4), Value::Float(5.6)];
        let mut frame = StackFrame::new(code.to_vec(), constants.to_vec());
        assert_eq!(frame.run(), Ok(Value::Float(-0.8214285714285714)))
    }

    #[test]
    fn test_jump_if() {
        let code = [
            Instruction::PushConstant(0),
            Instruction::PushConstant(1),
            Instruction::Leq,
            Instruction::JumpIfFalse(2),
            Instruction::PushConstant(2),
            Instruction::Jump(1),
            Instruction::PushConstant(3),
            Instruction::Return,
        ];
        let constants = [
            Value::Float(1.2),
            Value::Float(3.4),
            Value::Integer(42),
            Value::Integer(13),
        ];
        let mut frame = StackFrame::new(code.to_vec(), constants.to_vec());
        assert_eq!(frame.run(), Ok(Value::Integer(42)))
    }
    #[test]
    fn test_jump_else() {
        let code = [
            Instruction::PushConstant(0),
            Instruction::PushConstant(1),
            Instruction::Geq,
            Instruction::JumpIfFalse(2),
            Instruction::PushConstant(2),
            Instruction::Jump(1),
            Instruction::PushConstant(3),
            Instruction::Return,
        ];
        let constants = [
            Value::Float(1.2),
            Value::Float(3.4),
            Value::Integer(42),
            Value::Integer(13),
        ];
        let mut frame = StackFrame::new(code.to_vec(), constants.to_vec());
        assert_eq!(frame.run(), Ok(Value::Integer(13)))
    }
}
