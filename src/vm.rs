use std::{collections::HashMap, fmt, sync::Arc};

use crate::{
    error::Error,
    opcode::{Instruction, Instructions, OpCode, Operand},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(Arc<String>),
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::Null => write!(f, "null"),
            PrimitiveType::Bool(b) => write!(f, "{}", b),
            PrimitiveType::Integer(i) => write!(f, "{}", i),
            PrimitiveType::Float(ff) => write!(f, "{}", ff),
            PrimitiveType::Char(c) => write!(f, "'{}'", c),
            PrimitiveType::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtReg(pub usize);

impl fmt::Display for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    Rbp = 13,
    Rsp = 14,
    Rpc = 15,
}

impl Register {
    pub fn registers() -> [Register; 8] {
        [
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
        ]
    }

    pub fn as_usize(&self) -> usize {
        *self as usize
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::R0 => write!(f, "R0"),
            Register::R1 => write!(f, "R1"),
            Register::R2 => write!(f, "R2"),
            Register::R3 => write!(f, "R3"),
            Register::R4 => write!(f, "R4"),
            Register::R5 => write!(f, "R5"),
            Register::R6 => write!(f, "R6"),
            Register::R7 => write!(f, "R7"),
            Register::R8 => write!(f, "R8"),
            Register::R9 => write!(f, "R9"),
            Register::R10 => write!(f, "R10"),
            Register::R11 => write!(f, "R11"),
            Register::R12 => write!(f, "R12"),
            Register::Rbp => write!(f, "Rbp"),
            Register::Rsp => write!(f, "Rsp"),
            Register::Rpc => write!(f, "Rpc"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackOffset(usize);

impl StackOffset {
    pub fn new(offset: usize) -> StackOffset {
        StackOffset(offset)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl fmt::Display for StackOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rbp+{}", self.0)
    }
}

pub struct Environment {
    map: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            map: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.map.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.map.get(name).cloned()
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.map.get_mut(name)
    }
}

pub struct Vm {
    pub registers: [Value; 8],
    pub stack: Vec<Value>,
    pub stack_pointer: usize,
    pub frame_pointer: usize,
    pub program_counter: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            registers: [
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
            ],
            stack: Vec::with_capacity(4 * 1024),
            stack_pointer: 0,
            frame_pointer: 0,
            program_counter: 0,
        }
    }

    pub fn execute(&mut self, program: Instructions, env: &Environment) -> Result<Value, Error> {
        for inst in program {
            let Instruction {
                opcode,
                operand0,
                operand1,
                operand2,
            } = inst;

            macro_rules! implement_binop_instruction {
                ($op: expr) => {{
                    let lhs = self.load_operand(operand1);
                    let rhs = self.load_operand(operand2);
                    let dest = $op(lhs, rhs)?;
                    self.store_operand(dest, operand0);
                }};
            }

            match opcode {
                OpCode::Push => {
                    let value = self.load_operand(operand0);
                    self.stack.push(value);
                    self.stack_pointer += 1;
                }
                OpCode::Pop => {
                    let value = self.stack.pop().unwrap();
                    self.store_operand(value, operand0);
                    self.stack_pointer -= 1;
                }
                OpCode::Move => {
                    let value = self.load_operand(operand1);
                    self.store_operand(value, operand0);
                }
                OpCode::LoadEnv => {
                    let value = self.load_env(operand1, env)?;
                    self.store_operand(value, operand0);
                }
                OpCode::Add => implement_binop_instruction!(Ops::add),
                OpCode::Sub => implement_binop_instruction!(Ops::sub),
                OpCode::Mul => implement_binop_instruction!(Ops::mul),
                OpCode::Div => implement_binop_instruction!(Ops::div),
                OpCode::Mod => implement_binop_instruction!(Ops::mod_),
                OpCode::Pow => implement_binop_instruction!(Ops::pow),
                OpCode::And => implement_binop_instruction!(Ops::and),
                OpCode::Or => implement_binop_instruction!(Ops::or),
                OpCode::IfEqual => implement_binop_instruction!(Ops::eq),
                OpCode::IfNotEqual => implement_binop_instruction!(Ops::ne),
                OpCode::IfGreater => implement_binop_instruction!(Ops::gt),
                OpCode::IfGreaterOrEqual => implement_binop_instruction!(Ops::gte),
                OpCode::IfLess => implement_binop_instruction!(Ops::lt),
                OpCode::IfLessOrEqual => implement_binop_instruction!(Ops::lte),
                _ => unimplemented!(),
            }
        }

        Ok(Value::Null)
    }

    fn load_operand(&self, operand: Operand) -> Value {
        match operand {
            Operand::Immed(value) => value.into(),
            Operand::Register(reg) => self.registers[reg as usize].clone(),
            Operand::Stack(offset) => self.stack[self.frame_pointer + offset.as_usize()].clone(),
            Operand::VirtReg(_) | Operand::None => unreachable!(),
        }
    }

    fn store_operand(&mut self, value: Value, operand: Operand) {
        match operand {
            Operand::Register(reg) => self.registers[reg as usize] = value,
            Operand::Immed(_) | Operand::Stack(_) | Operand::VirtReg(_) | Operand::None => {
                unreachable!()
            }
        }
    }

    fn load_env(&self, operand: Operand, environment: &Environment) -> Result<Value, Error> {
        match operand {
            Operand::Immed(PrimitiveType::String(env)) => {
                environment.get(&env).ok_or(Error::UndefinedVariable(env))
            }
            _ => Err(Error::InvalidArgument),
        }
    }

    pub fn get_register(&self, reg: Register) -> Value {
        self.registers[reg as usize].clone()
    }
}

pub trait Ops
    where 
        Self: Sized,
{
    fn add(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn sub(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn mul(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn div(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn mod_(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn pow(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn and(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn or(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn gt(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn gte(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn lt(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn lte(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn eq(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn ne(self, rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn call(self, args: &[Value]) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
    fn index(self, index: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented)
    }
}

impl Ops for Value {
    fn add(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs + rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 + rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs + rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            (Value::String(lhs), Value::String(rhs)) => {
                let mut s = lhs.to_string();
                s.push_str(&rhs);
                Ok(Value::String(Arc::new(s)))
            }
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn sub(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs - rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 - rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs - rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn mul(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs * rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 * rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs * rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn div(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs / rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 / rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs / rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn mod_(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs % rhs)),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn pow(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.pow(rhs as u32))),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn eq(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs == rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs as f64 == rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs == rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs == rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Bool(lhs == rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn ne(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs != rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs as f64 != rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs != rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs != rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Bool(lhs != rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn gt(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs as f64 > rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs > rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Bool(_), Value::Bool(_)) => Ok(Value::Bool(false)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn gte(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs as f64 >= rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs >= rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Value::Bool(_), Value::Bool(_)) => Ok(Value::Bool(true)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn lt(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) < rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs < rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Bool(_), Value::Bool(_)) => Ok(Value::Bool(false)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn lte(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs as f64 <= rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Bool(lhs <= rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Value::Bool(_), Value::Bool(_)) => Ok(Value::Bool(true)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn and(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs && rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn or(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs || rhs)),
            _ => Err(Error::OpUnimplemented),
        }
    }

    fn call(self, args: &[Value]) -> Result<Value, Error> {
        unimplemented!()
    }

    fn index(self, index: Value) -> Result<Value, Error> {
        unimplemented!()
    }
}
