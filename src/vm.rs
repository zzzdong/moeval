use std::{collections::HashMap, sync::Arc};

use crate::{
    compiler::irmodule::{self, IRBuilder, IRModule, InstructionData},
    error::Error,
    instruction::{Instruction, Module, Opcode, Operand, Register},
    value::{Value},
};

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

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
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

    pub fn execute(&mut self, program: Module, env: &Environment) -> Result<Value, Error> {
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
                Opcode::Push => {
                    let value = self.load_operand(operand0);
                    self.stack.push(value);
                    self.stack_pointer += 1;
                }
                Opcode::Pop => {
                    let value = self.stack.pop().unwrap();
                    self.store_operand(value, operand0);
                    self.stack_pointer -= 1;
                }
                Opcode::Move => {
                    let value = self.load_operand(operand1);
                    self.store_operand(value, operand0);
                }
                Opcode::LoadEnv => {
                    let value = self.load_env(operand1, env)?;
                    self.store_operand(value, operand0);
                }
                Opcode::Return => {
                    let value = self.load_operand(operand0);
                    return Ok(value);
                }
                Opcode::Add => implement_binop_instruction!(Ops::add),
                Opcode::Sub => implement_binop_instruction!(Ops::sub),
                Opcode::Mul => implement_binop_instruction!(Ops::mul),
                Opcode::Div => implement_binop_instruction!(Ops::div),
                Opcode::Mod => implement_binop_instruction!(Ops::mod_),
                Opcode::Pow => implement_binop_instruction!(Ops::pow),
                Opcode::And => implement_binop_instruction!(Ops::and),
                Opcode::Or => implement_binop_instruction!(Ops::or),
                Opcode::IfEqual => implement_binop_instruction!(Ops::eq),
                Opcode::IfNotEqual => implement_binop_instruction!(Ops::ne),
                Opcode::IfGreater => implement_binop_instruction!(Ops::gt),
                Opcode::IfGreaterOrEqual => implement_binop_instruction!(Ops::gte),
                Opcode::IfLess => implement_binop_instruction!(Ops::lt),
                Opcode::IfLessOrEqual => implement_binop_instruction!(Ops::lte),
                Opcode::In => implement_binop_instruction!(Ops::in_),
                Opcode::Index => implement_binop_instruction!(Ops::index),
                Opcode::NewArray => {
                    let array: Vec<Value> = Vec::new();
                    self.store_operand(Value::Array(array), operand0);
                }
                Opcode::ArrayPush => {
                    let value = self.load_operand(operand1);
                    let array = self.load_operand_mut(operand0);
                    match array {
                        Value::Array(array) => {
                            array.push(value);
                        }
                        _ => return Err(Error::InvalidArgument),
                    }
                }
                Opcode::NewDictionary => {
                    let dict: HashMap<String, Value> = HashMap::new();
                    self.store_operand(Value::Dictionary(dict), operand0);
                }
                Opcode::DictionaryPut => {
                    let key = self.load_operand(operand1);
                    let value = self.load_operand(operand2);
                    let dict = self.load_operand_mut(operand0);
                    match (dict, key) {
                        (Value::Dictionary(dict), Value::String(key)) => {
                            dict.insert(key, value);
                        }
                        _ => return Err(Error::InvalidArgument),
                    }
                }
                _ => unimplemented!("{opcode:?}"),
            }
        }

        Ok(Value::Null)
    }

    fn load_operand(&self, operand: Operand) -> Value {
        match operand {
            Operand::Immed(value) => value.into(),
            Operand::Register(reg) => self.registers[reg as usize].clone(),
            Operand::Stack(stack) => self.stack[self.frame_pointer + stack.offset].clone(),
            Operand::VirtReg(_) | Operand::None => unreachable!(),
        }
    }

    fn load_operand_mut(&mut self, operand: Operand) -> &mut Value {
        match operand {
            Operand::Register(reg) => &mut self.registers[reg as usize],
            Operand::Stack(stack) => &mut self.stack[self.frame_pointer + stack.offset],
            Operand::VirtReg(_) | Operand::Immed(_) | Operand::None => unreachable!(),
        }
    }

    fn store_operand(&mut self, value: Value, dest: Operand) {
        match dest {
            Operand::Register(reg) => self.registers[reg as usize] = value,
            Operand::Immed(_) | Operand::Stack(_) | Operand::VirtReg(_) | Operand::None => {
                unreachable!()
            }
        }
    }

    fn load_env(&self, operand: Operand, environment: &Environment) -> Result<Value, Error> {
        match operand {
            Operand::Immed(Value::String(env)) => {
                environment.get(&env).ok_or(Error::UndefinedVariable(env))
            }
            _ => Err(Error::InvalidArgument),
        }
    }

    pub fn get_register(&self, reg: Register) -> Value {
        self.registers[reg as usize].clone()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Vm::new()
    }
}
pub trait Ops
where
    Self: Sized,
{
    fn add(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Add))
    }
    fn sub(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Sub))
    }
    fn mul(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Sub))
    }
    fn div(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Div))
    }
    fn mod_(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Mod))
    }
    fn pow(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Pow))
    }
    fn and(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::And))
    }
    fn or(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Or))
    }
    fn gt(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfGreater))
    }
    fn gte(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfGreaterOrEqual))
    }
    fn lt(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfLess))
    }
    fn lte(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfLessOrEqual))
    }
    fn eq(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfEqual))
    }
    fn ne(self, _rhs: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfNotEqual))
    }
    fn neg(self) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Negate))
    }
    fn not(self) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Not))
    }
    fn in_(self, _obj: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::In))
    }
    fn index(self, _index: Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Index))
    }
    fn call(self, _args: &[Value]) -> Result<Option<Value>, Error> {
        Err(Error::OpUnimplemented(Opcode::Call))
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
                let mut s = lhs.clone();
                s.push_str(&rhs);
                Ok(Value::String(s))
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn sub(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs - rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 - rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs - rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn mul(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs * rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 * rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs * rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn div(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs / rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 / rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs / rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn mod_(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs % rhs)),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn pow(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.pow(rhs as u32))),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn eq(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs as f64 == rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs == rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn ne(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs as f64 != rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs != rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn gt(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs > rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs as f64 > rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs > rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs > rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(false)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn gte(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs as f64 >= rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs >= rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(true)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn lt(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs < rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean((lhs as f64) < rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs < rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs < rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(false)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn lte(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs as f64 <= rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs <= rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(true)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn and(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs && rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn or(self, rhs: Value) -> Result<Value, Error> {
        match (self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs || rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn not(self) -> Result<Value, Error> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn neg(self) -> Result<Value, Error> {
        match self {
            Value::Integer(i) => Ok(Value::Integer(-i)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn in_(self, obj: Value) -> Result<Value, Error> {
        match (obj, self) {
            (Value::Array(array), ele) => {
                for item in &array {
                    if item == &ele {
                        return Ok(Value::Boolean(true));
                    }
                }

                Ok(Value::Boolean(false))
            }
            (Value::Dictionary(map), Value::String(key)) => {
                Ok(Value::Boolean(map.get(key.as_str()).is_some()))
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn index(self, index: Value) -> Result<Value, Error> {
        match (self, index) {
            (Value::Array(array), Value::Integer(i)) => {
                if i >= 0 && (i as usize) < array.len() {
                    Ok(array[i as usize].clone())
                } else {
                    Err(Error::IndexOutOfBounds(i as usize, array.len()))
                }
            }
            (Value::Dictionary(map), Value::String(key)) => {
                if let Some(value) = map.get(key.as_str()) {
                    Ok(value.clone())
                } else {
                    Err(Error::EntryNotFound(key))
                }
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn call(self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self {
            Value::Dynamic(obj) => obj.call(args),
            _ => Err(Error::OpUnimplemented(Opcode::Call)),
        }
    }
}
