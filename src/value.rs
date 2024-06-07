use std::{cmp, ops};

use crate::vm::Operate;

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    Undefined,
}

impl Default for Value {
    fn default() -> Self {
        Value::Undefined
    }
}


impl Operate for Value {
    fn add(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for addition")),
        }
    }

    fn sub(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for subtraction")),
        }
    }

    fn mul(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for multiplication")),
        }
    }

    fn div(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l / r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for division")),
        }
    }

    fn modulo(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l % r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for modulo")),
        }
    }

    fn pow(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.pow(*r as u32))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(*r))),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for exponentiation")),
        }
    }

    fn lt(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l < r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l < r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l < r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn le(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l <= r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l <= r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn gt(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l > r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l > r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l > r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn ge(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l >= r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l >= r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn eq(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l == r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l == r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l == r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l == r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l == r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l == r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn ne(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l != r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l != r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l != r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l != r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l != r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l != r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for comparison")),
        }
    }

    fn and(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l && *r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for logical and")),
        }
    }

    fn or(&self, other: &Self) -> Result<Self, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l || *r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operands for logical or")),
        }
    }


    fn not(&self) -> Result<Self, crate::vm::RuntimeError> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operand for negation")),
        }
    }

    // fn call(&self, args: &[Self]) -> Result<Option<Self>, crate::vm::RuntimeError> {
    //     match self {
    //         Value::Function(f) => f(args),
    //         _ => Err(crate::vm::RuntimeError::invalid_operation("Invalid operand for function call")),
    //     }
    // }
}
















