use std::{
    cell::RefCell,
    fmt::{self, Debug},
    ops,
    rc::Rc,
};

use crate::vm::{Operate, RuntimeError};

#[derive(Default)]
pub enum Value {
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    #[default]
    Undefined,
    Array(Vec<Value>),
    Object(Rc<RefCell<Box<dyn Object>>>),
    Function(Rc<RefCell<Box<dyn Fn(&[Value]) -> Result<Option<Value>, RuntimeError>>>>),
    Iterator(Box<dyn Iterator<Item = Value>>),
    Range(Range),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::Byte(arg0) => f.debug_tuple("Byte").field(arg0).finish(),
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Char(arg0) => f.debug_tuple("Char").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Undefined => write!(f, "Undefined"),
            Self::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
            Self::Range(arg0) => f.debug_tuple("Range").field(arg0).finish(),
            Self::Iterator(arg0) => f.debug_tuple("Iterator").finish(),
            Self::Function(arg0) => f.debug_tuple("Function").finish(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(arg0) => f.write_str(if *arg0 { "true" } else { "false" }),
            Self::Byte(arg0) => f.write_str(&format!("{}", arg0)),
            Self::Integer(arg0) => f.write_str(&format!("{}", arg0)),
            Self::Float(arg0) => f.write_str(&format!("{}", arg0)),
            Self::Char(arg0) => f.write_str(&format!("{}", arg0)),
            Self::String(arg0) => f.write_str(&arg0.to_string()),
            Self::Undefined => f.write_str("<undefined>"),
            Self::Array(arg0) => {
                f.write_str("[")?;
                if let Some((last, rest)) = arg0.split_last() {
                    for item in rest {
                        f.write_fmt(format_args!("{},", item))?;
                    }
                    f.write_str(&format!("{}", last))?;
                }
                f.write_str("]")
            }
            Self::Object(_) => f.write_str("<Object>"),
            Self::Range(arg0) => f.write_str(&format!("<{}..={}>", arg0.begin, arg0.end)),
            Self::Iterator(_) => f.write_str("<Iterator>"),
            Self::Function(_) => f.write_str("<Function>"),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Boolean(arg0) => Self::Boolean(*arg0),
            Self::Byte(arg0) => Self::Byte(*arg0),
            Self::Integer(arg0) => Self::Integer(*arg0),
            Self::Float(arg0) => Self::Float(*arg0),
            Self::Char(arg0) => Self::Char(*arg0),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Undefined => Self::Undefined,
            Self::Array(arg0) => Self::Array(arg0.clone()),
            Self::Object(arg0) => Self::Object(arg0.clone()),
            Self::Range(arg0) => Self::Range(*arg0),
            Self::Function(arg0) => Self::Function(arg0.clone()),
            Self::Iterator(arg0) => unimplemented!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Byte(l0), Self::Byte(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => l0.as_ptr() == r0.as_ptr(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Boolean(l0), Self::Boolean(r0)) => l0.partial_cmp(r0),
            (Self::Byte(l0), Self::Byte(r0)) => l0.partial_cmp(r0),
            (Self::Integer(l0), Self::Integer(r0)) => l0.partial_cmp(r0),
            (Self::Float(l0), Self::Float(r0)) => l0.partial_cmp(r0),
            (Self::Char(l0), Self::Char(r0)) => l0.partial_cmp(r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(r0),
            (Self::Array(l0), Self::Array(r0)) => l0.partial_cmp(r0),
            _ => None,
        }
    }
}

impl PartialEq<i64> for Value {
    fn eq(&self, other: &i64) -> bool {
        match self {
            Self::Integer(l0) => l0 == other,
            _ => false,
        }
    }
}

impl PartialOrd<i64> for Value {
    fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
        match self {
            Value::Integer(l0) => l0.partial_cmp(other),
            _ => None,
        }
    }
}

impl ops::Add<i64> for Value {
    type Output = Value;

    fn add(self, other: i64) -> Self::Output {
        match self {
            Self::Integer(i) => Value::Integer(i + other),
            _ => unimplemented!("can only add integer to integer"),
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l + r),
            _ => unimplemented!("can only add integer to integer"),
        }
    }
}

impl ops::Sub<i64> for Value {
    type Output = Value;

    fn sub(self, rhs: i64) -> Self::Output {
        match self {
            Value::Integer(i) => Value::Integer(i - rhs),
            _ => unimplemented!("can only sub integer to integer"),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(l), Value::Integer(r)) => Value::Integer(l - r),
            _ => unimplemented!("can only sub integer to integer"),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

pub trait Object: Debug {
    fn call(&mut self, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("Object is not callable"))
    }
    fn property(&mut self, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "Object property not supported",
        ))
    }
    fn make_iterator(&mut self) -> Result<Option<Box<dyn Iterator<Item = Value>>>, RuntimeError> {
        Err(RuntimeError::invalid_operation("Object is not iterator"))
    }
}

impl Operate for Value {
    fn add(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            _ => {
                println!("Invalid operands for addition, lhs:{self:?}, rhs:{other:?}");
                Err(crate::vm::RuntimeError::invalid_operation(
                    "Invalid operands for addition",
                ))
            }
        }
    }

    fn sub(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for subtraction",
            )),
        }
    }

    fn mul(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for multiplication",
            )),
        }
    }

    fn div(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l / r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for division",
            )),
        }
    }

    fn modulo(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l % r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for modulo",
            )),
        }
    }

    fn pow(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l.pow(*r as u32))),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(*r))),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for exponentiation",
            )),
        }
    }

    fn lt(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l < r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l < r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l < r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn le(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l <= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l <= r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l <= r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn gt(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l > r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l > r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l > r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn ge(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l >= r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l >= r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l >= r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn eq(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l == r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l == r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l == r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l == r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l == r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l == r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn ne(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Boolean(l != r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l != r)),
            (Value::Char(l), Value::Char(r)) => Ok(Value::Boolean(l != r)),
            (Value::Byte(l), Value::Byte(r)) => Ok(Value::Boolean(l != r)),
            (Value::String(l), Value::String(r)) => Ok(Value::Boolean(l != r)),
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l != r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for comparison",
            )),
        }
    }

    fn and(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l && *r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for logical and",
            )),
        }
    }

    fn or(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l || *r)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for logical or",
            )),
        }
    }

    fn not(&self) -> Result<Value, crate::vm::RuntimeError> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for negation",
            )),
        }
    }

    fn call(&mut self, args: &[Value]) -> Result<Option<Value>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref o) => o.as_ref().borrow_mut().call(args),
            Value::Function(ref f) => f.as_ref().borrow_mut()(args),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for function call",
            )),
        }
    }

    fn call_member(
        &mut self,
        member: &str,
        this: &mut Value,
        args: &[Value],
    ) -> Result<Option<Value>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref o) => o.as_ref().borrow_mut().call(args),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for call memeber",
            )),
        }
    }

    fn iterator(&self) -> Result<Option<Box<dyn Iterator<Item = Value>>>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref o) => o.as_ref().borrow_mut().make_iterator(),
            Value::Array(a) => Ok(Some(Box::new(a.clone().into_iter()))),
            Value::Range(r) => Ok(Some(Box::new(RangeIterator::new(r)?))),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for iterator call",
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Range {
    begin: i64,
    end: i64,
}

impl Range {
    pub fn new(begin: Value, end: Value) -> Result<Self, RuntimeError> {
        match (begin, end) {
            (Value::Integer(begin), Value::Integer(end)) => Ok(Self { begin, end }),
            _ => Err(RuntimeError::invalid_operation(
                "Invalid operands for range",
            )),
        }
    }
}

pub(crate) struct RangeIterator {
    inner: Box<dyn Iterator<Item = i64>>,
}

impl fmt::Debug for RangeIterator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RangeIterator").finish()
    }
}

impl RangeIterator {
    pub fn new(range: &Range) -> Result<Self, RuntimeError> {
        Ok(Self {
            inner: Box::new(range.begin..=range.end),
        })
    }
}

impl Iterator for RangeIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Value::Integer)
    }
}
