use std::{any::Any, collections::HashMap, fmt, sync::Arc};

use crate::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(Arc<String>),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Null => write!(f, "null"),
            Primitive::Bool(b) => write!(f, "{}", b),
            Primitive::Integer(i) => write!(f, "{}", i),
            Primitive::Float(ff) => write!(f, "{}", ff),
            Primitive::Char(c) => write!(f, "'{}'", c),
            Primitive::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Null,
    Bool,
    Integer,
    Float,
    String,
    Array,
    Dictionary,
}

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(Arc<String>),
    Array(Vec<Value>),
    Dictionary(HashMap<String, Value>),
    Dynamic(Arc<dyn Object>),
}

impl From<Primitive> for Value {
    fn from(value: Primitive) -> Self {
        match value {
            Primitive::Null => Self::Null,
            Primitive::Bool(value) => Self::Bool(value),
            Primitive::Integer(value) => Self::Integer(value),
            Primitive::Float(value) => Self::Float(value),
            Primitive::Char(value) => Self::Char(value),
            Primitive::String(value) => Self::String(value),
        }
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self::Null,
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

macro_rules! impl_from_int {
    ($t:ty) => {
        impl From<$t> for Value {
            fn from(value: $t) -> Self {
                Self::Integer(value as i64)
            }
        }
    };
}

impl_from_int!(i8);
impl_from_int!(u8);
impl_from_int!(i16);
impl_from_int!(u16);
impl_from_int!(i32);
impl_from_int!(u32);
impl_from_int!(i64);
impl_from_int!(u64);
impl_from_int!(isize);
impl_from_int!(usize);

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value as f64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(Arc::new(value))
    }
}

impl From<Arc<String>> for Value {
    fn from(value: Arc<String>) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string().into())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Integer(rhs)) => lhs == rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Array(lhs), Self::Array(rhs)) => lhs == rhs,
            (Self::Dictionary(lhs), Self::Dictionary(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl std::ops::Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs + rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs + rhs),
            (Self::Integer(lhs), Self::Float(rhs)) => Self::Float(lhs as f64 + rhs),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(lhs + rhs as f64),
            (Self::String(lhs), Self::String(rhs)) => {
                let mut s = lhs.to_string();
                s.push_str(&rhs);
                Self::String(Arc::new(s))
            }
            (lhs, rhs) => panic!("Cannot add {:?} and {:?}", lhs, rhs),
        }
    }
}

pub trait Object: fmt::Display + fmt::Debug + Any + Sync + Send {
    fn get_field(&self, _name: &str) -> Result<Option<Value>, Error> {
        Err(Error::OpUnimplemented)
    }
    fn set_field(&self, _name: &str, _value: Value) -> Result<(), Error> {
        Err(Error::OpUnimplemented)
    }
    fn call(&self, _args: &[Value]) -> Result<Option<Value>, Error> {
        Err(Error::OpUnimplemented)
    }
    fn call_method(&self, _name: &str, _args: &[Value]) -> Result<Option<Value>, Error> {
        Err(Error::OpUnimplemented)
    }
}
