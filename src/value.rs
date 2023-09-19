use std::{collections::HashMap, sync::Arc, fmt, any::Any};

use crate::vm::PrimitiveType;

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


impl From<PrimitiveType> for Value {
    fn from(value: PrimitiveType) -> Self {
        match value {
            PrimitiveType::Null => Self::Null,
            PrimitiveType::Bool(value) => Self::Bool(value),
            PrimitiveType::Integer(value) => Self::Integer(value),
            PrimitiveType::Float(value) => Self::Float(value),
            PrimitiveType::Char(value) => Self::Char(value),
            PrimitiveType::String(value) => Self::String(value),
        }
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
    fn get_field(&self, name: &str) -> Option<Value> {
        None
    }
    fn set_field(&mut self, name: &str, value: Value) {

    }
    fn call(&mut self, args: &[Value]) -> Option<Value> {
        None
    }
    fn call_method(&mut self, name: &str, args: &[Value]) -> Option<Value> {
        None
    }
}