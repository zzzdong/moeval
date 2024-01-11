use std::{cmp, ops};

#[derive(Debug, Clone)]
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

impl ops::Add for &Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            _ => Value::Undefined,
        }
    }
}

impl ops::Sub for &Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            _ => Value::Undefined,
        }
    }
}
impl ops::Mul for &Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            _ => Value::Undefined,
        }
    }
}

impl ops::Div for &Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs / rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            _ => Value::Undefined,
        }
    }
}

impl ops::Rem for &Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
            _ => Value::Undefined,
        }
    }
}

impl cmp::PartialEq for &Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::Undefined, Value::Undefined) => true,
            _ => false,
        }
    }
}

impl cmp::PartialOrd for &Value {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.partial_cmp(rhs),
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}
