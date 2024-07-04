use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::{self, Debug},
    ops::{self, Deref},
    rc::Rc,
};

use indexmap::IndexMap;

use crate::{
    ir::{types::Primitive, FunctionId},
    vm::RuntimeError,
};

pub trait Operate {
    fn add(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn sub(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn mul(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn div(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn modulo(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn pow(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn lt(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn gt(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn le(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn ge(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn eq(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn ne(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn and(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn or(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn not(&self) -> Result<Value, RuntimeError>;

    fn index_get(&self, index: ValueRef) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "indexing is not supported for this type",
        ))
    }

    fn index_set(&mut self, index: ValueRef, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "indexing is not supported for this type",
        ))
    }

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn call_member(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn iterator(&mut self) -> Result<Option<Box<dyn Iterator<Item = ValueRef>>>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }
}

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
    Tuple(Vec<ValueRef>),
    Array(Vec<ValueRef>),
    Map(IndexMap<Primitive, ValueRef>),
    Object(Box<dyn Object>),
    Function(FunctionId),
    ExternalFunction(Box<dyn Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError>>),
    Iterator(Box<dyn Iterator<Item = ValueRef>>),
    Range(Range),
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Boolean(b) => Ok(*b),
            _ => Err(RuntimeError::invalid_operation("not a bool")),
        }
    }

    pub fn as_integer(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Integer(i) => Ok(*i),
            _ => Err(RuntimeError::invalid_operation("not an integer")),
        }
    }

    pub fn as_float(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(RuntimeError::invalid_operation("not a float")),
        }
    }

    pub fn as_string(&self) -> Result<&str, RuntimeError> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(RuntimeError::invalid_operation("not a string")),
        }
    }

    pub fn as_primitive(&self) -> Result<Primitive, RuntimeError> {
        match self {
            Value::Boolean(b) => Ok(Primitive::Boolean(*b)),
            Value::Byte(b) => Ok(Primitive::Byte(*b)),
            Value::Integer(i) => Ok(Primitive::Integer(*i)),
            Value::Float(f) => Ok(Primitive::Float(*f)),
            Value::Char(c) => Ok(Primitive::Char(*c)),
            Value::String(s) => Ok(Primitive::String(s.clone())),
            _ => Err(RuntimeError::invalid_operation("not a primitive")),
        }
    }

    pub fn is_undefined(&self) -> bool {
        match self {
            Value::Undefined => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Value::Array(_) => true,
            _ => false,
        }
    }
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
            Self::Tuple(arg0) => f.debug_tuple("Tuple").field(arg0).finish(),
            Self::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Self::Map(arg0) => f.debug_tuple("Map").field(arg0).finish(),
            Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
            Self::Function(arg0) => f.debug_tuple("Function").field(arg0).finish(),
            Self::Range(arg0) => f.debug_tuple("Range").field(arg0).finish(),
            Self::Iterator(arg0) => f.debug_tuple("Iterator").finish(),
            Self::ExternalFunction(arg0) => f.debug_tuple("Function").finish(),
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
            Self::Tuple(arg0) => {
                f.write_str("(")?;
                if let Some((last, rest)) = arg0.split_last() {
                    for item in rest {
                        f.write_fmt(format_args!("{},", item))?;
                    }
                    f.write_str(&format!("{}", last))?;
                }
                f.write_str(")")
            }
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
            Self::Map(map) => {
                f.write_str("{")?;
                for (k, v) in map {
                    f.write_fmt(format_args!("{}:{},", k, v))?;
                }
                f.write_str("}")
            }
            Self::Function(id) => f.write_str(&format!("<Function {}>", id.as_usize())),
            Self::Object(_) => f.write_str("<Object>"),
            Self::Range(arg0) => f.write_str(&format!("<{}..={}>", arg0.begin, arg0.end)),
            Self::Iterator(_) => f.write_str("<Iterator>"),
            Self::ExternalFunction(_) => f.write_str("<ExternalFunction>"),
        }
    }
}

impl From<Primitive> for Value {
    fn from(value: Primitive) -> Self {
        match value {
            Primitive::Boolean(value) => Value::Boolean(value),
            Primitive::Byte(value) => Value::Byte(value),
            Primitive::Integer(value) => Value::Integer(value),
            Primitive::Float(value) => Value::Float(value),
            Primitive::Char(value) => Value::Char(value),
            Primitive::String(value) => Value::String(value),
            Primitive::Undefined => Value::Undefined,
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}
impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Byte(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Integer(value as i64)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Char(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
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

impl TryInto<i64> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Value::Integer(i64) => Ok(i64),
            _ => Err(RuntimeError::invalid_operation(
                "can only convert integer to i64",
            )),
        }
    }
}

impl TryInto<i64> for &Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Value::Integer(i) => Ok(*i),
            _ => Err(RuntimeError::invalid_operation(
                "can only convert integer to i64",
            )),
        }
    }
}

pub trait Object: Debug {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("Object is not callable"))
    }

    fn property_get(&self, prop: &str) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "Object property not supported",
        ))
    }

    fn property_set(&mut self, prop: &str, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "Object property not supported",
        ))
    }

    fn property_call(&mut self, prop: &str, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            "Object property not supported",
        ))
    }
    fn make_iterator(
        &mut self,
    ) -> Result<Option<Box<dyn Iterator<Item = ValueRef>>>, RuntimeError> {
        Err(RuntimeError::invalid_operation("Object is not iterator"))
    }
}

impl Operate for Value {
    fn add(&self, other: &Value) -> Result<Value, crate::vm::RuntimeError> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
            _ => Err(crate::vm::RuntimeError::invalid_operation(format!(
                "Invalid operands for addition, left: {}, right: {}",
                self, other
            ))),
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

    fn index_get(&self, index: ValueRef) -> Result<ValueRef, RuntimeError> {
        match self {
            Value::Tuple(t) => {
                let index = index.borrow().deref().as_integer()? as usize;
                if index >= t.len() {
                    Err(RuntimeError::invalid_operation("Tuple index out of bounds"))
                } else {
                    Ok(t[index as usize].clone())
                }
            }
            Value::Array(a) => {
                let index = index.borrow().deref().as_integer()? as usize;
                if index >= a.len() {
                    Err(RuntimeError::invalid_operation("Array index out of bounds"))
                } else {
                    Ok(a[index as usize].clone())
                }
            }
            Value::Map(m) => {
                let index = index.borrow();
                let index = index.deref().as_primitive()?;
                if m.contains_key(&index) {
                    Ok(m[&index].clone())
                } else {
                    Err(RuntimeError::invalid_operation("Map index out of bounds"))
                }
            }
            _ => Err(RuntimeError::invalid_operation("object not indexable")),
        }
    }

    fn index_set(&mut self, index: ValueRef, value: ValueRef) -> Result<(), RuntimeError> {
        match self {
            Value::Tuple(t) => {
                let index = index.borrow().deref().as_integer()? as usize;
                if index >= t.len() {
                    return Err(RuntimeError::invalid_operation("Tuple index out of bounds"));
                }
                t[index] = value;
                Ok(())
            }
            Value::Array(a) => {
                let index = index.borrow().deref().as_integer()? as usize;
                if index >= a.len() {
                    return Err(RuntimeError::invalid_operation("Array index out of bounds"));
                }
                a[index] = value;
                Ok(())
            }
            Value::Map(m) => {
                let index = index.borrow().deref().as_primitive()?;
                m.insert(index, value);
                Ok(())
            }
            _ => Err(RuntimeError::invalid_operation("object not indexable")),
        }
    }

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref mut o) => o.call(args),
            Value::ExternalFunction(ref mut f) => f(args),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for function call",
            )),
        }
    }

    fn call_member(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref mut o) => o.call(args),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for call memeber",
            )),
        }
    }

    fn iterator(
        &mut self,
    ) -> Result<Option<Box<dyn Iterator<Item = ValueRef>>>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref mut o) => o.make_iterator(),
            Value::Range(r) => Ok(Some(Box::new(RangeIterator::new(r)?))),
            Value::Array(a) => Ok(Some(Box::new(a.clone().into_iter()))),
            Value::Map(m) => Ok(Some(Box::new(m.clone().into_iter().map(|(k, v)| {
                ValueRef::new(Value::Tuple(vec![ValueRef::new(Value::from(k)), v]))
            })))),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for iterator call",
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ValueRef(Rc<RefCell<Value>>);

impl ValueRef {
    pub fn new(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }

    pub fn borrow(&self) -> Ref<Value> {
        self.0.as_ref().borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<Value> {
        self.0.borrow_mut()
    }

    pub fn as_ptr(&self) -> *mut Value {
        self.0.as_ptr()
    }

    pub fn take(self) -> Value {
        self.0.take()
    }
}

impl Default for ValueRef {
    fn default() -> Self {
        ValueRef::new(Value::default())
    }
}

impl PartialEq<Value> for ValueRef {
    fn eq(&self, other: &Value) -> bool {
        PartialEq::eq(self.borrow().deref(), other)
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{}", self.borrow().deref()))
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        ValueRef::new(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Range {
    begin: i64,
    end: i64,
}

impl Range {
    pub fn new(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        match (begin.borrow().deref(), end.borrow().deref()) {
            (Value::Integer(begin), Value::Integer(end)) => Ok(Self {
                begin: *begin,
                end: *end,
            }),
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
    type Item = ValueRef;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| ValueRef::new(Value::Integer(v)))
    }
}
