use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
    fmt,
    rc::Rc,
};

use super::Object;
use super::RuntimeError;
use crate::bytecode::{Constant, Primitive};

/// Helper to convert a `T: Object + 'static` into the appropriate inline `Value`
/// variant. Primitive types (`bool`, `i64`, `f64`, `char`, `String`) are stored
/// inline rather than wrapped in `Value::Object`.
fn into_value<T: Object + 'static>(object: T) -> Value {
    let type_id = TypeId::of::<T>();
    if type_id == TypeId::of::<bool>() {
        let b = unsafe { std::ptr::read(&object as *const T as *const bool) };
        std::mem::forget(object);
        Value::Bool(b)
    } else if type_id == TypeId::of::<i64>() {
        let i = unsafe { std::ptr::read(&object as *const T as *const i64) };
        std::mem::forget(object);
        Value::Int(i)
    } else if type_id == TypeId::of::<f64>() {
        let f = unsafe { std::ptr::read(&object as *const T as *const f64) };
        std::mem::forget(object);
        Value::Float(f)
    } else if type_id == TypeId::of::<char>() {
        let c = unsafe { std::ptr::read(&object as *const T as *const char) };
        std::mem::forget(object);
        Value::Char(c)
    } else if type_id == TypeId::of::<String>() {
        let s = unsafe { std::ptr::read(&object as *const T as *const String) };
        std::mem::forget(object);
        Value::Str(Rc::new(s))
    } else {
        Value::Object(Box::new(object))
    }
}

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Char(char),
    Str(Rc<String>),
    Object(Box<dyn Object>),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Null => Value::Null,
            Value::Bool(b) => Value::Bool(*b),
            Value::Int(i) => Value::Int(*i),
            Value::Float(f) => Value::Float(*f),
            Value::Char(c) => Value::Char(*c),
            Value::Str(s) => Value::Str(s.clone()),
            Value::Object(obj) => Value::Object(obj.clone_box()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => {
                (&**a as *const dyn Object) == (&**b as *const dyn Object)
            }
            _ => false,
        }
    }
}

impl Value {
    pub fn new<T: Object + 'static>(object: T) -> Self {
        into_value(object)
    }

    pub fn null() -> Self {
        Value::Null
    }

    pub fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        match self {
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<T>(),
            Value::Null if TypeId::of::<T>() == TypeId::of::<super::object::Null>() => {
                const NULL: super::object::Null = super::object::Null;
                unsafe { Some(&*(&NULL as *const super::object::Null as *const T)) }
            }
            Value::Null if TypeId::of::<T>() == TypeId::of::<()>() => {
                unsafe { Some(&*(&() as *const () as *const T)) }
            }
            Value::Bool(b) if TypeId::of::<T>() == TypeId::of::<bool>() => {
                unsafe { Some(&*(b as *const bool as *const T)) }
            }
            Value::Int(i) if TypeId::of::<T>() == TypeId::of::<i64>() => {
                unsafe { Some(&*(i as *const i64 as *const T)) }
            }
            Value::Float(f) if TypeId::of::<T>() == TypeId::of::<f64>() => {
                unsafe { Some(&*(f as *const f64 as *const T)) }
            }
            Value::Char(c) if TypeId::of::<T>() == TypeId::of::<char>() => {
                unsafe { Some(&*(c as *const char as *const T)) }
            }
            Value::Str(s) if TypeId::of::<T>() == TypeId::of::<String>() => {
                unsafe { Some(&*(s.as_ref() as *const String as *const T)) }
            }
            _ => None,
        }
    }

    pub fn downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        match self {
            Value::Object(obj) => (&mut **obj as &mut dyn Any).downcast_mut::<T>(),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&dyn Object> {
        match self {
            Value::Object(obj) => Some(&**obj),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut dyn Object> {
        match self {
            Value::Object(obj) => Some(&mut **obj),
            _ => None,
        }
    }

    pub fn into_inner<T: 'static>(self) -> Result<T, Self> {
        match self {
            Value::Object(obj) => {
                // Check type match before consuming the value
                let type_id = (obj.as_ref() as &dyn Any).type_id();
                if type_id == std::any::TypeId::of::<T>() {
                    let any_box: Box<dyn Any> = unsafe {
                        std::mem::transmute::<Box<dyn Object>, Box<dyn Any>>(obj)
                    };
                    Ok(*any_box.downcast::<T>().unwrap())
                } else {
                    Err(Value::Object(obj))
                }
            }
            other => Err(other),
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<i64>().copied(),
            _ => None,
        }
    }

    pub fn to_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<f64>().copied(),
            _ => None,
        }
    }

    pub fn to_char(&self) -> Option<char> {
        match self {
            Value::Char(c) => Some(*c),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<char>().copied(),
            _ => None,
        }
    }

    pub fn to_str(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s.as_str()),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<String>().map(|s| s.as_str()),
            _ => None,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<bool>().copied(),
            _ => None,
        }
    }

    pub fn to_string_value(&self) -> Option<String> {
        match self {
            Value::Str(s) => Some(s.to_string()),
            Value::Object(obj) => (&**obj as &dyn Any).downcast_ref::<String>().cloned(),
            _ => None,
        }
    }

    pub fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.checked_add(*b).ok_or(RuntimeError::Overflow)?)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::Str(a), Value::Str(b)) => {
                let mut result = (**a).clone();
                result.push_str(b);
                Ok(Value::Str(Rc::new(result)))
            }
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Add, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.add(other),
        }
    }

    pub fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.checked_sub(*b).ok_or(RuntimeError::Overflow)?)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Subtract, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.sub(other),
        }
    }

    pub fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.checked_mul(*b).ok_or(RuntimeError::Overflow)?)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Multiply, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.mul(other),
        }
    }

    pub fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.checked_div(*b).ok_or(RuntimeError::Overflow)?)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / *b as f64)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Divide, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.div(other),
        }
    }

    pub fn rem(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.checked_rem(*b).ok_or(RuntimeError::Overflow)?)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Remainder, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.rem(other),
        }
    }

    pub fn negate(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::Int(a) => Ok(Value::Int(a.checked_neg().ok_or(RuntimeError::Overflow)?)),
            Value::Float(a) => Ok(Value::Float(-a)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Negate, format!("rhs: {self:?}")
            ))?.negate(),
        }
    }

    pub fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(a.cmp(b)),
            (Value::Float(a), Value::Float(b)) => Ok(a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)),
            (Value::Int(a), Value::Float(b)) => Ok((*a as f64).partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)),
            (Value::Float(a), Value::Int(b)) => Ok(a.partial_cmp(&(*b as f64)).unwrap_or(std::cmp::Ordering::Equal)),
            (Value::Bool(a), Value::Bool(b)) => Ok(a.cmp(b)),
            (Value::Char(a), Value::Char(b)) => Ok(a.cmp(b)),
            (Value::Str(a), Value::Str(b)) => Ok(a.as_str().cmp(b.as_str())),
            (Value::Null, Value::Null) => Ok(std::cmp::Ordering::Equal),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::Compare, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.compare(other),
        }
    }

    pub fn logic_and(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::LogicAnd, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.logic_and(other),
        }
    }

    pub fn logic_or(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
            _ => self.as_object().ok_or_else(|| RuntimeError::invalid_operation(
                super::object::OperateKind::LogicOr, format!("lhs: {self:?}, rhs: {other:?}")
            ))?.logic_or(other),
        }
    }

    pub fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::Char(a), Value::Char(b)) => Ok(Value::Bool(a == b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Bool(a.as_str() == b.as_str())),
            (Value::Null, Value::Null) => Ok(Value::Bool(true)),
            (Value::Object(a), Value::Object(b)) => a.equal(&Value::Object(b.clone_box())),
            (Value::Object(a), other) => a.equal(other),
            (self_val, Value::Object(b)) => b.equal(self_val),
            _ => Ok(Value::Bool(false)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValueRef(Rc<RefCell<Value>>);

impl ValueRef {
    pub fn new<T: Object + 'static>(object: T) -> Self {
        ValueRef(Rc::new(RefCell::new(into_value(object))))
    }

    pub fn from_value(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }

    pub fn value(&self) -> Ref<Value> {
        self.0.borrow()
    }

    pub fn value_mut(&self) -> RefMut<Value> {
        self.0.borrow_mut()
    }

    pub fn from_ref(r: &Self) -> Self {
        r.clone()
    }

    pub fn as_object(&self) -> Ref<dyn Object> {
        let guard = self.0.borrow();
        Ref::map(guard, |value| {
            value.as_object().expect("expected Object variant")
        })
    }

    pub fn as_object_mut(&self) -> RefMut<dyn Object> {
        let guard = self.0.borrow_mut();
        RefMut::map(guard, |value| {
            value.as_object_mut().expect("expected Object variant")
        })
    }

    pub fn null() -> Self {
        ValueRef(Rc::new(RefCell::new(Value::Null)))
    }

    pub fn immd(immd: isize) -> Self {
        ValueRef(Rc::new(RefCell::new(Value::Int(immd as i64))))
    }

    pub fn take(&self) -> Value {
        self.0.replace(Value::Null)
    }

    pub fn from_constant(constant: &Constant) -> Self {
        match constant {
            Constant::String(s) => ValueRef(Rc::new(RefCell::new(Value::Str(Rc::new(s.to_string()))))),
        }
    }

    pub fn from_primitive(primitive: Primitive) -> Self {
        match primitive {
            Primitive::Null => Self::null(),
            Primitive::Byte(b) => ValueRef(Rc::new(RefCell::new(Value::Int(b as i64)))),
            Primitive::Boolean(b) => ValueRef(Rc::new(RefCell::new(Value::Bool(b)))),
            Primitive::Integer(i) => ValueRef(Rc::new(RefCell::new(Value::Int(i)))),
            Primitive::Float(f) => ValueRef(Rc::new(RefCell::new(Value::Float(f)))),
            Primitive::Char(c) => ValueRef(Rc::new(RefCell::new(Value::Char(c)))),
        }
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        Self::from_value(value)
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = self.value();
        match &*value {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Object(obj) => obj.debug(f),
        }
    }
}