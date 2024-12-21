use std::{
    any::TypeId,
    fmt::{self, Debug},
    ops::{Deref, DerefMut},
    sync::Arc,
};

use bevy_utils::syncunsafecell::SyncUnsafeCell;

use super::object::{Object, Undefined};
use super::RuntimeError;
use crate::ir::Primitive;

#[derive(Debug)]
pub struct Value(Box<dyn Object + Sync + Send>);

impl Value {
    pub fn new(object: impl Object + Sync + Send) -> Value {
        Value(Box::new(object))
    }

    pub fn downcast_ref<T: Object>(&self) -> Option<&T> {
        if TypeId::of::<T>() == (*self.0).type_id() {
            return unsafe { Some(&*(&*self.0 as *const dyn Object as *const T)) };
        }

        None
    }
    pub fn downcast_mut<T: Object>(&mut self) -> Option<&mut T> {
        if TypeId::of::<T>() == (*self.0).type_id() {
            return unsafe { Some(&mut *(&mut *self.0 as *mut dyn Object as *mut T)) };
        }

        None
    }

    pub fn try_downcast_ref<T: 'static>(&self) -> Result<&T, RuntimeError> {
        if TypeId::of::<T>() == (*self.0).type_id() {
            return unsafe { Ok(&*(&*self.0 as *const dyn Object as *const T)) };
        }

        Err(RuntimeError::invalid_type::<T>(&self.0))
    }

    pub fn try_downcast_mut<T: Object>(&mut self) -> Result<&mut T, RuntimeError> {
        if TypeId::of::<T>() == (*self.0).type_id() {
            return unsafe { Ok(&mut *(&mut *self.0 as *mut dyn Object as *mut T)) };
        }

        Err(RuntimeError::invalid_type::<T>(&self.0))
    }

    pub fn try_cast<T: Object>(mut self) -> Result<Box<T>, RuntimeError> {
        if TypeId::of::<T>() == (*self.0).type_id() {
            return unsafe {
                Ok(Box::from_raw(
                    &mut *(&mut *self.0 as *mut dyn Object as *mut T),
                ))
            };
        }

        Err(RuntimeError::invalid_type::<T>(&self.0))
    }

    // FIXME: stupid
    pub(crate) fn from_primitive(value: Primitive) -> Value {
        match value {
            Primitive::Undefined => Value::new(Undefined),
            Primitive::Boolean(value) => Value::new(value),
            Primitive::Integer(value) => Value::new(value),
            Primitive::Float(value) => Value::new(value),
            Primitive::String(value) => Value::new(value),
            _ => unimplemented!(),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::new(Undefined)
    }
}

impl AsRef<dyn Object> for Value {
    fn as_ref(&self) -> &dyn Object {
        self.0.as_ref()
    }
}

impl<T: Object + Sync + Send> From<T> for Value {
    fn from(value: T) -> Self {
        Value::new(value)
    }
}

impl From<Primitive> for Value {
    fn from(value: Primitive) -> Self {
        match value {
            Primitive::Undefined => Value::new(Undefined),
            Primitive::Boolean(value) => Value::new(value),
            Primitive::Integer(value) => Value::new(value),
            Primitive::Float(value) => Value::new(value),
            Primitive::String(value) => Value::new(value),
            _ => unimplemented!(),
        }
    }
}

impl Deref for Value {
    type Target = dyn Object;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl DerefMut for Value {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}

impl PartialEq<Undefined> for Value {
    fn eq(&self, other: &Undefined) -> bool {
        match self.downcast_ref::<Undefined>() {
            Some(b) => *b == *other,
            None => false,
        }
    }
}

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self.downcast_ref::<bool>() {
            Some(b) => *b == *other,
            None => false,
        }
    }
}

impl PartialEq<i64> for Value {
    fn eq(&self, other: &i64) -> bool {
        match self.downcast_ref::<i64>() {
            Some(i) => i == other,
            None => false,
        }
    }
}

impl PartialEq<f64> for Value {
    fn eq(&self, other: &f64) -> bool {
        match self.downcast_ref::<f64>() {
            Some(f) => f == other,
            None => false,
        }
    }
}

impl PartialEq<String> for Value {
    fn eq(&self, other: &String) -> bool {
        match self.downcast_ref::<String>() {
            Some(s) => s == other.as_str(),
            None => false,
        }
    }
}

#[derive(Clone)]
pub struct ValueRef(Arc<SyncUnsafeCell<Value>>);

impl ValueRef {
    pub fn new(value: Value) -> Self {
        ValueRef(Arc::new(SyncUnsafeCell::new(value)))
    }

    pub fn value(&self) -> &Value {
        unsafe { &*self.0.get() }
    }

    pub fn get(&self) -> &Value {
        unsafe { &*self.0.get() }
    }

    pub fn get_mut(&mut self) -> &mut Value {
        unsafe { &mut *self.0.get() }
    }
}

impl Default for ValueRef {
    fn default() -> Self {
        ValueRef::new(Value::default())
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.get().debug(f)
    }
}

impl fmt::Debug for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.get().debug(f)
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        ValueRef::new(value)
    }
}

impl PartialEq<Undefined> for ValueRef {
    fn eq(&self, other: &Undefined) -> bool {
        match self.get().downcast_ref::<Undefined>() {
            Some(b) => *b == *other,
            None => false,
        }
    }
}

impl PartialEq<bool> for ValueRef {
    fn eq(&self, other: &bool) -> bool {
        match self.get().downcast_ref::<bool>() {
            Some(b) => *b == *other,
            None => false,
        }
    }
}

impl PartialEq<i64> for ValueRef {
    fn eq(&self, other: &i64) -> bool {
        match self.get().downcast_ref::<i64>() {
            Some(i) => i == other,
            None => false,
        }
    }
}

impl PartialEq<f64> for ValueRef {
    fn eq(&self, other: &f64) -> bool {
        match self.get().downcast_ref::<f64>() {
            Some(f) => f == other,
            None => false,
        }
    }
}

impl PartialEq<String> for ValueRef {
    fn eq(&self, other: &String) -> bool {
        match self.get().downcast_ref::<String>() {
            Some(s) => s == other.as_str(),
            None => false,
        }
    }
}

impl PartialEq<str> for ValueRef {
    fn eq(&self, other: &str) -> bool {
        match self.get().downcast_ref::<String>() {
            Some(s) => s == other,
            None => false,
        }
    }
}
impl PartialEq<&str> for ValueRef {
    fn eq(&self, other: &&str) -> bool {
        match self.get().downcast_ref::<String>() {
            Some(s) => s == other,
            None => false,
        }
    }
}

impl PartialEq<Value> for ValueRef {
    fn eq(&self, other: &Value) -> bool {
        matches!(
            self.get().as_ref().compare(other),
            Ok(std::cmp::Ordering::Equal)
        )
    }
}
