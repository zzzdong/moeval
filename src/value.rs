use std::{
    any::TypeId,
    cell::{Ref, RefCell, RefMut},
    fmt::{self, Debug},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{error::RuntimeError, ir::types::Primitive, object::Object, types::*};

#[derive(Debug)]
pub struct Value(Box<dyn Object>);

impl Value {
    pub fn new(object: impl Object) -> Value {
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
        // println!(
        //     "type of value: {}, {:?}",
        //     std::any::type_name::<T>(),
        //     TypeId::of::<T>()
        // );

        // println!(
        //     "type of self.0: {},{:?}",
        //     std::any::type_name_of_val(&*self.0),
        //     (&*self.0).type_id()
        // );

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

impl<T: Object> From<T> for Value {
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

#[derive(Debug, Clone)]
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

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.borrow().0.display().to_string())
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        ValueRef::new(value)
    }
}
