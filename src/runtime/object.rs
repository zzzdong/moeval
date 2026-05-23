mod array;
mod boolean;
mod emurator;
mod float;
mod function;
mod immd;
mod integer;
mod map;
mod metatable;
mod null;
mod optional;
mod range;
mod string;
mod structobject;
mod tuple;

pub use emurator::Enumerator;
pub use function::{Callable, NativeFunction, UserFunction};
pub use immd::Immd;
pub use null::Null;
pub use range::Range;
pub use structobject::StructObject;

use super::{RuntimeError, Value, ValueRef};

use std::{any::type_name_of_val, fmt};

pub trait Object: std::any::Any + std::fmt::Debug {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{self:?}"))
    }

    fn clone_box(&self) -> Box<dyn Object> {
        panic!("Clone not implemented for {}", std::any::type_name_of_val(self))
    }

    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Add,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Subtract,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Multiply,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Divide,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn rem(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Remainder,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Equal,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Compare,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn logic_and(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicAnd,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn logic_or(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicOr,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Negate,
            format!("rhs: {self:?}"),
        ))
    }

    fn call(&mut self, _args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Call,
            "unimplemented",
        ))
    }

    fn index_get(&self, _index: &Value) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexGet,
            "unimplemented",
        ))
    }

    fn index_set(&mut self, _index: &Value, _value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexSet,
            "unimplemented",
        ))
    }

    fn property_get(&self, _property: &str) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertyGet,
            "unimplemented",
        ))
    }

    fn property_set(&mut self, _property: &str, _value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertySet,
            "unimplemented",
        ))
    }

    fn call_method(
        &mut self,
        method: &str,
        _args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        Err(RuntimeError::MissingMethod {
            object: type_name_of_val(self).to_string(),
            method: method.to_string(),
        })
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::MakeIterator,
            "unimplemented",
        ))
    }

    fn make_slice(&self, _range: ValueRef) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::MakeSlice,
            "unimplemented",
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperateKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    Compare,
    LogicAnd,
    LogicOr,
    Negate,
    Call,
    Range,
    IndexGet,
    IndexSet,
    PropertyGet,
    PropertySet,
    PropertyCall,
    MakeIterator,
    IterateNext,
    MakeSlice,
    Display,
    TypeCast,
}

impl fmt::Display for OperateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OperateKind::Add => write!(f, "add"),
            OperateKind::Subtract => write!(f, "subtract"),
            OperateKind::Multiply => write!(f, "multiply"),
            OperateKind::Divide => write!(f, "divide"),
            OperateKind::Remainder => write!(f, "reminder"),
            OperateKind::Equal => write!(f, "equal"),
            OperateKind::Compare => write!(f, "compare"),
            OperateKind::LogicAnd => write!(f, "logic_and"),
            OperateKind::LogicOr => write!(f, "logic_or"),
            OperateKind::Negate => write!(f, "negate"),
            OperateKind::Call => write!(f, "call"),
            OperateKind::Range => write!(f, "range"),
            OperateKind::IndexGet => write!(f, "index_get"),
            OperateKind::IndexSet => write!(f, "index_set"),
            OperateKind::PropertyGet => write!(f, "property_get"),
            OperateKind::PropertySet => write!(f, "property_set"),
            OperateKind::PropertyCall => write!(f, "property_call"),
            OperateKind::MakeIterator => write!(f, "make_iterator"),
            OperateKind::IterateNext => write!(f, "iterate_next"),
            OperateKind::MakeSlice => write!(f, "make_slice"),
            OperateKind::Display => write!(f, "display"),
            OperateKind::TypeCast => write!(f, "type_cast"),
        }
    }
}