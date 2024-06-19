use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::{self, Debug},
    marker::PhantomData,
    ops::{self, Deref},
    rc::Rc,
};

use crate::vm::{Operate, RuntimeError};

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive {
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    #[default]
    Undefined,
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
    Array(Vec<ValueRef>),
    Object(Box<dyn Object>),
    Function(Box<dyn Function>),
    Iterator(Box<dyn Iterator<Item = ValueRef>>),
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

/// Function trait for external functions.
pub trait Function: Send + 'static {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;
}

pub struct IntoFunction<F, Args> {
    func: F,
    _marker: PhantomData<fn(Args) -> ()>,
}

impl<F, Args> Function for IntoFunction<F, Args>
where
    F: Callable<Args> + Clone,
    Args: 'static,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self.func.call(args)
    }
}

pub trait IntoRet {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError>;
}

impl<T> IntoRet for T
where
    T: Into<Value>,
{
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(Some(self.into()))
    }
}

impl IntoRet for () {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(None)
    }
}

pub trait FromValue<T = Self>: Sized {
    fn from_value(value: &ValueRef) -> Result<T, RuntimeError>;
}

impl FromValue for ValueRef {
    fn from_value(value: &ValueRef) -> Result<Self, RuntimeError> {
        Ok(value.clone())
    }
}

impl FromValue for bool {
    fn from_value(value: &ValueRef) -> Result<bool, RuntimeError> {
        match *value.borrow() {
            Value::Boolean(b) => Ok(b),
            ref v => Err(RuntimeError::invalid_value_type("bool", v)),
        }
    }
}

impl FromValue for u8 {
    fn from_value(value: &ValueRef) -> Result<u8, RuntimeError> {
        match *value.borrow() {
            Value::Byte(b) => Ok(b),
            ref v => Err(RuntimeError::invalid_value_type("byte", v)),
        }
    }
}

impl FromValue for i64 {
    fn from_value(value: &ValueRef) -> Result<i64, RuntimeError> {
        match *value.borrow() {
            Value::Integer(i) => Ok(i),
            ref v => Err(RuntimeError::invalid_value_type("integer", v)),
        }
    }
}

impl FromValue for f64 {
    fn from_value(value: &ValueRef) -> Result<f64, RuntimeError> {
        match *value.borrow() {
            Value::Float(f) => Ok(f),
            ref v => Err(RuntimeError::invalid_value_type("float", v)),
        }
    }
}

impl FromValue for char {
    fn from_value(value: &ValueRef) -> Result<char, RuntimeError> {
        match *value.borrow() {
            Value::Char(c) => Ok(c),
            ref v => Err(RuntimeError::invalid_value_type("char", v)),
        }
    }
}

pub trait Callable<Args>: Clone + Send + Sized + 'static {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;

    fn into_function(self) -> IntoFunction<Self, Args> {
        IntoFunction {
            func: self,
            _marker: PhantomData,
        }
    }
}

impl<F, Ret> Callable<&[ValueRef]> for F
where
    F: Fn(&[ValueRef]) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self)(args).into_ret()
    }
}

impl<F, Ret> Callable<()> for F
where
    F: Fn() -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self().into_ret()
    }
}

// impl<F, T0, Ret> Callable<T0> for F
// where
//     F: Fn(T0) -> Ret + Clone + Send + 'static,
//     T0: FromValue,
//     Ret: IntoRet,
// {
//     fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
//         let arg0 = T0::from_value(&args[0])?;
//         (self)(arg0).into_ret()
//     }
// }

// impl<F, T0, T1, Ret> Callable<(T0, T1)> for F
// where
//     F: Fn(T0, T1) -> Ret + Clone + Send + 'static,
//     T0: FromValue,
//     T1: FromValue,
//     Ret: IntoRet,
// {
//     fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
//         let arg0 = T0::from_value(&args[0])?;
//         let arg1 = T1::from_value(&args[1])?;
//         (self)(arg0, arg1).into_ret()
//     }
// }

// macro_rules! impl_callable_tuple {
//     ($($arg: ident),+) => {
//         impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
//         where
//             F: Fn($($arg,)*) -> Ret + Clone + Send + 'static,
//             Ret: IntoRet,
//             $( $arg: FromValue, )*
//         {
//             fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
//                 $(
//                     let $arg = <$arg>::from_value(&args[${index()}])?;
//                 )*
//                 (self)($($arg,)*).into_ret()
//             }
//         }
//     }
// }

macro_rules! impl_callable {
    ($($idx: expr => $arg: ident),+) => {
        #[allow(non_snake_case)]
        impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
        where
            F: Fn($($arg,)*) -> Ret + Clone + Send + 'static,
            Ret: IntoRet,
            $( $arg: FromValue, )*
        {
            fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
                $(
                    let $arg = <$arg>::from_value(&args[$idx])?;
                )*
                (self)($($arg,)*).into_ret()
            }
        }
    }
}

impl_callable!(0=>T0);
impl_callable!(0=>T0, 1=>T1);
impl_callable!(0=>T0, 1=>T1, 2=>T2);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13, 14=>T14);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13, 14=>T14, 15=>T15);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13, 14=>T14, 15=>T15, 16=>T16);

/* use this when [feature(macro_metavar_expr)](https://github.com/rust-lang/rust/pull/122808) is available
macro_rules! impl_callable_tuple {
    ($($arg: ident),+) => {
        #[allow(non_snake_case)]
        impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
        where
            F: Fn($($arg,)*) -> Ret + Clone + Send + 'static,
            Ret: IntoRet,
            $( $arg: FromValue, )*
        {
            fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
                $(
                    let $arg = <$arg>::from_value(&args[${index()}])?;
                )*
                (self)($($arg,)*).into_ret()
            }
        }
    }
}
impl_callable_tuple!(T0);
impl_callable_tuple!(T0, T1);
impl_callable_tuple!(T0, T1, T2);
impl_callable_tuple!(T0, T1, T2, T3);
impl_callable_tuple!(T0, T1, T2, T3, T4);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);
 */

/// Dynamic Object trait for external objects.
pub trait Object: Debug {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("Object is not callable"))
    }
    fn property(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
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
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operands for addition",
            )),
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

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, crate::vm::RuntimeError> {
        match self {
            Value::Object(ref mut o) => o.call(args),
            Value::Function(ref mut f) => f.call(args),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for function call",
            )),
        }
    }

    fn call_member(
        &mut self,
        member: &str,
        this: ValueRef,
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
            Value::Array(a) => Ok(Some(Box::new(a.clone().into_iter()))),
            Value::Range(r) => Ok(Some(Box::new(RangeIterator::new(r)?))),
            _ => Err(crate::vm::RuntimeError::invalid_operation(
                "Invalid operand for iterator call",
            )),
        }
    }
}

/// ValueRef is a reference to a value.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ValueRef(Rc<RefCell<Value>>);

impl ValueRef {
    pub fn new(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }

    pub fn borrow(&self) -> Ref<Value> {
        self.0.as_ref().borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, Value> {
        self.0.as_ref().borrow_mut()
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
pub struct Range {
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
