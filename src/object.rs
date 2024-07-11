use core::fmt;
use std::any::Any;

use crate::error::RuntimeError;
use crate::value::{Value, ValueRef};

#[derive(Debug, Clone, Copy)]
pub enum OperateKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Compare,
    LogicAnd,
    LogicOr,
    Negate,
    Call,
    IndexGet,
    IndexSet,
    PropertyGet,
    PropertySet,
    PropertyCall,
    MakeIterator,
    IterateNext,
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
            OperateKind::Modulo => write!(f, "modulo"),
            OperateKind::Power => write!(f, "power"),
            OperateKind::Compare => write!(f, "compare"),
            OperateKind::LogicAnd => write!(f, "logic_and"),
            OperateKind::LogicOr => write!(f, "logic_or"),
            OperateKind::Negate => write!(f, "negate"),
            OperateKind::Call => write!(f, "call"),
            OperateKind::IndexGet => write!(f, "index_get"),
            OperateKind::IndexSet => write!(f, "index_set"),
            OperateKind::PropertyGet => write!(f, "property_get"),
            OperateKind::PropertySet => write!(f, "property_set"),
            OperateKind::PropertyCall => write!(f, "property_call"),
            OperateKind::MakeIterator => write!(f, "make_iterator"),
            OperateKind::IterateNext => write!(f, "iterate_next"),
            OperateKind::Display => write!(f, "display"),
            OperateKind::TypeCast => write!(f, "type_cast"),
        }
    }
}

#[derive(Debug)]
pub struct OperateError {
    kind: OperateKind,
    cause: Option<Box<dyn std::error::Error + Send + Sync + 'static>>,
}

impl OperateError {
    pub fn new(
        kind: OperateKind,
        cause: impl Into<Option<Box<dyn std::error::Error + Send + Sync + 'static>>>,
    ) -> OperateError {
        OperateError {
            kind,
            cause: cause.into(),
        }
    }

    pub fn with_cause(
        mut self,
        cause: Box<dyn std::error::Error + Send + Sync + 'static>,
    ) -> OperateError {
        self.cause = Some(cause);
        self
    }

    pub fn invalid_addition(
        cause: Box<dyn std::error::Error + Send + Sync + 'static>,
    ) -> OperateError {
        OperateError::new(OperateKind::Add, cause)
    }
}

impl fmt::Display for OperateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invailid {} operate", self.kind)?;

        if let Some(cause) = &self.cause {
            write!(f, ", cause: {}", cause)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct BoxedObject(Box<dyn Object>);

impl BoxedObject {
    pub fn new(value: Box<dyn Object>) -> BoxedObject {
        BoxedObject(value)
    }

    pub fn as_ref(&self) -> &dyn Object {
        self.0.as_ref()
    }

    pub fn as_mut(&mut self) -> &mut dyn Object {
        self.0.as_mut()
    }

    fn as_any(&self) -> &dyn Any {
        &self.0
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        &mut self.0
    }

    pub fn downcast_ref<T: Any>(&self) -> Result<&T, RuntimeError> {
        self.as_any()
            .downcast_ref::<T>()
            .ok_or(RuntimeError::invalid_type::<T>(self))
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Result<&mut T, RuntimeError> {
        let value = self.as_any_mut();
        if value.is::<T>() {
            Ok(value.downcast_mut::<T>().unwrap())
        } else {
            Err(RuntimeError::invalid_type::<T>(value))
        }
    }
}

pub trait Object: std::any::Any + std::fmt::Debug {
    fn display(&self) -> String {
        format!("{:?}", self)
    }

    /// arithmetic addition operation
    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Add,
            "unimplemented",
        ))
    }

    /// arithmetic subtraction operation
    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Subtract,
            "unimplemented",
        ))
    }

    /// arithmetic multiplication operation
    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Multiply,
            "unimplemented",
        ))
    }

    /// arithmetic division operation
    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Divide,
            "unimplemented",
        ))
    }

    /// arithmetic modulo operation
    fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Modulo,
            "unimplemented",
        ))
    }

    /// arithmetic power operation
    fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Power,
            "unimplemented",
        ))
    }

    /// compare operation
    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Compare,
            "unimplemented",
        ))
    }

    /// logic and operation
    fn logic_and(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicAnd,
            "unimplemented",
        ))
    }

    /// logic or operation
    fn logic_or(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicOr,
            "unimplemented",
        ))
    }

    /// negate operation
    fn negate(&self) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Negate,
            "unimplemented",
        ))
    }

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Call,
            "unimplemented",
        ))
    }

    /// index get operation
    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexGet,
            "unimplemented",
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexSet,
            "unimplemented",
        ))
    }

    fn property_get(&mut self, member: &str) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertyGet,
            "unimplemented",
        ))
    }

    fn property_set(&mut self, member: &str, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertySet,
            "unimplemented",
        ))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertyCall,
            "unimplemented",
        ))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::MakeIterator,
            "unimplemented",
        ))
    }

    fn iterate_next(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IterateNext,
            "unimplemented",
        ))
    }
}

// impl Object for bool {
//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.downcast_ref::<bool>()?;
//         match (self, other) {
//             (true, true) => Ok(std::cmp::Ordering::Equal),
//             (true, false) => Ok(std::cmp::Ordering::Greater),
//             (false, true) => Ok(std::cmp::Ordering::Less),
//             (false, false) => Ok(std::cmp::Ordering::Equal),
//         }
//     }
// }

// impl Object for i64 {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(*self + *other)))
//     }

//     fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(*self - *other)))
//     }

//     fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(*self * *other)))
//     }

//     fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(*self / *other)))
//     }

//     fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(*self % *other)))
//     }

//     fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         Ok(Value::object(Box::new(i64::pow(*self, *other as u32))))
//     }

//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.downcast_ref::<i64>()?;
//         match self.cmp(other) {
//             std::cmp::Ordering::Less => Ok(std::cmp::Ordering::Less),
//             std::cmp::Ordering::Equal => Ok(std::cmp::Ordering::Equal),
//             std::cmp::Ordering::Greater => Ok(std::cmp::Ordering::Greater),
//         }
//     }

//     fn property_get(&mut self, member: &str) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "max" => Ok(Some(Value::object(Box::new(i64::MAX)))),
//             "min" => Ok(Some(Value::object(Box::new(i64::MIN)))),
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyGet,
//                 format!("{} is not a member of {}", member, type_name::<i64>()),
//             )),
//         }
//     }
// }

// impl Object for f64 {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(*self + *other)))
//     }

//     fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(*self - *other)))
//     }

//     fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(*self * *other)))
//     }

//     fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(*self / *other)))
//     }

//     fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(*self % *other)))
//     }

//     fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         Ok(Value::object(Box::new(f64::powf(*self, *other))))
//     }

//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.downcast_ref::<f64>()?;
//         match self.partial_cmp(other) {
//             Some(std::cmp::Ordering::Less) => Ok(std::cmp::Ordering::Less),
//             Some(std::cmp::Ordering::Equal) => Ok(std::cmp::Ordering::Equal),
//             Some(std::cmp::Ordering::Greater) => Ok(std::cmp::Ordering::Greater),
//             None => Err(RuntimeError::invalid_operation(
//                 OperateKind::Compare,
//                 "unimplemented",
//             )),
//         }
//     }

//     fn property_get(&mut self, member: &str) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "max" => Ok(Some(Value::object(Box::new(f64::MAX)))),
//             "min" => Ok(Some(Value::object(Box::new(f64::MIN)))),
//             "nan" => Ok(Some(Value::object(Box::new(f64::NAN)))),
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyGet,
//                 format!("{} is not a member of {}", member, type_name::<f64>()),
//             )),
//         }
//     }
// }

// impl Object for char {
//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.downcast_ref::<char>()?;
//         match self.cmp(other) {
//             std::cmp::Ordering::Less => Ok(std::cmp::Ordering::Less),
//             std::cmp::Ordering::Equal => Ok(std::cmp::Ordering::Equal),
//             std::cmp::Ordering::Greater => Ok(std::cmp::Ordering::Greater),
//         }
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<Value>, RuntimeError> {
//         if !args.is_empty() {
//             return Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyCall,
//                 format!("{} does not take any arguments", member),
//             ));
//         }

//         match member {
//             "is_ascii" => Ok(Some(Value::object(Box::new(self.is_ascii())))),
//             "is_alphanumeric" => Ok(Some(Value::object(Box::new(self.is_alphanumeric())))),
//             "is_ascii_alphanumeric" => Ok(Some(Value::object(Box::new(self.is_ascii_alphanumeric())))),
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyGet,
//                 format!("{} is not a member of {}", member, type_name::<char>()),
//             )),
//         }
//     }
// }

// impl Object for String {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.downcast_ref::<String>()?;
//         Ok(Value::object(Box::new(self.clone() + other)))
//     }

//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.downcast_ref::<String>()?;
//         match self.cmp(other) {
//             std::cmp::Ordering::Less => Ok(std::cmp::Ordering::Less),
//             std::cmp::Ordering::Equal => Ok(std::cmp::Ordering::Equal),
//             std::cmp::Ordering::Greater => Ok(std::cmp::Ordering::Greater),
//         }
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "to_lowercase" => {
//                 if !args.is_empty() {
//                     return Err(RuntimeError::invalid_operation(
//                         OperateKind::PropertyCall,
//                         format!("{} does not take any arguments", member),
//                     ));
//                 }
//                 Ok(Some(Value::object(Box::new(self.to_lowercase()))))
//             }
//             "to_uppercase" => {
//                 if !args.is_empty() {
//                     return Err(RuntimeError::invalid_operation(
//                         OperateKind::PropertyCall,
//                         format!("{} does not take any arguments", member),
//                     ));
//                 }
//                 Ok(Some(Value::object(Box::new(self.to_uppercase()))))
//             }
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyCall,
//                 format!("{} is not a member of {}", member, type_name::<String>()),
//             )),
//         }
//     }
// }
