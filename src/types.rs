use std::iter::Iterator;
use std::marker::PhantomData;
use std::{any::type_name, fmt};

use indexmap::IndexMap;

use crate::ir::types::Primitive;
use crate::{
    error::RuntimeError,
    ir::FunctionId,
    object::{Object, OperateKind},
    value::{Value, ValueRef},
};

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Undefined;

impl Object for Undefined {
    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        let other = other.try_downcast_ref::<Undefined>()?;
        self.partial_cmp(other).ok_or_else(|| {
            RuntimeError::invalid_operation(OperateKind::Compare, "can not compare undefined")
        })
    }
}

impl Object for bool {
    fn negate(&self) -> Result<Value, RuntimeError> {
        Ok(Value::new(!self))
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        let other = other.try_downcast_ref::<bool>()?;
        match (self, other) {
            (true, true) => Ok(std::cmp::Ordering::Equal),
            (true, false) => Ok(std::cmp::Ordering::Greater),
            (false, true) => Ok(std::cmp::Ordering::Less),
            (false, false) => Ok(std::cmp::Ordering::Equal),
        }
    }
}

// #[derive(Debug)]
// pub struct Boolean(bool);

// impl Boolean {
//     pub fn new(value: bool) -> Self {
//         Boolean(value)
//     }

//     pub fn value(&self) -> bool {
//         self.0
//     }
// }

// impl Object for Boolean {
//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.try_downcast_ref::<Boolean>()?;
//         match (self.0, other.0) {
//             (true, true) => Ok(std::cmp::Ordering::Equal),
//             (true, false) => Ok(std::cmp::Ordering::Greater),
//             (false, true) => Ok(std::cmp::Ordering::Less),
//             (false, false) => Ok(std::cmp::Ordering::Equal),
//         }
//     }
// }

impl Object for i64 {
    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = other.try_downcast_ref::<i64>()?;
        match self.checked_add(*other) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = other.try_downcast_ref::<i64>()?;
        match self.checked_sub(*other) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = other.try_downcast_ref::<i64>()?;
        match self.checked_mul(*other) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = other.try_downcast_ref::<i64>()?;
        match self.checked_rem(*other) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = *other.try_downcast_ref::<i64>()?;
        match self.checked_div(other) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = *other.try_downcast_ref::<i64>()?;
        if other >= u32::MAX as i64 {
            return Err(RuntimeError::invalid_operation(
                OperateKind::Power,
                "exp >= u32::MAX",
            ));
        }
        match self.checked_pow(other as u32) {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        match self.checked_neg() {
            Some(result) => Ok(Value::new(result)),
            None => Err(RuntimeError::Overflow),
        }
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        let other = other.try_downcast_ref::<i64>()?;

        self.partial_cmp(other)
            .ok_or(RuntimeError::invalid_operation(
                OperateKind::Compare,
                "uncomparable",
            ))
    }

    fn property_get(&mut self, member: &str) -> Result<Value, RuntimeError> {
        match member {
            "max" => Ok(Value::new(i64::MAX)),
            "min" => Ok(Value::new(i64::MIN)),
            _ => Err(RuntimeError::InvalidOperation {
                kind: OperateKind::PropertyGet,
                message: format!("{} is not a member of Integer", member),
            }),
        }
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "abs" => {
                if !args.is_empty() {
                    Err(RuntimeError::InvalidOperation {
                        kind: OperateKind::PropertyCall,
                        message: format!("{} takes no arguments", member),
                    })
                } else {
                    Ok(Some(Value::new(self.abs())))
                }
            }
            "to_string" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_operation(
                        OperateKind::PropertyCall,
                        format!("{} takes no arguments", member),
                    ));
                }
                Ok(Some(Value::new(self.to_string())))
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!("{} is not a callable property", member),
            )),
        }
    }
}

// #[derive(Debug)]
// pub struct Integer(i64);

// impl Integer {
//     pub fn new(value: i64) -> Self {
//         Integer(value)
//     }

//     pub fn value(&self) -> i64 {
//         self.0
//     }
// }

// impl Object for Integer {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         match self.0.checked_add(other.0) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         match self.0.checked_sub(other.0) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         match self.0.checked_mul(other.0) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         match self.0.checked_rem(other.0) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         match self.0.checked_div(other.0) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;
//         if other.0 >= u32::MAX as i64 {
//             return Err(RuntimeError::invalid_operation(
//                 OperateKind::Power,
//                 "exp >= u32::MAX",
//             ));
//         }
//         match self.0.checked_pow(other.0 as u32) {
//             Some(result) => Ok(Value::new(result)),
//             None => Err(RuntimeError::Overflow),
//         }
//     }

//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         let other = other.try_downcast_ref::<Integer>()?;

//         self.0
//             .partial_cmp(&other.0)
//             .ok_or(RuntimeError::invalid_operation(
//                 OperateKind::Compare,
//                 "uncomparable",
//             ))
//     }

//     fn property_get(&mut self, member: &str) -> Result<Value, RuntimeError> {
//         match member {
//             "max" => Ok(Value::new(Integer(i64::MAX))),
//             "min" => Ok(Value::new(Integer(i64::MIN))),
//             _ => Err(RuntimeError::InvalidOperation {
//                 kind: OperateKind::PropertyGet,
//                 message: format!("{} is not a member of Integer", member),
//             }),
//         }
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "abs" => {
//                 if args.len() != 0 {
//                     Err(RuntimeError::InvalidOperation {
//                         kind: OperateKind::PropertyCall,
//                         message: format!("{} takes no arguments", member),
//                     })
//                 } else {
//                     Ok(Some(Value::new(Integer(self.0.abs()))))
//                 }
//             }
//             "to_string" => {
//                 if args.len() != 0 {
//                     return Err(RuntimeError::invalid_operation(
//                         OperateKind::PropertyCall,
//                         format!("{} takes no arguments", member),
//                     ));
//                 }
//                 Ok(Some(Value::new(self.0.to_string())))
//             }
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyCall,
//                 format!("{} is not a callable property", member),
//             )),
//         }
//     }
// }

impl Object for f64 {
    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self + other));
        } else if let Some(other) = other.downcast_ref::<i64>() {
            return Ok(Value::new(self + *other as f64));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self - other));
        } else if let Some(other) = other.downcast_ref::<i64>() {
            return Ok(Value::new(self - *other as f64));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self * *other));
        } else if let Some(other) = other.downcast_ref::<i64>() {
            return Ok(Value::new(self * (*other as f64)));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self / other));
        } else if let Some(other) = other.downcast_ref::<i64>() {
            return Ok(Value::new(self / (*other as f64)));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        Ok(Value::new(-self))
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            self.partial_cmp(other)
                .ok_or(RuntimeError::invalid_operation(
                    OperateKind::Compare,
                    "uncomparable",
                ))
        } else if let Some(other) = other.downcast_ref::<i64>() {
            self.partial_cmp(&(*other as f64))
                .ok_or(RuntimeError::invalid_operation(
                    OperateKind::Compare,
                    "uncomparable",
                ))
        } else {
            Err(RuntimeError::invalid_type::<f64>(self))
        }
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "to_string" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }
                Ok(Some(Value::new(self.to_string())))
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!("{} is not a callable property", member),
            )),
        }
    }
}

// #[derive(Debug)]
// pub struct Float(f64);

// impl Float {
//     pub fn new(value: f64) -> Self {
//         Float(value)
//     }

//     pub fn value(&self) -> f64 {
//         self.0
//     }
// }

// impl Object for Float {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         if let Some(other) = other.downcast_ref::<Float>() {
//             return Ok(Value::new(Float(self.0 + other.0)));
//         } else if let Some(other) = other.downcast_ref::<Integer>() {
//             return Ok(Value::new(Float(self.0 + other.0 as f64)));
//         }

//         Err(RuntimeError::invalid_type::<Float>(other))
//     }

//     fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
//         if let Some(other) = other.downcast_ref::<Float>() {
//             return Ok(Value::new(Float(self.0 - other.0)));
//         } else if let Some(other) = other.downcast_ref::<Integer>() {
//             return Ok(Value::new(Float(self.0 - other.0 as f64)));
//         }

//         Err(RuntimeError::invalid_type::<Float>(other))
//     }

//     fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
//         if let Some(other) = other.downcast_ref::<Float>() {
//             return Ok(Value::new(Float(self.0 * other.0)));
//         } else if let Some(other) = other.downcast_ref::<Integer>() {
//             return Ok(Value::new(Float(self.0 * other.0 as f64)));
//         }

//         Err(RuntimeError::invalid_type::<Float>(other))
//     }

//     fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
//         if let Some(other) = other.downcast_ref::<Float>() {
//             return Ok(Value::new(Float(self.0 / other.0)));
//         } else if let Some(other) = other.downcast_ref::<Integer>() {
//             return Ok(Value::new(Float(self.0 / other.0 as f64)));
//         }

//         Err(RuntimeError::invalid_type::<Float>(other))
//     }

//     fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
//         if let Some(other) = other.downcast_ref::<Float>() {
//             self.0
//                 .partial_cmp(&other.0)
//                 .ok_or(RuntimeError::invalid_operation(
//                     OperateKind::Compare,
//                     "uncomparable",
//                 ))
//         } else if let Some(other) = other.downcast_ref::<Integer>() {
//             self.0
//                 .partial_cmp(&(other.0 as f64))
//                 .ok_or(RuntimeError::invalid_operation(
//                     OperateKind::Compare,
//                     "uncomparable",
//                 ))
//         } else {
//             Err(RuntimeError::invalid_type::<Float>(self))
//         }
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "to_string" => {
//                 if !args.is_empty() {
//                     return Err(RuntimeError::invalid_argument_count(0, args.len()));
//                 }
//                 Ok(Some(Value::new(self.0.to_string())))
//             }
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyCall,
//                 format!("{} is not a callable property", member),
//             )),
//         }
//     }
// }

// #[derive(Debug)]
// pub struct EString(String);

// impl EString {
//     pub fn new(s: impl Into<String>) -> Self {
//         EString(s.into())
//     }

//     pub fn value(&self) -> &str {
//         &self.0
//     }
// }

// impl Object for EString {
//     fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
//         let other = other.try_downcast_ref::<EString>()?;

//         Ok(Value::new(EString(self.0.clone() + &other.0)))
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<Value>, RuntimeError> {
//         match member {
//             "to_string" => {
//                 if args.len() != 0 {
//                     return Err(RuntimeError::invalid_argument_count(0, args.len()));
//                 }

//                 Ok(Some(Value::new(EString(self.0.clone()))))
//             }
//             "len" => {
//                 if args.len() != 0 {
//                     return Err(RuntimeError::invalid_argument_count(0, args.len()));
//                 }

//                 Ok(Some(Value::new(Integer(self.0.chars().count() as i64))))
//             }

//             "split" => {
//                 if args.len() != 1 {
//                     return Err(RuntimeError::invalid_argument_count(1, args.len()));
//                 }

//                 let pat = args.get(0).unwrap().borrow();

//                 let pat = pat.try_downcast_ref::<EString>()?;

//                 if pat.0.is_empty() {
//                     return Err(RuntimeError::invalid_argument::<EString>(
//                         0,
//                         "pat must not empty",
//                     ));
//                 }

//                 let parts: Vec<ValueRef> = self
//                     .0
//                     .split(&pat.0)
//                     .map(|s| ValueRef::new(Value::new(EString::new(s))))
//                     .collect();

//                 Ok(Some(Value::new(Array::new(parts))))
//             }
//             "find" => {
//                 if args.len() != 1 {
//                     return Err(RuntimeError::invalid_argument_count(1, args.len()));
//                 }
//                 let pat = args.get(0).unwrap().borrow();

//                 let pat = pat.try_downcast_ref::<EString>()?;

//                 match self.0.find(&pat.0) {
//                     Some(index) => {
//                         let sub = &self.0.as_str()[0..index];

//                         Ok(Some(Value::new(Integer(sub.chars().count() as i64))))
//                     }
//                     None => Ok(Some(Value::new(Integer(-1)))),
//                 }
//             }

//             "starts_with" => {
//                 if args.len() != 1 {
//                     return Err(RuntimeError::invalid_argument_count(1, args.len()));
//                 }
//                 let pat = args.get(0).unwrap().borrow();

//                 let pat = pat.try_downcast_ref::<EString>()?;

//                 Ok(Some(Value::new(Boolean::new(self.0.starts_with(&pat.0)))))
//             }
//             "ends_with" => {
//                 if args.len() != 1 {
//                     return Err(RuntimeError::invalid_argument_count(1, args.len()));
//                 }
//                 let pat = args.get(0).unwrap().borrow();

//                 let pat = pat.try_downcast_ref::<EString>()?;

//                 Ok(Some(Value::new(Boolean::new(self.0.ends_with(&pat.0)))))
//             }
//             _ => Err(RuntimeError::invalid_operation(
//                 OperateKind::PropertyCall,
//                 format!("{} is not a callable property", member),
//             )),
//         }
//     }
// }

impl Object for String {
    fn display(&self) -> String {
        self.to_string()
    }

    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        let other = other.try_downcast_ref::<String>()?;

        Ok(Value::new((self.clone() + other).to_string()))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "to_string" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                Ok(Some(Value::new(self.clone())))
            }
            "len" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                Ok(Some(Value::new(self.chars().count() as i64)))
            }

            "split" => {
                if args.len() != 1 {
                    return Err(RuntimeError::invalid_argument_count(1, args.len()));
                }

                let pat = args.first().unwrap().borrow();

                let pat = pat.try_downcast_ref::<String>()?;

                if pat.is_empty() {
                    return Err(RuntimeError::invalid_argument::<String>(
                        0,
                        "pat must not empty",
                    ));
                }

                let parts: Vec<ValueRef> = self
                    .split(pat)
                    .map(|s| ValueRef::new(Value::new(s.to_string())))
                    .collect();

                Ok(Some(Value::new(Array::new(parts))))
            }
            "find" => {
                if args.len() != 1 {
                    return Err(RuntimeError::invalid_argument_count(1, args.len()));
                }
                let pat = args.first().unwrap().borrow();

                let pat = pat.try_downcast_ref::<String>()?;

                match self.find(pat) {
                    Some(index) => {
                        let sub = &self.as_str()[0..index];

                        Ok(Some(Value::new(sub.chars().count() as i64)))
                    }
                    None => Ok(Some(Value::new(-1))),
                }
            }

            "starts_with" => {
                if args.len() != 1 {
                    return Err(RuntimeError::invalid_argument_count(1, args.len()));
                }
                let pat = args.first().unwrap().borrow();

                let pat = pat.try_downcast_ref::<String>()?;

                Ok(Some(Value::new(self.starts_with(pat))))
            }
            "ends_with" => {
                if args.len() != 1 {
                    return Err(RuntimeError::invalid_argument_count(1, args.len()));
                }
                let pat = args.first().unwrap().borrow();

                let pat = pat.try_downcast_ref::<String>()?;

                Ok(Some(Value::new(self.ends_with(pat))))
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!("{} is not a callable property", member),
            )),
        }
    }
}

impl<T: Object> Object for Vec<T> {}

#[derive(Debug)]
pub struct Tuple(Vec<ValueRef>);

impl Tuple {
    pub fn new(values: Vec<ValueRef>) -> Self {
        Tuple(values)
    }
}

impl Object for Tuple {
    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "len" => {
                if args.is_empty() {
                    return Ok(Some(Value::new(self.0.len() as f64)));
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            "make_iterator" => {
                unimplemented!()
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!("{} is not a callable property", member),
            )),
        }
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        let index = *index.try_downcast_ref::<i64>()? as usize;

        self.0
            .get(index)
            .cloned()
            .ok_or(RuntimeError::index_out_of_bound(index, self.0.len()))
    }
}

#[derive(Debug)]
pub struct Array(Vec<ValueRef>);

impl Array {
    pub fn new(elements: Vec<ValueRef>) -> Array {
        Array(elements)
    }

    pub fn push(&mut self, value: ValueRef) {
        self.0.push(value);
    }

    pub fn get(&self, index: usize) -> Option<ValueRef> {
        self.0.get(index).cloned()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl Object for Array {
    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "push" => {
                for arg in args {
                    self.0.push(arg.clone());
                }
                Ok(None)
            }
            "len" => {
                if args.is_empty() {
                    Ok(Some(Value::new(self.0.len() as f64)))
                } else {
                    Err(RuntimeError::invalid_argument_count(0, args.len()))
                }
            }
            "enumerate" => {
                if args.is_empty() {
                    let new_arr: Vec<ValueRef> = self
                        .0
                        .clone()
                        .into_iter()
                        .enumerate()
                        .map(|(i, v)| {
                            ValueRef::new(Value::new(Tuple::new(vec![
                                ValueRef::new(Value::new(i as i64)),
                                v.clone(),
                            ])))
                        })
                        .collect();

                    Ok(Some(Value::new(Array::new(new_arr))))
                } else {
                    Err(RuntimeError::invalid_argument_count(0, args.len()))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!(
                    "{} is not a callable property for {}",
                    member,
                    type_name::<Self>()
                ),
            )),
        }
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Ok(Box::new(self.0.clone().into_iter()))
    }
}

#[derive(Debug)]
pub struct Map(IndexMap<Primitive, ValueRef>);

impl Map {
    pub fn new() -> Map {
        Map(IndexMap::new())
    }
}

impl Object for Map {
    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(key) = index.downcast_ref::<bool>() {
            self.0.insert(Primitive::Boolean(*key), value);
        } else if let Some(key) = index.downcast_ref::<i64>() {
            self.0.insert(Primitive::Integer(*key), value);
        } else if let Some(key) = index.downcast_ref::<f64>() {
            self.0.insert(Primitive::Float(*key), value);
        } else if let Some(key) = index.downcast_ref::<String>() {
            self.0.insert(Primitive::String(key.clone()), value);
        } else {
            return Err(RuntimeError::invalid_type::<String>(format!(
                "{index:?} not a valid key type"
            )));
        }

        Ok(())
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        let key = if let Some(key) = index.downcast_ref::<bool>() {
            Primitive::Boolean(*key)
        } else if let Some(key) = index.downcast_ref::<i64>() {
            Primitive::Integer(*key)
        } else if let Some(key) = index.downcast_ref::<f64>() {
            Primitive::Float(*key)
        } else if let Some(key) = index.downcast_ref::<String>() {
            Primitive::String(key.clone())
        } else {
            return Err(RuntimeError::invalid_type::<String>(format!(
                "{index:?} not a valid key type"
            )));
        };

        self.0
            .get(&key)
            .cloned()
            .ok_or(RuntimeError::key_not_found(key))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Ok(Box::new(self.0.clone().into_iter().map(|(k, v)| {
            ValueRef::new(Value::new(Tuple::new(vec![
                ValueRef::new(Value::from_primitive(k)),
                v.clone(),
            ])))
        })))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "keys" => {
                if args.is_empty() {
                    let keys: Vec<ValueRef> = self
                        .0
                        .keys()
                        .map(|k| ValueRef::new(Value::from_primitive(k.clone())))
                        .collect();

                    Ok(Some(Value::new(Array::new(keys))))
                } else {
                    Err(RuntimeError::invalid_argument_count(0, args.len()))
                }
            }
            "values" => {
                if args.is_empty() {
                    let values: Vec<ValueRef> = self.0.values().cloned().collect();

                    Ok(Some(Value::new(Array::new(values))))
                } else {
                    Err(RuntimeError::invalid_argument_count(0, args.len()))
                }
            }
            "contains_key" => {
                if args.len() == 1 {
                    let key = args[0].borrow();

                    if let Some(key) = key.downcast_ref::<bool>() {
                        Ok(Some(Value::new(
                            self.0.contains_key(&Primitive::Boolean(*key)),
                        )))
                    } else if let Some(key) = key.downcast_ref::<i64>() {
                        Ok(Some(Value::new(
                            self.0.contains_key(&Primitive::Integer(*key)),
                        )))
                    } else if let Some(key) = key.downcast_ref::<f64>() {
                        Ok(Some(Value::new(
                            self.0.contains_key(&Primitive::Float(*key)),
                        )))
                    } else if let Some(key) = key.downcast_ref::<String>() {
                        Ok(Some(Value::new(
                            self.0.contains_key(&Primitive::String(key.clone())),
                        )))
                    } else {
                        Err(RuntimeError::invalid_type::<String>(format!(
                            "{key:?} not a valid key type"
                        )))
                    }
                } else {
                    Err(RuntimeError::invalid_argument_count(1, args.len()))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!(
                    "{} is not a callable property for {}",
                    member,
                    type_name::<Self>()
                ),
            )),
        }
    }
}

// #[derive(Debug)]
// pub struct Function(crate::ir::builder::Function);

// impl Function {
//     pub fn new(func: crate::ir::builder::Function) -> Function {
//         Function(func)
//     }
// }

#[derive(Debug)]
pub struct UserFunction(pub(crate) FunctionId);

impl UserFunction {
    pub fn new(func: FunctionId) -> UserFunction {
        UserFunction(func)
    }
}

impl Object for UserFunction {}

#[derive(Debug)]
pub struct Range {
    begin: i64,
    end: i64,
    bounded: bool,
}

impl Range {
    pub fn new(begin: ValueRef, end: ValueRef, bounded: bool) -> Result<Self, RuntimeError> {
        let begin = begin.borrow();
        let begin = begin.try_downcast_ref::<i64>()?;

        let end = end.borrow();
        let end = end.try_downcast_ref::<i64>()?;

        Ok(Range {
            begin: *begin,
            end: *end,
            bounded,
        })
    }
}

impl Object for Range {
    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        if self.bounded {
            Ok(Box::new(
                (self.begin..=self.end).map(|i| Value::from(i).into()),
            ))
        } else {
            Ok(Box::new(
                (self.begin..self.end).map(|i| Value::from(i).into()),
            ))
        }
    }
}

pub struct Enumerator {
    iter: Box<dyn Iterator<Item = ValueRef>>,
}

impl Enumerator {
    pub fn new(iter: Box<dyn Iterator<Item = ValueRef>>) -> Self {
        Self { iter }
    }
}

impl fmt::Debug for Enumerator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enumerator").finish()
    }
}

impl Object for Enumerator {
    fn iterate_next(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        Ok(self.iter.next())
    }
}

pub struct NativeFunction {
    pub name: String,
    pub func: Box<dyn Function>,
}

impl NativeFunction {
    pub fn new(name: impl ToString, func: Box<dyn Function>) -> Self {
        Self {
            name: name.to_string(),
            func,
        }
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<NativeFunction`{}`>", self.name)
    }
}

impl Object for NativeFunction {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self.func).call(args)
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
    T: Object,
{
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(Some(Value::from(self)))
    }
}

impl IntoRet for Result<Option<Value>, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self
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
    fn from_value(value: &ValueRef) -> Result<ValueRef, RuntimeError> {
        Ok(value.clone())
    }
}

impl<T> FromValue<T> for ValueRef
where
    T: Object + Clone,
{
    fn from_value(value: &ValueRef) -> Result<T, RuntimeError> {
        let value = value.borrow();
        value.try_downcast_ref::<T>().cloned()
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

// impl<F, Ret, T> Callable<T> for F
// where
//     F: Fn(T) -> Ret + Clone + Send + 'static,
//     Ret: IntoRet,
//     T: FromValue
// {
//     fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {

//         let arg = T::from_value(&args[0])?;

//         (self)(arg).into_ret()
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
