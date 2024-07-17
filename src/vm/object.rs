use std::any::type_name;
use std::fmt;
use std::marker::PhantomData;

use futures::{Future, FutureExt};
use indexmap::IndexMap;

use crate::ir::{FunctionId, Primitive};

use super::value::{Value, ValueRef};
use super::RuntimeError;

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
    Await,
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
            OperateKind::Await => write!(f, "await"),
        }
    }
}

pub trait Object: std::any::Any + std::fmt::Debug {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{self:?}"))
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

    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
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

    fn into_future(
        self: Box<Self>,
    ) -> Result<Box<dyn Future<Output = Value> + Unpin + Send + 'static>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Await,
            "unimplemented",
        ))
    }
}

/// Undefined
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

/// ()
impl Object for () {
    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        let other = other.try_downcast_ref::<()>()?;
        self.partial_cmp(other).ok_or_else(|| {
            RuntimeError::invalid_operation(OperateKind::Compare, "can not compare undefined")
        })
    }
}

/// Boolean
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

/// Integer
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

/// Float
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

/// String
impl Object for String {
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

                let pat = args.first().unwrap().get();

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
                let pat = args.first().unwrap().get();

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
                let pat = args.first().unwrap().get();

                let pat = pat.try_downcast_ref::<String>()?;

                Ok(Some(Value::new(self.starts_with(pat))))
            }
            "ends_with" => {
                if args.len() != 1 {
                    return Err(RuntimeError::invalid_argument_count(1, args.len()));
                }
                let pat = args.first().unwrap().get();

                let pat = pat.try_downcast_ref::<String>()?;

                Ok(Some(Value::new(self.ends_with(pat))))
            }
            _ => Err(RuntimeError::invalid_operation(
                OperateKind::PropertyCall,
                format!("{} is not a callable property", member),
            )),
        }
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>().copied() {
            let char_count: i64 = self.chars().count() as i64;

            if index < 0 && index >= char_count {
                return Err(RuntimeError::index_out_of_bound(index, char_count));
            }
            return Ok(ValueRef::new(Value::new(
                self.chars().nth(index as usize).unwrap().to_string(),
            )));
        } else if let Some(index) = index.downcast_ref::<SliceIndex>() {
            let char_count: i64 = self.chars().count() as i64;
            let chars = self.chars();

            match *index {
                SliceIndex::Range { begin, end } => {
                    if begin < 0 || begin > char_count - 1 || begin >= end || end > char_count {
                        return Err(RuntimeError::index_out_of_bound(end, char_count));
                    }
                    let chars = chars.skip(begin as usize).take((end - begin) as usize);
                    return Ok(ValueRef::new(Value::new(String::from_iter(chars))));
                }
                SliceIndex::RangeInclusive { begin, end } => {
                    if begin < 0 || begin > char_count - 1 || begin >= end || end > char_count - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, char_count));
                    }

                    let chars = chars.skip(begin as usize).take((end - begin + 1) as usize);
                    return Ok(ValueRef::new(Value::new(String::from_iter(chars))));
                }
                SliceIndex::RangeFull => return Ok(ValueRef::new(Value::new(self.clone()))),
                SliceIndex::RangeFrom { begin } => {
                    if begin < 0 || begin > char_count - 1 {
                        return Err(RuntimeError::index_out_of_bound(begin, char_count));
                    }
                    let chars = chars.skip(begin as usize);
                    return Ok(ValueRef::new(Value::new(String::from_iter(chars))));
                }
                SliceIndex::RangeTo { end } => {
                    if end < 1 || end > char_count {
                        return Err(RuntimeError::index_out_of_bound(end, char_count));
                    }
                    let chars = chars.take(end as usize);
                    return Ok(ValueRef::new(Value::new(String::from_iter(chars))));
                }
                SliceIndex::RangeToInclusive { end } => {
                    if end < 1 || end > char_count - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, char_count));
                    }
                    let chars = chars.take(end as usize + 1);
                    return Ok(ValueRef::new(Value::new(String::from_iter(chars))));
                }
            }
        }

        Err(RuntimeError::invalid_type::<i64>(index))
    }

    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
        let chars = self.chars().collect::<Vec<char>>();
        Ok(Box::new(
            chars
                .into_iter()
                .map(|c| ValueRef::new(Value::new(c.to_string()))),
        ))
    }
}

/// Tuple
#[derive(Debug, Clone)]
pub struct Tuple(Vec<ValueRef>);

impl Tuple {
    pub fn new(values: Vec<ValueRef>) -> Self {
        Tuple(values)
    }

    fn len(&self) -> i64 {
        self.0.len() as i64
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
                    return Ok(Some(Value::new(self.len())));
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
        if let Some(index) = index.downcast_ref::<i64>().copied() {
            if index < 0 && index >= self.len() {
                return Err(RuntimeError::index_out_of_bound(index, self.len()));
            }
            return Ok(self.0.get(index as usize).cloned().unwrap());
        } else if let Some(index) = index.downcast_ref::<SliceIndex>() {
            match *index {
                SliceIndex::Range { begin, end } => {
                    if begin < 0 || begin > self.len() - 1 || begin >= end || end > self.len() {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Tuple::new(
                        self.0.get(begin as usize..end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeInclusive { begin, end } => {
                    if begin < 0 || begin > self.len() - 1 || begin >= end || end > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Tuple::new(
                        self.0.get(begin as usize..=end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeFull => return Ok(ValueRef::new(Value::new(self.clone()))),
                SliceIndex::RangeFrom { begin } => {
                    if begin < 0 || begin > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(begin, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Tuple::new(
                        self.0.get(begin as usize..).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeTo { end } => {
                    if end < 1 || end > self.len() {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Tuple::new(
                        self.0.get(..end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeToInclusive { end } => {
                    if end < 1 || end > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Tuple::new(
                        self.0.get(..=end as usize).unwrap().to_vec(),
                    ))));
                }
            }
        }

        Err(RuntimeError::invalid_type::<i64>(index))
    }
}

/// Array
#[derive(Debug, Clone)]
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

    pub fn len(&self) -> i64 {
        self.0.len() as i64
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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
                    Ok(Some(Value::new(self.len())))
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

    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
        Ok(Box::new(self.0.clone().into_iter()))
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>().copied() {
            if index < 0 && index >= self.len() {
                return Err(RuntimeError::index_out_of_bound(index, self.len()));
            }
            return Ok(self.0.get(index as usize).cloned().unwrap());
        } else if let Some(index) = index.downcast_ref::<SliceIndex>() {
            match *index {
                SliceIndex::Range { begin, end } => {
                    if begin < 0 || begin > self.len() - 1 || begin >= end || end > self.len() {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Array::new(
                        self.0.get(begin as usize..end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeInclusive { begin, end } => {
                    if begin < 0 || begin > self.len() - 1 || begin >= end || end > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Array::new(
                        self.0.get(begin as usize..=end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeFull => return Ok(ValueRef::new(Value::new(self.clone()))),
                SliceIndex::RangeFrom { begin } => {
                    if begin < 0 || begin > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(begin, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Array::new(
                        self.0.get(begin as usize..).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeTo { end } => {
                    if end < 1 || end > self.len() {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Array::new(
                        self.0.get(..end as usize).unwrap().to_vec(),
                    ))));
                }
                SliceIndex::RangeToInclusive { end } => {
                    if end < 1 || end > self.len() - 1 {
                        return Err(RuntimeError::index_out_of_bound(end, self.len()));
                    }
                    return Ok(ValueRef::new(Value::new(Array::new(
                        self.0.get(..=end as usize).unwrap().to_vec(),
                    ))));
                }
            }
        }

        Err(RuntimeError::invalid_type::<i64>(index))
    }
}

/// Map
#[derive(Debug)]
pub struct Map(IndexMap<Primitive, ValueRef>);

impl Default for Map {
    fn default() -> Self {
        Self::new()
    }
}

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

    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
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
                    let key = args[0].get();

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

/// Range
#[derive(Debug)]
pub enum Range {
    Range { begin: i64, end: i64 },
    RangeInclusive { begin: i64, end: i64 },
}

impl Range {
    pub fn new(begin: ValueRef, end: ValueRef, bounded: bool) -> Result<Self, RuntimeError> {
        if bounded {
            Self::inclusive(begin, end)
        } else {
            Self::exclusive(begin, end)
        }
    }

    pub fn exclusive(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = begin.get();
        let begin = begin.try_downcast_ref::<i64>().copied()?;

        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(Range::Range { begin, end })
    }

    pub fn inclusive(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = begin.get();
        let begin = begin.try_downcast_ref::<i64>().copied()?;

        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(Range::RangeInclusive { begin, end })
    }
}

impl Object for Range {
    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
        match self {
            Range::Range { begin, end } => {
                Ok(Box::new((*begin..*end).map(|i| Value::from(i).into())))
            }
            Range::RangeInclusive { begin, end } => {
                Ok(Box::new((*begin..=*end).map(|i| Value::from(i).into())))
            }
        }
    }
}

/// SliceIndex
#[derive(Debug, PartialEq, Eq)]
pub enum SliceIndex {
    Range { begin: i64, end: i64 },
    RangeInclusive { begin: i64, end: i64 },
    RangeFull,
    RangeFrom { begin: i64 },
    RangeTo { end: i64 },
    RangeToInclusive { end: i64 },
}

impl SliceIndex {
    pub fn range(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = begin.get();
        let begin = begin.try_downcast_ref::<i64>().copied()?;
        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(SliceIndex::Range { begin, end })
    }

    pub fn range_inclusive(begin: ValueRef, end: ValueRef) -> Result<Self, RuntimeError> {
        let begin = begin.get();
        let begin = begin.try_downcast_ref::<i64>().copied()?;
        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(SliceIndex::RangeInclusive { begin, end })
    }

    pub fn range_full() -> Self {
        SliceIndex::RangeFull
    }

    pub fn range_from(begin: ValueRef) -> Result<Self, RuntimeError> {
        let begin = begin.get();
        let begin = begin.try_downcast_ref::<i64>().copied()?;

        Ok(SliceIndex::RangeFrom { begin })
    }

    pub fn range_to(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(SliceIndex::RangeTo { end })
    }

    pub fn range_to_inclusive(end: ValueRef) -> Result<Self, RuntimeError> {
        let end = end.get();
        let end = end.try_downcast_ref::<i64>().copied()?;

        Ok(SliceIndex::RangeToInclusive { end })
    }
}

impl Object for SliceIndex {}

/// Enumerator
pub struct Enumerator {
    iter: Box<dyn Iterator<Item = ValueRef> + Send + Sync>,
}

impl Enumerator {
    pub fn new(iter: Box<dyn Iterator<Item = ValueRef> + Send + Sync>) -> Self {
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

pub struct Promise(pub(crate) Box<dyn Future<Output = Value> + Unpin + Send + Sync + 'static>);

impl Promise {
    pub fn new(fut: impl Future<Output = Value> + Unpin + Send + Sync + 'static) -> Self {
        Self(Box::new(fut))
    }
}

impl Future for Promise {
    type Output = Value;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.0.poll_unpin(cx)
    }
}

impl fmt::Debug for Promise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Promise").finish()
    }
}

impl Object for Promise {}

/// UserFunction
#[derive(Debug)]
pub struct UserFunction(pub(crate) FunctionId);

impl UserFunction {
    pub fn new(func: FunctionId) -> UserFunction {
        UserFunction(func)
    }
}

impl Object for UserFunction {}

/// NativeFunction
pub struct NativeFunction {
    pub name: String,
    pub func: Box<dyn Function + Send + Sync>,
}

impl NativeFunction {
    pub fn new(name: impl ToString, func: Box<dyn Function + Send + Sync>) -> Self {
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

impl IntoRet for Value {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(Some(self))
    }
}

impl<T: Object + Send + Sync> IntoRet for T {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(Some(Value::from(self)))
    }
}

impl IntoRet for Result<Value, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self.map(Some)
    }
}

impl<T: Object + Send + Sync> IntoRet for Result<T, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self.map(|v| Some(Value::new(v)))
    }
}

impl<T: Object + Send + Sync> IntoRet for Result<Option<T>, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self.map(|v| v.map(Value::new))
    }
}

pub trait FromValue: Sized {
    fn from_value(value: &ValueRef) -> Result<Self, RuntimeError>;
}

impl FromValue for ValueRef {
    fn from_value(value: &ValueRef) -> Result<ValueRef, RuntimeError> {
        Ok(value.clone())
    }
}

impl<T> FromValue for T
where
    T: Object + Clone,
{
    fn from_value(value: &ValueRef) -> Result<T, RuntimeError> {
        let value = value.get();
        value.try_downcast_ref::<T>().cloned()
    }
}

pub trait Callable<Args>: Clone + Send + Sync + Sized + 'static {
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
    F: Fn(&[ValueRef]) -> Ret + Clone + Send + Sync + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self)(args).into_ret()
    }
}

impl<F, Ret> Callable<()> for F
where
    F: Fn() -> Ret + Clone + Send + Sync + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self().into_ret()
    }
}

macro_rules! impl_callable {
    ($($idx: expr => $arg: ident),+) => {
        #[allow(non_snake_case)]
        impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
        where
            F: Fn($($arg,)*) -> Ret + Clone + Send + Sync + 'static,
            Ret: IntoRet,
            $( $arg: FromValue + 'static, )*
        {
            fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
                $(
                    let $arg = <$arg>::from_value(args.get($idx).ok_or(RuntimeError::invalid_argument::<$arg>($idx, "NoThing"))?)?;
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

/* TODO:
pub struct Method<T> {
    pub name: String,
    pub func: Box<dyn MethodFunction<T>>,
}

impl<T> fmt::Debug for Method<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Method<{}.{}>", type_name::<T>(), self.name)
    }
}

impl<T: Object> Method<T> {
    pub fn new<Args>(name: impl ToString, method: impl MethodCallable<T, Args>) -> Self
    where
        Args: 'static,
    {
        Method {
            name: name.to_string(),
            func: Box::new(method.into_function()),
        }
    }
}

/// Function trait for external functions.
pub trait MethodFunction<T>: Send + 'static {
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;
}

pub struct IntoMethodFunction<F, Args> {
    func: F,
    _marker: PhantomData<fn(Args) -> ()>,
}

impl<T, F, Args> MethodFunction<T> for IntoMethodFunction<F, Args>
where
    F: MethodCallable<T, Args> + Clone,
    Args: 'static,
{
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self.func.call(this, args)
    }
}

pub trait MethodCallable<T, Args>: Clone + Send + Sized + 'static {
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;

    fn into_function(self) -> IntoMethodFunction<Self, Args> {
        IntoMethodFunction {
            func: self,
            _marker: PhantomData,
        }
    }
}

impl<T, F, Ret> MethodCallable<T, &[ValueRef]> for F
where
    T: Object,
    F: Fn(&mut T, &[ValueRef]) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self)(this, args).into_ret()
    }
}

impl<T, F, Ret> MethodCallable<T, ()> for F
where
    F: Fn(&T) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self(this).into_ret()
    }
}

impl<T, F, Ret, Arg> MethodCallable<T, Arg> for F
where
    F: Fn(&mut T, Arg) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
    Arg: FromValue,
{
    fn call(&mut self, this: &mut T, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        let arg = Arg::from_value(&args[0])?;

        (self)(this, arg).into_ret()
    }
}

*/
