use regex::Regex;

use crate::{
    ast::{BinaryOperation, UnaryOperation},
    Error, Value,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Opcode {
    LoadEnv,
    LoadMember,
    StackAlloc,
    Move,
    Load,
    Store,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Negate,
    Not,
    And,
    Or,
    Push,
    Pop,
    In,
    Matches,
    IfEqual,
    IfNotEqual,
    IfLess,
    IfLessOrEqual,
    IfGreater,
    IfGreaterOrEqual,
    NewArray,
    ArrayPush,
    NewDictionary,
    DictionaryPut,
    Index,
    Call,
    Return,
}

impl TryFrom<BinaryOperation> for Opcode {
    type Error = ();

    fn try_from(op: BinaryOperation) -> Result<Self, Self::Error> {
        Ok(match op {
            BinaryOperation::Addition => Opcode::Add,
            BinaryOperation::Subtraction => Opcode::Sub,
            BinaryOperation::Multiplication => Opcode::Mul,
            BinaryOperation::Division => Opcode::Div,
            BinaryOperation::Modulus => Opcode::Mod,
            BinaryOperation::Power => Opcode::Pow,
            BinaryOperation::And => Opcode::And,
            BinaryOperation::Or => Opcode::Or,
            BinaryOperation::In => Opcode::In,
            BinaryOperation::Matches => Opcode::Matches,
            BinaryOperation::Equal => Opcode::IfEqual,
            BinaryOperation::NotEqual => Opcode::IfNotEqual,
            BinaryOperation::GreaterThan => Opcode::IfGreater,
            BinaryOperation::GreaterThanOrEqual => Opcode::IfGreaterOrEqual,
            BinaryOperation::LessThan => Opcode::IfLess,
            BinaryOperation::LessThanOrEqual => Opcode::IfLessOrEqual,
            BinaryOperation::Dot => Opcode::Call,
            _ => {
                return Err(());
            }
        })
    }
}

impl TryFrom<UnaryOperation> for Opcode {
    type Error = ();

    fn try_from(op: UnaryOperation) -> Result<Self, Self::Error> {
        Ok(match op {
            UnaryOperation::Negation => Opcode::Negate,
            UnaryOperation::Not => Opcode::Not,
            UnaryOperation::Try => unimplemented!(), //OpCode::Try,
            _ => {
                return Err(());
            }
        })
    }
}

pub trait Ops
where
    Self: Sized,
{
    fn add(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Add))
    }
    fn sub(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Sub))
    }
    fn mul(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Sub))
    }
    fn div(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Div))
    }
    fn mod_(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Mod))
    }
    fn pow(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Pow))
    }
    fn and(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::And))
    }
    fn or(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Or))
    }
    fn gt(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfGreater))
    }
    fn gte(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfGreaterOrEqual))
    }
    fn lt(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfLess))
    }
    fn lte(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfLessOrEqual))
    }
    fn eq(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfEqual))
    }
    fn ne(&self, _rhs: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::IfNotEqual))
    }
    fn neg(self) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Negate))
    }
    fn not(self) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Not))
    }
    fn in_(&self, _obj: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::In))
    }
    fn index(&self, _index: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Index))
    }
    fn matches(&self, _obj: &Value) -> Result<Value, Error> {
        Err(Error::OpUnimplemented(Opcode::Matches))
    }
    fn call(&self, _args: &[Value]) -> Result<Option<Value>, Error> {
        Err(Error::OpUnimplemented(Opcode::Call))
    }
}

impl Ops for Value {
    fn add(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs + rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 + rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs + *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            (Value::String(lhs), Value::String(rhs)) => {
                let mut s = lhs.clone();
                s.push_str(&rhs);
                Ok(Value::String(s))
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn sub(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs - rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 - rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs - *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn mul(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs * rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 * rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs * (*rhs) as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn div(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs / rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 / rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Float(lhs / *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn mod_(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs % rhs)),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn pow(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Integer(lhs.pow(*rhs as u32))),
            (Value::Integer(_), Value::Float(_))
            | (Value::Float(_), Value::Integer(_))
            | (Value::Float(_), Value::Float(_)) => Err(Error::OpIllegalOperate),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn eq(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(*lhs as f64 == *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs == *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs == rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn ne(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(*lhs as f64 != *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs != *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs != rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn gt(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs > rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(*lhs as f64 > *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs > *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs > rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(false)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn gte(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean(*lhs as f64 >= *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs >= *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(true)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn lt(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs < rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean((*lhs as f64) < *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs < (*rhs as f64))),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs < rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(false)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn lte(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Ok(Value::Boolean((*lhs as f64) <= *rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(*lhs <= (*rhs as f64))),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
            (Value::Boolean(_), Value::Boolean(_)) => Ok(Value::Boolean(true)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn and(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(*lhs && *rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn or(&self, rhs: &Value) -> Result<Value, Error> {
        match (&self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(*lhs || *rhs)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn not(self) -> Result<Value, Error> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn neg(self) -> Result<Value, Error> {
        match self {
            Value::Integer(i) => Ok(Value::Integer(-i)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn in_(&self, obj: &Value) -> Result<Value, Error> {
        match (obj, self) {
            (Value::Array(array), ele) => {
                for item in array {
                    if item == ele {
                        return Ok(Value::Boolean(true));
                    }
                }

                Ok(Value::Boolean(false))
            }
            (Value::Dictionary(map), Value::String(key)) => {
                Ok(Value::Boolean(map.get(key.as_str()).is_some()))
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn index(&self, index: &Value) -> Result<Value, Error> {
        match (&self, index) {
            (Value::Array(array), Value::Integer(i)) => {
                if *i >= 0 && (*i as usize) < array.len() {
                    Ok(array[*i as usize].clone())
                } else {
                    Err(Error::IndexOutOfBounds(*i as usize, array.len()))
                }
            }
            (Value::Dictionary(map), Value::String(key)) => {
                if let Some(value) = map.get(key.as_str()) {
                    Ok(value.clone())
                } else {
                    Err(Error::EntryNotFound(key.to_string()))
                }
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn matches(&self, obj: &Value) -> Result<Value, Error> {
        match (&self, obj) {
            (Value::String(s), Value::String(o)) => {
                let r = Regex::new(o.as_str()).map_err(|e| Error::RegexError(e))?;
                Ok(Value::Boolean(r.is_match(s.as_str())))
            }
            _ => Err(Error::OpIllegalOperate),
        }
    }

    fn call(&self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self {
            Value::Dynamic(obj) => obj.call(args),
            _ => Err(Error::OpUnimplemented(Opcode::Call)),
        }
    }
}
