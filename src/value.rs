use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Null,
    Bool,
    Integer,
    Float,
    String,
    Array,
    Dictionary,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    Array(Vec<Value>),
    Dictionary(HashMap<String, Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(ff) => write!(f, "{}", ff),
            Value::Char(c) => write!(f, "'{}'", c),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(a) => {
                write!(f, "[")?;
                for item in a {
                    write!(f, "{},", item)?;
                }
                write!(f, "]")
            }
            Value::Dictionary(d) => {
                write!(f, "{{")?;
                for (key, value) in d {
                    write!(f, "{}: {},", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}
