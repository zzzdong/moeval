use std::collections::HashMap;

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

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Dictionary(HashMap<String, Value>),
}
