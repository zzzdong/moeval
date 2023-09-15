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
