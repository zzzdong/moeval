use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::Null => write!(f, "null"),
            PrimitiveType::Bool(b) => write!(f, "{}", b),
            PrimitiveType::Integer(i) => write!(f, "{}", i),
            PrimitiveType::Float(ff) => write!(f, "{}", ff),
            PrimitiveType::Char(c) => write!(f, "'{}'", c),
            PrimitiveType::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtReg(pub usize);

impl fmt::Display for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    Ret,
    Rsp,
    Rbp,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::R0 => write!(f, "R0"),
            Register::R1 => write!(f, "R1"),
            Register::R2 => write!(f, "R2"),
            Register::R3 => write!(f, "R3"),
            Register::R4 => write!(f, "R4"),
            Register::R5 => write!(f, "R5"),
            Register::R6 => write!(f, "R6"),
            Register::R7 => write!(f, "R7"),
            Register::Ret => write!(f, "Ret"),
            Register::Rsp => write!(f, "Rsp"),
            Register::Rbp => write!(f, "Rbp"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackOffset(usize);
impl StackOffset {
    pub fn new(offset: usize) -> StackOffset {
        StackOffset(offset)
    }
}

impl fmt::Display for StackOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rbp+{}", self.0)
    }
}
