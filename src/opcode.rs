use std::fmt;

use crate::{
    ast::BinaryOperation,
    vm::{PrimitiveType, Register, StackOffset, VirtReg},
};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    LoadEnv,
    LoadMember,
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
    Call,
    Return,
}

impl TryFrom<BinaryOperation> for OpCode {
    type Error = ();

    fn try_from(op: BinaryOperation) -> Result<Self, Self::Error> {
        Ok(match op {
            BinaryOperation::Addition => OpCode::Add,
            BinaryOperation::Subtraction => OpCode::Sub,
            BinaryOperation::Multiplication => OpCode::Mul,
            BinaryOperation::Division => OpCode::Div,
            BinaryOperation::Modulus => OpCode::Mod,
            BinaryOperation::Power => OpCode::Pow,
            BinaryOperation::And => OpCode::And,
            BinaryOperation::Or => OpCode::Or,
            BinaryOperation::Member => OpCode::LoadMember,
            BinaryOperation::In => OpCode::In,
            BinaryOperation::Matches => OpCode::Matches,
            BinaryOperation::Equal => OpCode::IfEqual,
            BinaryOperation::NotEqual => OpCode::IfNotEqual,
            BinaryOperation::GreaterThan => OpCode::IfGreater,
            BinaryOperation::GreaterThanOrEqual => OpCode::IfGreaterOrEqual,
            BinaryOperation::LessThan => OpCode::IfLess,
            BinaryOperation::LessThanOrEqual => OpCode::IfLessOrEqual,
            _ => {
                return Err(());
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Immed(PrimitiveType),
    Register(Register),
    Stack(StackOffset),
    VirtReg(VirtReg),
    None,
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Immed(value) => write!(f, "{}", value),
            Operand::Register(reg) => write!(f, "{}", reg),
            Operand::VirtReg(vreg) => write!(f, "{}", vreg),
            Operand::Stack(s) => write!(f, "{}", s),
            Operand::None => write!(f, ""),
        }
    }
}
