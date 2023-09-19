use std::{fmt, ops::Index, slice::SliceIndex};

use crate::{
    ast::BinaryOperation,
    vm::{PrimitiveType, Register, StackOffset, VirtReg}, value::Value,
};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    LoadEnv,
    LoadMember,
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

#[derive(Debug, Clone)]
pub struct Instruction {
    pub opcode: OpCode,
    pub operand0: Operand,
    pub operand1: Operand,
    pub operand2: Operand,
}

impl Instruction {
    pub fn new(opcode: OpCode, operand0: Operand, operand1: Operand, operand2: Operand) -> Self {
        Instruction {
            opcode,
            operand0,
            operand1,
            operand2,
        }
    }

    pub fn single(opcode: OpCode, operand0: Operand) -> Self {
        Instruction {
            opcode,
            operand0,
            operand1: Operand::None,
            operand2: Operand::None,
        }
    }

    pub fn two(opcode: OpCode, operand0: Operand, operand1: Operand) -> Self {
        Instruction {
            opcode,
            operand0,
            operand1,
            operand2: Operand::None,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.opcode, self.operand0)?;
        if self.operand1 != Operand::None {
            write!(f, " {}", self.operand1)?;
        }
        if self.operand2 != Operand::None {
            write!(f, " {}", self.operand2)?;
        }

        Ok(())
    }
}

pub struct Instructions(Vec<Instruction>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn with_instructions(insts: Vec<Instruction>) -> Self {
        Instructions(insts)
    }

    pub fn get(&self, index: usize) -> Option<&Instruction> {
        self.0.get(index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn last(&self) -> Option<&Instruction> {
        self.0.last()
    }

    pub fn push(&mut self, inst: Instruction) {
        self.0.push(inst)
    }

    pub fn extend(&mut self, insts: impl IntoIterator<Item = Instruction>) {
        self.0.extend(insts)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Instruction> {
        self.0.iter()
    }
}

impl Index<usize> for Instructions {
    type Output = Instruction;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IntoIterator for Instructions {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in &self.0 {
            writeln!(f, "{};", i)?;
        }
        Ok(())
    }
}




