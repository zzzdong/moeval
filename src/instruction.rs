use std::{fmt, ops::Index};

use crate::{ast::BinaryOperation, value::Primitive};

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
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
    Immed(Primitive),
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

#[derive(Debug, Clone)]
pub struct Module(Vec<Instruction>);

impl Module {
    pub fn new() -> Self {
        Module(Vec::new())
    }

    pub fn with_instructions(insts: Vec<Instruction>) -> Self {
        Module(insts)
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

impl Index<usize> for Module {
    type Output = Instruction;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IntoIterator for Module {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in &self.0 {
            writeln!(f, "{};", i)?;
        }
        Ok(())
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
#[repr(u8)]
pub enum Register {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    Rbp = 13,
    Rsp = 14,
    Rpc = 15,
}

impl Register {
    pub fn registers() -> [Register; 8] {
        [
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
        ]
    }

    pub fn as_usize(&self) -> usize {
        *self as usize
    }
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
            Register::R8 => write!(f, "R8"),
            Register::R9 => write!(f, "R9"),
            Register::R10 => write!(f, "R10"),
            Register::R11 => write!(f, "R11"),
            Register::R12 => write!(f, "R12"),
            Register::Rbp => write!(f, "Rbp"),
            Register::Rsp => write!(f, "Rsp"),
            Register::Rpc => write!(f, "Rpc"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackOffset(usize);

impl StackOffset {
    pub fn new(offset: usize) -> StackOffset {
        StackOffset(offset)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl fmt::Display for StackOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rbp+{}", self.0)
    }
}
