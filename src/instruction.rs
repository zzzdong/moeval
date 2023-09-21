use std::{fmt, ops::Index};

use crate::{
    ast::{BinaryOperation, UnaryOperation},
    value::Primitive,
};

#[derive(Debug, Clone, Copy)]
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
            BinaryOperation::Member => Opcode::LoadMember,
            BinaryOperation::In => Opcode::In,
            BinaryOperation::Matches => Opcode::Matches,
            BinaryOperation::Equal => Opcode::IfEqual,
            BinaryOperation::NotEqual => Opcode::IfNotEqual,
            BinaryOperation::GreaterThan => Opcode::IfGreater,
            BinaryOperation::GreaterThanOrEqual => Opcode::IfGreaterOrEqual,
            BinaryOperation::LessThan => Opcode::IfLess,
            BinaryOperation::LessThanOrEqual => Opcode::IfLessOrEqual,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Immed(Primitive),
    Register(Register),
    Stack(StackSlot),
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
pub enum InstructionData {
    LoadEnv {
        opcode: Opcode,
        dest: Operand,
        env_var: String,
    },
    LoadMember {
        opcode: Opcode,
        dest: Operand,
        object: Operand,
        member: String,
    },
    Load {
        opcode: Opcode,
        dest: Operand,
        source: Operand,
    },
    Store {
        opcode: Opcode,
        source: Operand,
        dest: Operand,
    },
    AllocStack {
        opcode: Opcode,
        size: usize,
    },
    Push {
        opcode: Opcode,
        value: Operand,
    },
    Pop {
        opcode: Opcode,
        dest: Operand,
    },
    Move {
        opcode: Opcode,
        dest: Operand,
        src: Operand,
    },
    Unary {
        opcode: Opcode,
        dest: Operand,
        src: Operand,
    },
    Binary {
        opcode: Opcode,
        dest: Operand,
        lhs: Operand,
        rhs: Operand,
    },
    Call {
        opcode: Opcode,
        dest: Operand,
        func: Operand,
    },
    Return {
        opcode: Opcode,
        ret: Operand,
    },
    NewArray {
        opcode: Opcode,
        dest: Operand,
    },
    ArrayPush {
        opcode: Opcode,
        array: Operand,
        element: Operand,
    },
    NewDictionary {
        opcode: Opcode,
        dest: Operand,
    },
    DictionaryPut {
        opcode: Opcode,
        dict: Operand,
        key: Operand,
        value: Operand,
    },
    Index {
        opcode: Opcode,
        dest: Operand,
        object: Operand,
        index: Operand,
    },
}

impl InstructionData {
    pub fn load_env(dest: Operand, env_var: &str) -> Self {
        InstructionData::LoadEnv {
            opcode: Opcode::LoadEnv,
            dest,
            env_var: env_var.to_string(),
        }
    }

    pub fn load_member(dest: Operand, object: Operand, member: &str) -> Self {
        InstructionData::LoadMember {
            opcode: Opcode::LoadMember,
            dest,
            object,
            member: member.to_string(),
        }
    }
    pub fn load(dest: Operand, source: Operand) -> Self {
        InstructionData::Load {
            opcode: Opcode::Load,
            dest,
            source,
        }
    }

    pub fn store(source: Operand, dest: Operand) -> Self {
        InstructionData::Store {
            opcode: Opcode::Store,
            source,
            dest,
        }
    }

    pub fn push(value: Operand) -> Self {
        InstructionData::Push {
            opcode: Opcode::Push,
            value,
        }
    }

    pub fn pop(dest: Operand) -> Self {
        InstructionData::Pop {
            opcode: Opcode::Pop,
            dest,
        }
    }

    pub fn move_(dest: Operand, src: Operand) -> Self {
        InstructionData::Move {
            opcode: Opcode::Move,
            dest,
            src,
        }
    }

    pub fn unary(opcode: Opcode, dest: Operand, src: Operand) -> Self {
        InstructionData::Unary { opcode, dest, src }
    }

    pub fn binary(opcode: Opcode, dest: Operand, lhs: Operand, rhs: Operand) -> Self {
        InstructionData::Binary {
            opcode,
            dest,
            lhs,
            rhs,
        }
    }

    pub fn call(dest: Operand, func: Operand) -> Self {
        InstructionData::Call {
            opcode: Opcode::Call,
            dest,
            func,
        }
    }

    pub fn return_(ret: Operand) -> Self {
        InstructionData::Return {
            opcode: Opcode::Return,
            ret,
        }
    }

    pub fn new_array(dest: Operand) -> Self {
        InstructionData::NewArray {
            opcode: Opcode::NewArray,
            dest,
        }
    }

    pub fn array_push(array: Operand, element: Operand) -> Self {
        InstructionData::ArrayPush {
            opcode: Opcode::ArrayPush,
            array,
            element,
        }
    }

    pub fn new_dict(dest: Operand) -> Self {
        InstructionData::NewDictionary {
            opcode: Opcode::NewDictionary,
            dest,
        }
    }
    pub fn dict_put(dict: Operand, key: Operand, value: Operand) -> Self {
        InstructionData::DictionaryPut {
            opcode: Opcode::DictionaryPut,
            dict,
            key,
            value,
        }
    }

    pub fn index(dest: Operand, object: Operand, index: Operand) -> Self {
        InstructionData::Index {
            opcode: Opcode::Index,
            dest,
            object,
            index,
        }
    }

    pub(crate) fn alloc_stack(size: usize) -> InstructionData {
        InstructionData::AllocStack {
            opcode: Opcode::StackAlloc,
            size,
        }
    }
}

impl fmt::Display for InstructionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstructionData::LoadEnv {
                opcode,
                dest,
                env_var,
            } => {
                write!(f, "{:?} {} {}", opcode, dest, env_var)
            }
            InstructionData::LoadMember {
                opcode,
                dest,
                object: source,
                member,
            } => {
                write!(f, "{:?} {} {} {}", opcode, dest, source, member)
            }
            InstructionData::Load {
                opcode,
                dest,
                source,
            } => {
                write!(f, "{:?} {} {}", opcode, dest, source)
            }
            InstructionData::Store {
                opcode,
                source,
                dest,
            } => {
                write!(f, "{:?} {} {}", opcode, source, dest)
            }
            InstructionData::Unary { opcode, dest, src } => {
                write!(f, "{:?} {} {}", opcode, dest, src)
            }
            InstructionData::Binary {
                opcode,
                dest,
                lhs,
                rhs,
            } => {
                write!(f, "{:?} {} {} {}", opcode, dest, lhs, rhs)
            }
            InstructionData::Push { opcode, value } => {
                write!(f, "{:?} {}", opcode, value)
            }
            InstructionData::Pop { opcode, dest } => {
                write!(f, "{:?} {}", opcode, dest)
            }
            InstructionData::Move { opcode, dest, src } => {
                write!(f, "{:?} {} {}", opcode, dest, src)
            }
            InstructionData::Call { opcode, dest, func } => {
                write!(f, "{:?} {} {}", opcode, dest, func)
            }
            InstructionData::Return { opcode, ret } => {
                write!(f, "{:?} {}", opcode, ret)
            }
            InstructionData::NewArray { opcode, dest } => {
                write!(f, "{:?} {}", opcode, dest)
            }
            InstructionData::NewDictionary { opcode, dest } => {
                write!(f, "{:?} {}", opcode, dest)
            }
            InstructionData::ArrayPush {
                opcode,
                array,
                element,
            } => {
                write!(f, "{:?} {} {}", opcode, array, element)
            }
            InstructionData::DictionaryPut {
                opcode,
                dict,
                key,
                value,
            } => {
                write!(f, "{:?} {} {} {}", opcode, dict, key, value)
            }
            InstructionData::Index {
                opcode,
                dest,
                object,
                index,
            } => {
                write!(f, "{:?} {} {} {}", opcode, dest, object, index)
            }
            InstructionData::AllocStack { opcode, size } => {
                write!(f, "{:?} {}", opcode, size)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand0: Operand,
    pub operand1: Operand,
    pub operand2: Operand,
}

impl Instruction {
    pub fn new(opcode: Opcode, operand0: Operand, operand1: Operand, operand2: Operand) -> Self {
        Instruction {
            opcode,
            operand0,
            operand1,
            operand2,
        }
    }

    pub fn single(opcode: Opcode, operand0: Operand) -> Self {
        Instruction {
            opcode,
            operand0,
            operand1: Operand::None,
            operand2: Operand::None,
        }
    }

    pub fn two(opcode: Opcode, operand0: Operand, operand1: Operand) -> Self {
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

impl Default for Module {
    fn default() -> Self {
        Module::new()
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
pub struct StackSlot {
    pub offset: usize,
    pub size: usize,
}

impl StackSlot {
    pub fn new(offset: usize, size: usize) -> StackSlot {
        StackSlot { offset, size }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl fmt::Display for StackSlot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rbp+{}", self.offset)
    }
}
