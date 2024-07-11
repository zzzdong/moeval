use core::fmt;
use std::slice::Iter;

use super::types::*;

#[derive(Debug, Clone, Copy)]
pub enum Address {
    /// Value on stack
    Stack(usize),
    /// Function
    Function(FunctionId),
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Address::Stack(id) => write!(f, "%{}", id),
            Address::Function(id) => write!(f, "@{}", id.as_usize()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    Not, // eg: !true
    Neg, // eg: -1
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Add => write!(f, "add"),
            Opcode::Sub => write!(f, "sub"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Div => write!(f, "div"),
            Opcode::Mod => write!(f, "mod"),
            Opcode::And => write!(f, "and"),
            Opcode::Or => write!(f, "or"),
            Opcode::Less => write!(f, "lt"),
            Opcode::LessEqual => write!(f, "lte"),
            Opcode::Greater => write!(f, "gt"),
            Opcode::GreaterEqual => write!(f, "gte"),
            Opcode::Equal => write!(f, "eq"),
            Opcode::NotEqual => write!(f, "ne"),
            Opcode::Not => write!(f, "not"),
            Opcode::Neg => write!(f, "neg"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Alloc {
        dst: Address,
    },
    LoadConst {
        dst: Address,
        src: ConstantId,
    },
    LoadEnv {
        dst: Address,
        name: String,
    },
    Store {
        dst: Address,
        src: Address,
    },
    LoadArg {
        index: usize,
        dst: Address,
    },
    UnaryOp {
        op: Opcode,
        dst: Address,
        src: Address,
    },
    BinaryOp {
        op: Opcode,
        dst: Address,
        lhs: Address,
        rhs: Address,
    },
    Call {
        func: Address,
        args: Vec<Address>,
        result: Address,
    },
    PropertyGet {
        dst: Address,
        object: Address,
        property: String,
    },
    PropertySet {
        object: Address,
        property: String,
        value: Address,
    },
    PropertyCall {
        object: Address,
        property: String,
        args: Vec<Address>,
        result: Address,
    },
    Return {
        value: Option<Address>,
    },
    BrIf {
        condition: Address,
        true_blk: BlockId,
        false_blk: BlockId,
    },
    Br {
        dst: BlockId,
    },
    MakeIterator {
        iter: Address,
        result: Address,
    },
    IterateNext {
        iter: Address,
        next: Address,
        after_blk: BlockId,
    },
    Range {
        begin: Address,
        end: Address,
        bounded: bool,
        result: Address,
    },
    NewArray {
        dst: Address,
        size: Option<usize>,
    },
    ArrayPush {
        array: Address,
        value: Address,
    },
    NewMap {
        dst: Address,
    },
    IndexGet {
        dst: Address,
        object: Address,
        index: Address,
    },
    IndexSet {
        object: Address,
        index: Address,
        value: Address,
    },
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Alloc { dst } => {
                write!(f, "{} = alloc", dst)
            }
            Instruction::LoadConst { dst, src } => {
                write!(f, "{} = load_const #{}", dst, src.as_usize())
            }
            Instruction::LoadEnv { dst, name } => {
                write!(f, "{} = load_env @{}", dst, name)
            }
            Instruction::Store { dst, src } => {
                write!(f, "store {} {}", dst, src)
            }
            Instruction::UnaryOp { op, dst, src } => {
                write!(f, "{} = {} {}", dst, op, src)
            }
            Instruction::BinaryOp { op, dst, lhs, rhs } => {
                write!(f, "{} = {} {} {}", dst, op, lhs, rhs)
            }
            Instruction::LoadArg { index, dst } => {
                write!(f, "{} = load_arg #{}", dst, index)
            }
            Instruction::Call {
                func,
                args,
                result: dst,
            } => {
                write!(f, "{} = call {}", dst, func)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
            Instruction::PropertyGet {
                dst,
                object,
                property,
            } => {
                write!(f, "{} = prop_get {} @{}", dst, object, property)
            }
            Instruction::PropertySet {
                object,
                property,
                value,
            } => {
                write!(f, "prop_set {} @{} {}", object, property, value)
            }
            Instruction::PropertyCall {
                object,
                property,
                args,
                result: dst,
            } => {
                write!(f, "{} = prop_call {} @{}", dst, object, property,)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
            Instruction::Return { value } => {
                write!(f, "return")?;
                if let Some(value) = value {
                    write!(f, " {}", value)?;
                }
                Ok(())
            }
            Instruction::Br { dst } => {
                write!(f, "br block#{}", dst.as_usize())
            }
            Instruction::BrIf {
                condition,
                true_blk,
                false_blk,
            } => {
                write!(
                    f,
                    "br_if {} block#{} block#{}",
                    condition,
                    true_blk.as_usize(),
                    false_blk.as_usize()
                )
            }
            Instruction::MakeIterator { iter, result } => {
                write!(f, "make_iterator {} {}", iter, result)
            }
            Instruction::IterateNext {
                iter,
                next,
                after_blk,
            } => {
                write!(
                    f,
                    "iterate_next {} {} block#{}",
                    iter,
                    next,
                    after_blk.as_usize()
                )
            }
            Instruction::Range {
                begin,
                end,
                bounded,
                result,
            } => {
                write!(f, "range {}..", begin)?;
                if *bounded {
                    write!(f, "={}", end)?;
                } else {
                    write!(f, "{}", end)?;
                }
                write!(f, " {}", result)
            }
            Instruction::NewArray { dst: array, size } => {
                write!(f, "{} = new_array", array)?;
                if let Some(size) = size {
                    write!(f, " {}", size)?;
                }
                Ok(())
            }
            Instruction::ArrayPush { array, value } => {
                write!(f, "array_push {} {}", array, value)
            }
            Instruction::NewMap { dst } => {
                write!(f, "{} = new_map", dst)
            }
            Instruction::IndexGet { dst, object, index } => {
                write!(f, "{} = index_get {} {}", dst, object, index)
            }
            Instruction::IndexSet {
                object,
                index,
                value,
            } => {
                write!(f, "index_set {} {} {}", object, index, value)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Instructions {
    pub instructions: Vec<Instruction>,
}

impl Instructions {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction) -> InstId {
        let id = InstId::new(self.instructions.len());
        self.instructions.push(instruction);
        id
    }

    pub fn extend(&mut self, other: Instructions) {
        for inst in other.instructions {
            self.emit(inst);
        }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn iter(&self) -> Iter<Instruction> {
        self.instructions.iter()
    }
}

impl IntoIterator for Instructions {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.instructions.into_iter()
    }
}
