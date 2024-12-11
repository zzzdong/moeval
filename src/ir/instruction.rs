use core::fmt;
use std::slice::Iter;

use super::types::*;

#[derive(Debug, Clone, Copy)]
pub struct Variable(VariableId);

impl Variable {
    pub fn new(id: VariableId) -> Self {
        Variable(id)
    }

    pub fn id(&self) -> VariableId {
        self.0
    }
}


impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0.as_usize())
    }
}

// #[derive(Debug, Clone, Copy)]
// pub enum Address {
//     /// Value on stack
//     Stack(usize),
//     /// Function
//     Function(FunctionId),
// }

// impl fmt::Display for Address {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Address::Stack(id) => write!(f, "%{}", id),
//             Address::Function(id) => write!(f, "@{}", id.as_usize()),
//         }
//     }
// }

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
    /// begin..end
    Range {
        begin: Variable,
        end: Variable,
    },
    /// ..=
    RangeInclusive {
        begin: Variable,
        end: Variable,
    },
    /// begin..
    RangeFrom {
        begin: Variable,
    },
    /// ..
    RangeFull,
    /// ..end
    RangeTo {
        end: Variable,
    },
    /// ..=end
    RangeToInclusive {
        end: Variable,
    },
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
            Opcode::Range { begin, end } => write!(f, "{}..{}", begin, end),
            Opcode::RangeInclusive { begin, end } => write!(f, "{}..={}", begin, end),
            Opcode::RangeFrom { begin } => write!(f, "{}..", begin),
            Opcode::RangeFull => write!(f, ".."),
            Opcode::RangeTo { end } => write!(f, "..{}", end),
            Opcode::RangeToInclusive { end } => write!(f, "..={}", end),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Alloc {
        dst: Variable,
    },
    LoadConst {
        dst: Variable,
        src: ConstantId,
    },
    LoadEnv {
        dst: Variable,
        name: String,
    },
    Store {
        dst: Variable,
        src: Variable,
    },
    LoadArg {
        index: usize,
        dst: Variable,
    },
    UnaryOp {
        op: Opcode,
        dst: Variable,
        src: Variable,
    },
    BinaryOp {
        op: Opcode,
        dst: Variable,
        lhs: Variable,
        rhs: Variable,
    },
    Await {
        promise: Variable,
        dst: Variable,
    },
    Call {
        func: Variable,
        args: Vec<Variable>,
        result: Variable,
    },
    PropertyGet {
        dst: Variable,
        object: Variable,
        property: String,
    },
    PropertySet {
        object: Variable,
        property: String,
        value: Variable,
    },
    PropertyCall {
        object: Variable,
        property: String,
        args: Vec<Variable>,
        result: Variable,
    },
    Return {
        value: Option<Variable>,
    },
    BrIf {
        condition: Variable,
        true_blk: BlockId,
        false_blk: BlockId,
    },
    Br {
        dst: BlockId,
    },
    /// Create an iterator from an object.
    /// The iterator will be stored in `result`.
    MakeIterator {
        iter: Variable,
        result: Variable,
    },
    /// Check if the iterator has another item.
    IteratorHasNext {
        iter: Variable,
        result: Variable,
    },
    /// Get the next value from an iterator.
    /// The next value will be stored in `next`.
    IterateNext {
        iter: Variable,
        next: Variable,
    },
    Range {
        begin: Variable,
        end: Variable,
        bounded: bool,
        result: Variable,
    },
    NewArray {
        dst: Variable,
        size: Option<usize>,
    },
    ArrayPush {
        array: Variable,
        value: Variable,
    },
    NewMap {
        dst: Variable,
    },
    IndexGet {
        dst: Variable,
        object: Variable,
        index: Variable,
    },
    IndexSet {
        object: Variable,
        index: Variable,
        value: Variable,
    },
    Slice {
        dst: Variable,
        object: Variable,
        op: Opcode,
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
            Instruction::Await { promise, dst } => {
                write!(f, "{} = await {}", dst, promise)
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
                write!(f, "jump block#{}", dst.as_usize())
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
            Instruction::IteratorHasNext { iter, result } => {
                write!(f, "iterator_has_next {} {}", iter, result)
            }
            Instruction::IterateNext { iter, next } => {
                write!(f, "iterate_next {} {}", iter, next,)
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
            Instruction::Slice { dst, object, op } => {
                write!(f, "{} = slice {} {}", dst, object, op)
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
