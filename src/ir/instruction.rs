use std::fmt;

use super::types::*;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    /// A variable
    Variable(VariableId),
    /// A function
    Function(FunctionId),
    /// A block
    Block(BlockId),
    /// A constant
    Constant(ConstantId),
    /// A br location
    Location(usize),
}

impl Operand {
    pub fn new(id: VariableId) -> Self {
        Operand::Variable(id)
    }

    pub fn id(&self) -> VariableId {
        match self {
            Operand::Variable(id) => *id,
            _ => panic!("Variable::id() called on non-variable({self:?})"),
        }
    }

    pub fn as_block(&self) -> Option<BlockId> {
        match self {
            Operand::Block(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<FunctionId> {
        match self {
            Operand::Function(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_constant(&self) -> Option<ConstantId> {
        match self {
            Operand::Constant(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_location(&self) -> Option<usize> {
        match self {
            Operand::Location(offset) => Some(*offset),
            _ => None,
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Variable(id) => write!(f, "%{}", id.as_usize()),
            Operand::Function(id) => write!(f, "@{}", id.as_usize()),
            Operand::Block(id) => write!(f, "@block{}", id.as_usize()),
            Operand::Constant(id) => write!(f, "#{}", id.as_usize()),
            Operand::Location(offset) => write!(f, ">{}", offset),
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
    /// begin..end
    Range {
        begin: Operand,
        end: Operand,
    },
    /// ..=
    RangeInclusive {
        begin: Operand,
        end: Operand,
    },
    /// begin..
    RangeFrom {
        begin: Operand,
    },
    /// ..
    RangeFull,
    /// ..end
    RangeTo {
        end: Operand,
    },
    /// ..=end
    RangeToInclusive {
        end: Operand,
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
        dst: Operand,
    },
    LoadConst {
        dst: Operand,
        src: Operand,
    },
    LoadEnv {
        dst: Operand,
        name: Operand,
    },
    Store {
        dst: Operand,
        src: Operand,
    },
    LoadArg {
        index: usize,
        dst: Operand,
    },
    UnaryOp {
        op: Opcode,
        dst: Operand,
        src: Operand,
    },
    BinaryOp {
        op: Opcode,
        dst: Operand,
        lhs: Operand,
        rhs: Operand,
    },
    Await {
        promise: Operand,
        dst: Operand,
    },
    Call {
        func: Operand,
        args: Vec<Operand>,
        result: Operand,
    },
    PropertyGet {
        dst: Operand,
        object: Operand,
        property: String,
    },
    PropertySet {
        object: Operand,
        property: String,
        value: Operand,
    },
    PropertyCall {
        object: Operand,
        property: String,
        args: Vec<Operand>,
        result: Operand,
    },
    Return {
        value: Option<Operand>,
    },
    BrIf {
        condition: Operand,
        true_blk: Operand,
        false_blk: Operand,
    },
    Br {
        dst: Operand,
    },
    /// Create an iterator from an object.
    /// The iterator will be stored in `result`.
    MakeIterator {
        iter: Operand,
        result: Operand,
    },
    /// Check if the iterator has another item.
    IteratorHasNext {
        iter: Operand,
        result: Operand,
    },
    /// Get the next value from an iterator.
    /// The next value will be stored in `next`.
    IterateNext {
        iter: Operand,
        next: Operand,
    },
    Range {
        begin: Operand,
        end: Operand,
        bounded: bool,
        result: Operand,
    },
    NewArray {
        dst: Operand,
        size: Option<usize>,
    },
    ArrayPush {
        array: Operand,
        value: Operand,
    },
    NewMap {
        dst: Operand,
    },
    IndexGet {
        dst: Operand,
        object: Operand,
        index: Operand,
    },
    IndexSet {
        object: Operand,
        index: Operand,
        value: Operand,
    },
    Slice {
        dst: Operand,
        object: Operand,
        op: Opcode,
    },
    Halt,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Alloc { dst } => {
                write!(f, "{} = alloc", dst)
            }
            Instruction::LoadConst { dst, src } => {
                write!(f, "{} = load_const {}", dst, src)
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
                write!(f, "br {}", dst)
            }
            Instruction::BrIf {
                condition,
                true_blk,
                false_blk,
            } => {
                write!(f, "br_if {} {} {}", condition, true_blk, false_blk)
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
            Instruction::Halt => write!(f, "halt"),
        }
    }
}
