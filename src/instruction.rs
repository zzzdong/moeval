use std::collections::BTreeMap;

pub struct ValueRef(usize);

pub struct BlockId(usize);

pub enum Opcode {
    /// Add
    Add,
    /// Sub
    Sub,
    Mul,
    Div,
    Mod,
}




pub enum Instruction {
    BinaryOp {
        op: Opcode,
        result: ValueRef,
        operand1: ValueRef,
        operand2: ValueRef,
    },
    LoadArg {
        index: usize,
        result: ValueRef,
    },
    Call {
        function: ValueRef,
        result: ValueRef,
    },
    Block {
        block_id: BlockId,
        instruction_index: usize,
    }
}




pub struct Instructions {
    instructions: Vec<Instruction>,
    blocks: BTreeMap<BlockId, Instruction>,
    entry: Option<usize>,
}

pub struct Function {
    instructions: Vec<Instruction>,
}
