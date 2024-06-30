use super::types::*;

#[derive(Debug, Clone, Copy)]
pub enum ValueId {
    /// Constants value, immediate value
    Constant(usize),
    /// Variable reference
    Variable(usize),
    /// Value from instruction
    Inst(usize),
    /// Function
    Function(FunctionId),
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
    Member,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    BinaryOp {
        op: Opcode,
        result: ValueId,
        lhs: ValueId,
        rhs: ValueId,
    },
    LoadArg {
        index: usize,
        result: ValueId,
    },
    Call {
        func: ValueId,
        args: Vec<ValueId>,
        result: ValueId,
    },
    LoadEnv {
        result: ValueId,
        name: String,
    },
    PropertyGet {
        result: ValueId,
        object: ValueId,
        property: String,
    },
    PropertySet {
        object: ValueId,
        property: String,
        value: ValueId,
    },
    PropertyCall {
        object: ValueId,
        property: String,
        args: Vec<ValueId>,
    },
    Store {
        object: ValueId,
        value: ValueId,
    },
    Return {
        value: Option<ValueId>,
    },
    BrIf {
        condition: ValueId,
        then_blk: BlockId,
        else_blk: BlockId,
    },
    Br {
        target: BlockId,
    },
    MakeIterator {
        iter: ValueId,
        result: ValueId,
    },
    IterateNext {
        iter: ValueId,
        next: ValueId,
        after_blk: BlockId,
    },
    Range {
        begin: ValueId,
        end: ValueId,
        result: ValueId,
    },
    NewArray {
        array: ValueId,
        size: Option<usize>,
    },
    ArrayPush {
        array: ValueId,
        value: ValueId,
    },
}


#[derive(Debug, Clone)]
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

    
}