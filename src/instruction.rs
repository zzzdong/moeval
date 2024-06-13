use std::collections::BTreeMap;

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
    /// Add
    Add,
    /// Sub
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

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(usize);

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
    func_map: BTreeMap<String, FunctionId>,
    entry: Option<FunctionId>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            func_map: BTreeMap::new(),
            entry: None,
        }
    }
    pub fn declare_function(&mut self, name: impl Into<Option<String>>) -> FunctionId {
        let id = FunctionId(self.functions.len());
        self.functions.push(Function::new(name));
        id
    }

    pub fn define_function(&mut self, id: FunctionId, func: Function) {
        if let Some(ref name) = &func.name {
            if !name.is_empty() {
                self.func_map.insert(name.to_string(), id);
            }
        }

        self.functions[id.0] = func;
    }

    pub fn set_entry(&mut self, id: FunctionId) {
        self.entry = Some(id);
    }

    pub fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.functions[self.entry.unwrap().0].dfg
    }

    pub fn get_function(&self, id: FunctionId) -> &ControlFlowGraph {
        &self.functions[id.0].dfg
    }

    pub fn debug(&self) {
        for (i, func) in self.functions.iter().enumerate() {
            println!("Function{}({:?}):", i, func.name);
            func.dfg.debug();
            println!();
        }
        println!("entry: {:?}", self.entry);
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub dfg: ControlFlowGraph,
}

impl Function {
    pub fn new(name: impl Into<Option<String>>) -> Self {
        Self {
            name: name.into(),
            dfg: ControlFlowGraph::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockId(usize);

#[derive(Debug)]
pub struct Block {
    pub label: Option<String>,
    pub instructions: Vec<Instruction>,
    // pub terminator: Option<Terminator>,
}

impl Block {
    pub fn new(label: Option<String>) -> Self {
        Self {
            label,
            instructions: Vec::new(),
            // terminator: None,
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub constants: Vec<crate::value::Primitive>,
    pub inst_values: Vec<ValueId>,
    pub blocks: Vec<Block>,
    pub entry: Option<BlockId>,
    current_block: Option<BlockId>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            inst_values: Vec::new(),
            blocks: Vec::new(),
            entry: None,
            current_block: None,
        }
    }

    pub fn create_block(&mut self, label: impl Into<Option<String>>) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block::new(label.into()));
        BlockId(id)
    }

    pub fn entry(&self) -> Option<BlockId> {
        self.entry
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.entry = Some(block);
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn emit(&mut self, instruction: Instruction) {
        let block = self.current_block.expect("No current block");
        self.blocks[block.0].emit(instruction);
    }

    pub fn make_constant(&mut self, value: crate::value::Primitive) -> ValueId {
        let idx = self.constants.len();
        self.constants.push(value);
        ValueId::Constant(idx)
    }

    pub fn make_inst_value(&mut self) -> ValueId {
        let idx = self.inst_values.len();
        self.inst_values.push(ValueId::Inst(idx));
        ValueId::Inst(idx)
    }

    pub fn instructions(&self, block: BlockId) -> &[Instruction] {
        &self.blocks[block.0].instructions
    }

    pub fn debug(&self) {
        println!("constants:",);
        for (i, c) in self.constants.iter().enumerate() {
            println!("  {}: {:?}", i, c);
        }
        println!("inst_values:");
        for (i, v) in self.inst_values.iter().enumerate() {
            println!("  {}: {:?}", i, v);
        }
        println!("blocks:");

        for (id, block) in self.blocks.iter().enumerate() {
            println!("  {}:", id);
            for (i, inst) in block.instructions.iter().enumerate() {
                println!("    {}: {:?}", i, inst);
            }
        }
    }
}
