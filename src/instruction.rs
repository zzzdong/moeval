use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum ValueRef {
    /// Constants value, immediate value
    Constant(usize),
    /// Variable reference
    Variable(usize),
    /// Value from instruction
    Inst(usize),
    /// Function
    Function(usize),
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
        result: ValueRef,
        lhs: ValueRef,
        rhs: ValueRef,
    },
    LoadArg {
        index: usize,
        result: ValueRef,
    },
    Call {
        func: ValueRef,
        args: Vec<ValueRef>,
        result: ValueRef,
    },
    LoadEnv {
        result: ValueRef,
        name: String,
    },
    PropertyGet {
        result: ValueRef,
        object: ValueRef,
        property: String,
    },
    PropertySet {
        object: ValueRef,
        property: String,
        value: ValueRef,
    },
    PropertyCall {
        object: ValueRef,
        property: String,
        args: Vec<ValueRef>,
    },
    Assign {
        object: ValueRef,
        value: ValueRef,
    },
    Return {
        value: Option<ValueRef>,
    },
}

#[derive(Debug)]
pub struct Module {
    constants: Vec<crate::value::Value>,
    inst_values: Vec<ValueRef>,
    functions: Vec<Function>,
    blocks: BTreeMap<BlockId, Block>,
    cfg: ControlFlowGraph,
}

impl Module {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            inst_values: Vec::new(),
            functions: Vec::new(),
            blocks: BTreeMap::new(),
            cfg: ControlFlowGraph::new(),
        }
    }

    pub fn cfg_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.cfg
    }

    pub fn create_block(&mut self, label: Option<impl ToString>) -> BlockId {
        let id = BlockId(self.blocks.len());
        let block = Block::new(label.map(|l| l.to_string()));
        self.blocks.insert(id, block);
        self.cfg.add_block(id);
        id
    }

    pub fn block_mut(&mut self, id: BlockId) -> &mut Block {
        self.blocks.get_mut(&id).unwrap()
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.cfg.set_entry_block(block);
    }

    pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        let idx = self.constants.len();
        self.constants.push(value);
        ValueRef::Constant(idx)
    }

    pub fn make_inst_value(&mut self) -> ValueRef {
        let idx = self.inst_values.len();
        self.inst_values.push(ValueRef::Inst(idx));
        ValueRef::Inst(idx)
    }

    pub fn create_function(&mut self, name: Option<impl ToString>, entry: BlockId) -> ValueRef {
        let idx = self.functions.len();
        self.functions.push(Function {
            name: name.map(|n| n.to_string()),
            entry,
        });
        ValueRef::Function(idx)
    }

    pub fn debug(&self) {
        println!("constants:");
        for (i, c) in self.constants.iter().enumerate() {
            println!("  {}: {:?}", i, c);
        }

        // println!("functions:");
        // for func in &self.functions {
        //     println!("  {:?}", func.name);
        //     func.debug();
        // }

        println!("instructions:");
        let mut idx = 0;
        for (id, blk) in self.blocks.iter() {
            println!(
                "  [{}]{}:",
                id.0,
                blk.label.as_ref().unwrap_or(&"".to_string())
            );
            for inst in &blk.instructions {
                println!("    {} {:?}", idx, inst);
            }
            idx += 1;
        }
    }
}

#[derive(Debug)]
pub struct Function {
    name: Option<String>,
    entry: BlockId,
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
struct BlockNode {
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

#[derive(Debug)]
struct Layout {
    blocks: BTreeMap<BlockId, BlockNode>,
}

impl Layout {
    pub fn new() -> Self {
        Self {
            blocks: BTreeMap::new(),
        }
    }

    pub fn add_block(&mut self, block: BlockId) {
        self.blocks.insert(
            block,
            BlockNode {
                prev: None,
                next: None,
            },
        );
    }

    pub fn set_block_before(&mut self, block: BlockId, before: BlockId) {
        let old = self.blocks[&before].prev;

        self.blocks.get_mut(&before).unwrap().prev = Some(block);
        {
            let blk = self.blocks.get_mut(&block).unwrap();
            blk.next = Some(before);
            blk.prev = old;
        }

        match old {
            Some(id) => self.blocks.get_mut(&id).unwrap().next = Some(block),
            None => {}
        }
    }

    pub fn set_block_after(&mut self, block: BlockId, after: BlockId) {
        let old = self.blocks[&after].next;

        self.blocks.get_mut(&after).unwrap().next = Some(block);
        {
            let blk = self.blocks.get_mut(&block).unwrap();
            blk.prev = Some(after);
            blk.next = old;
        }

        match old {
            Some(id) => self.blocks.get_mut(&id).unwrap().prev = Some(block),
            None => {}
        }
    }

    pub fn get_block_before(&self, block: BlockId) -> Option<BlockId> {
        self.blocks[&block].prev
    }

    pub fn get_block_after(&self, block: BlockId) -> Option<BlockId> {
        self.blocks[&block].next
    }
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub layout: Layout,
    pub entry_block: Option<BlockId>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        let layout = Layout::new();

        Self {
            layout,
            entry_block: None,
        }
    }

    pub fn add_block(&mut self, block: BlockId) {
        self.layout.add_block(block);
    }

    pub fn entry_block(&self) -> Option<BlockId> {
        self.entry_block
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.entry_block = Some(block);
    }

    pub fn set_block_after(&mut self, block: BlockId, after: BlockId) {
        self.layout.set_block_after(block, after);
    }

    pub fn set_block_before(&mut self, block: BlockId, before: BlockId) {
        self.layout.set_block_before(block, before);
    }
}

/*
#[derive(Debug)]
pub struct Instructions {
    instructions: Vec<Instruction>,
    blocks: BTreeMap<BlockId, Block>,
}

impl Instructions {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            blocks: BTreeMap::new(),
        }
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn create_block(&mut self, label: Option<String>) -> BlockId {
        let block_id = BlockId(self.blocks.len());
        self.blocks.insert(
            block_id,
            Block {
                label,
                start: self.location(),
                end: 0,
            },
        );
        block_id
    }

    fn end_block(&mut self, block_id: BlockId) {
        let l = self.location();
        let block = self.blocks.get_mut(&block_id).unwrap();
        block.end = l;
    }

    fn location(&self) -> usize {
        self.instructions.len()
    }

    fn pop(&mut self, block_id: BlockId) -> Vec<Instruction> {
        let block = self.blocks.get_mut(&block_id).unwrap();
        self.instructions.drain(start..end).collect()
    }

    fn push(&mut self, instructions: Vec<Instruction>) {
        self.instructions.extend(instructions);
    }

    pub fn debug(&self) {
        for (i, instruction) in self.instructions.iter().enumerate() {
            if let Some(label) = self.is_block_start(i) {
                println!("{}:", label);
            }
            println!("{:04}: {:?}", i, instruction);
        }
    }

    fn is_block_start(&self, location: usize) -> Option<&String> {
        self.blocks
            .values()
            .find(|block| block.start == location)
            .and_then(|b| b.label.as_ref())
    }
}

pub trait InstBuilder {
    fn instructions(&mut self) -> &mut Instructions;

    fn make_constant(&mut self, value: crate::value::Value) -> ValueRef;

    fn make_inst_value(&mut self) -> ValueRef;

    fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.instructions()
            .emit(Instruction::Assign { object, value })
    }

    fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions().emit(Instruction::PropertyGet {
            result,
            object,
            property: property.to_string(),
        });

        result
    }

    fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.instructions().emit(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    fn call_property(
        &mut self,
        object: ValueRef,
        property: String,
        args: Vec<ValueRef>,
    ) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions().emit(Instruction::PropertyCall {
            object,
            property,
            args,
        });

        result
    }

    fn load_external_variable(&mut self, name: String) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    fn load_argument(&mut self, index: usize) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions()
            .emit(Instruction::Call { result, func, args });

        result
    }

    fn br_if(&mut self, cond: ValueRef, then_blk: BlockId, else_blk: Option<BlockId>) {}

    fn return_(&mut self, value: Option<ValueRef>) {
        self.instructions().emit(Instruction::Return { value });
    }
}



pub struct ModuleBuilder<'a> {
    module: &'a mut Module,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
        }
    }

    pub fn build(self) -> Module {
        self.module
    }

    pub fn create_block(&mut self, label: Option<impl ToString>) -> BlockId {
        self.module
            .instructions
            .create_block(label.map(|l| l.to_string()))
    }

    pub fn end_block(&mut self, block_id: BlockId) {
        self.module.instructions.end_block(block_id)
    }

    pub fn create_function(&mut self, name: Option<impl ToString>) -> FunctionBuilder {
        FunctionBuilder::new(self, name.map(|n| n.to_string()))
    }

    pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        let idx = self.module.constants.len();
        self.module.constants.push(value);
        ValueRef::Constant(idx)
    }

    pub fn make_variable(&mut self, name: impl ToString) -> ValueRef {
        let idx = self.module.variables.len();
        self.module.variables.insert(name.to_string(), idx);
        ValueRef::Variable(idx)
    }

    pub fn make_inst_value(&mut self) -> ValueRef {
        let idx = self.module.inst_values.len();
        self.module.inst_values.push(ValueRef::Inst(idx));
        ValueRef::Inst(idx)
    }

    pub fn make_function(&mut self, func: Function) -> ValueRef {
        let idx = self.module.functions.len();
        self.module.functions.push(func);
        ValueRef::Function(idx)
    }

    pub fn inst_location(&self) -> usize {
        self.module.instructions.location()
    }
}

impl InstBuilder for ModuleBuilder {
    fn instructions(&mut self) -> &mut Instructions {
        &mut self.module.instructions
    }

    fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        ModuleBuilder::make_constant(self, value)
    }

    fn make_inst_value(&mut self) -> ValueRef {
        ModuleBuilder::make_inst_value(self)
    }
}

#[derive(Debug)]
pub struct Function {
    name: Option<String>,
    instructions: Instructions,
}

impl Function {
    pub fn debug(&self) {
        println!("{:?}", self.name);
        self.instructions.debug();
    }
}

pub struct FunctionBuilder {
    function: Function,
    start: usize,
}

impl FunctionBuilder {
    fn new(module: &ModuleBuilder, name: Option<String>) -> Self {
        let start = module.inst_location();
        Self {
            function: Function {
                name,
                instructions: Instructions::new(),
            },
            start,
        }
    }

    pub fn build(self, module: &mut ModuleBuilder) -> ValueRef {
        let FunctionBuilder {
            mut function,
            start,
        } = self;
        let end = module.instructions().location();
        let insts = module.instructions().pop(start, end);
        function.instructions.push(insts);
        module.make_function(function)
    }
}


*/
