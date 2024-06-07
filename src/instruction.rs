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
    Store {
        object: ValueRef,
        value: ValueRef,
    },
    Return {
        value: Option<ValueRef>,
    },
    BrIf {
        condition: ValueRef,
        then_blk: BlockId,
        else_blk: BlockId,
    },
    Br {
        target: BlockId,
    },
    IterateNext {
        result: ValueRef,
        iter: ValueRef,
    },
    Range {
        begin: ValueRef,
        end: ValueRef,
        result: ValueRef,
    }
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
    pub constants: Vec<crate::value::Value>,
    pub inst_values: Vec<ValueRef>,
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
