use super::instruction::*;
use super::types::*;

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub(crate) blocks: Vec<Block>,
    pub(crate) entry: Option<BlockId>,
    pub(crate) current_block: Option<BlockId>,
    pub(crate) variables: Vec<VariableId>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            entry: None,
            current_block: None,
            variables: Vec::new(),
        }
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        let id = BlockId::new(self.blocks.len());
        self.blocks.push(Block::new(id, label));
        id
    }

    pub fn entry(&self) -> Option<BlockId> {
        self.entry
    }

    pub fn set_entry(&mut self, entry: BlockId) {
        self.entry = Some(entry);
    }

    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    pub fn emit(&mut self, inst: Instruction) {
        self.blocks[self.current_block.expect("no current block").as_usize()].emit(inst);
    }

    pub fn create_variable(&mut self) -> Variable {
        let id = VariableId::new(self.variables.len());
        self.variables.push(id);
        Variable::new(id)
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}

pub trait InstBuilder {
    fn module(&self) -> &Module;

    fn module_mut(&mut self) -> &mut Module;

    fn control_flow_graph(&self) -> &ControlFlowGraph;

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph;

    fn take_control_flow_graph(&mut self) -> ControlFlowGraph {
        std::mem::take(self.control_flow_graph_mut())
    }

    fn emit(&mut self, inst: Instruction) {
        self.control_flow_graph_mut().emit(inst);
    }

    fn create_alloc(&mut self) -> Variable {
        let dst = self.control_flow_graph_mut().create_variable();
        self.emit(Instruction::Alloc { dst });
        dst
    }

    fn make_constant(&mut self, value: Primitive) -> Variable {
        let src = self.module_mut().make_constant(value);
        let dst = self.create_alloc();
        self.emit(Instruction::LoadConst { dst, src });
        dst
    }

    fn create_block(&mut self, label: Name) -> BlockId {
        let block = self.control_flow_graph_mut().create_block(label);
        block
    }

    fn current_block(&self) -> BlockId {
        self.control_flow_graph()
            .current_block()
            .expect("no current block")
    }

    fn switch_to_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().switch_to_block(block);
    }

    fn unaryop(&mut self, op: Opcode, src: Variable) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::UnaryOp {
            op,
            dst: result,
            src,
        });

        result
    }

    fn binop(&mut self, op: Opcode, lhs: Variable, rhs: Variable) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::BinaryOp {
            op,
            dst: result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, dst: Variable, src: Variable) {
        self.emit(Instruction::Store { dst, src })
    }

    fn get_property(&mut self, object: Variable, property: &str) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::PropertyGet {
            dst: result,
            object,
            property: property.to_string(),
        });

        result
    }

    fn set_property(&mut self, object: Variable, property: &str, value: Variable) {
        self.emit(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    fn call_property(
        &mut self,
        object: Variable,
        property: String,
        args: Vec<Variable>,
    ) -> Variable {
        let dst = self.create_alloc();

        self.emit(Instruction::PropertyCall {
            object,
            property,
            args,
            result: dst,
        });

        dst
    }

    fn load_external_variable(&mut self, name: String) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::LoadEnv { dst: result, name });

        result
    }

    fn load_argument(&mut self, index: usize) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::LoadArg { dst: result, index });

        result
    }

    fn make_call(&mut self, func: Variable, args: Vec<Variable>) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::Call { func, args, result });

        result
    }

    fn br(&mut self, dst_blk: BlockId) {
        self.emit(Instruction::Br { dst: dst_blk });
    }

    fn br_if(&mut self, condition: Variable, true_blk: BlockId, false_blk: BlockId) {
        self.emit(Instruction::BrIf {
            condition,
            true_blk,
            false_blk,
        });
    }
    fn return_(&mut self, value: Option<Variable>) {
        self.emit(Instruction::Return { value });
    }

    fn make_iterator(&mut self, iter: Variable) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::MakeIterator { iter, result });

        result
    }

    fn iterator_has_next(&mut self, iter: Variable) -> Variable {
        let result = self.create_alloc();

        self.emit(Instruction::IteratorHasNext { iter, result });

        result
    }

    fn iterate_next(&mut self, iter: Variable) -> Variable {
        let next = self.create_alloc();
        self.emit(Instruction::IterateNext { iter, next });

        next
    }

    fn range(&mut self, begin: Variable, end: Variable, bounded: bool) -> Variable {
        let result = self.create_alloc();
        self.emit(Instruction::Range {
            begin,
            end,
            bounded,
            result,
        });
        result
    }

    fn new_array(&mut self, size: Option<usize>) -> Variable {
        let array = self.create_alloc();
        self.emit(Instruction::NewArray { dst: array, size });
        array
    }

    fn array_push(&mut self, array: Variable, value: Variable) -> Variable {
        self.emit(Instruction::ArrayPush { array, value });
        array
    }

    fn new_map(&mut self) -> Variable {
        let map = self.create_alloc();
        self.emit(Instruction::NewMap { dst: map });
        map
    }

    fn index_get(&mut self, object: Variable, index: Variable) -> Variable {
        let dst = self.create_alloc();
        self.emit(Instruction::IndexGet { dst, object, index });
        dst
    }

    fn index_set(&mut self, object: Variable, index: Variable, value: Variable) {
        self.emit(Instruction::IndexSet {
            object,
            index,
            value,
        });
    }

    fn slice(&mut self, object: Variable, op: Opcode) -> Variable {
        let dst = self.create_alloc();
        self.emit(Instruction::Slice { dst, object, op });
        dst
    }
}

#[derive(Debug)]
pub struct ModuleBuilder<'a> {
    module: &'a mut Module,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self { module }
    }
}

impl<'a> InstBuilder for ModuleBuilder<'a> {
    fn module(&self) -> &Module {
        &self.module
    }

    fn module_mut(&mut self) -> &mut Module {
        &mut self.module
    }

    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.module.control_flow_graph
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.module.control_flow_graph
    }
}

pub struct FunctionBuilder<'long, 'short: 'long> {
    module: &'long mut Module,
    function: &'short mut Function,
}

impl<'long, 'short: 'long> FunctionBuilder<'long, 'short> {
    pub fn new(module: &'long mut Module, function: &'short mut Function) -> Self {
        Self { module, function }
    }
}

impl<'long, 'short: 'long> InstBuilder for FunctionBuilder<'long, 'short> {
    fn module(&self) -> &Module {
        &self.module
    }

    fn module_mut(&mut self) -> &mut Module {
        &mut self.module
    }

    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.function.control_flow_graph
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.function.control_flow_graph
    }
}
