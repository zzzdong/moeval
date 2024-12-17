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
        match &inst {
            Instruction::Br { dst } => {
                let curr = self.current_block.expect("no current block");
                let dst = dst.as_block().expect("not a block");

                self.block_append_predecessor(dst, curr);
                self.block_append_successor(curr, dst);
            }
            Instruction::BrIf {
                true_blk,
                false_blk,
                ..
            } => {
                let curr = self.current_block.expect("no current block");
                let then_blk = true_blk.as_block().expect("not a block");
                let else_blk = false_blk.as_block().expect("not a block");

                self.block_append_predecessor(then_blk, curr);
                self.block_append_predecessor(else_blk, curr);
                self.block_append_successor(curr, then_blk);
                self.block_append_successor(curr, else_blk);
            }
            _ => {}
        }

        self.current_block
            .and_then(|curr| self.blocks.get_mut(curr.as_usize()))
            .expect("no current block")
            .emit(inst);
    }

    pub fn create_variable(&mut self) -> Operand {
        let id = VariableId::new(self.variables.len());
        self.variables.push(id);
        Operand::new(id)
    }

    pub(crate) fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id.as_usize())
    }

    fn block_append_successor(&mut self, block: BlockId, successors: BlockId) {
        let block = self
            .blocks
            .get_mut(block.as_usize())
            .expect("no such block");
        block.append_successor(successors);
    }

    fn block_append_predecessor(&mut self, block: BlockId, predecessors: BlockId) {
        let block = self
            .blocks
            .get_mut(block.as_usize())
            .expect("no such block");
        block.append_predecessor(predecessors);
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}

pub trait InstBuilder {
    fn module(&self) -> &Inst;

    fn module_mut(&mut self) -> &mut Inst;

    fn control_flow_graph(&self) -> &ControlFlowGraph;

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph;

    fn set_entry(&mut self, entry: BlockId) {
        self.control_flow_graph_mut().set_entry(entry);
    }

    fn emit(&mut self, inst: Instruction) {
        self.control_flow_graph_mut().emit(inst);
    }

    fn create_alloc(&mut self) -> Operand {
        let dst = self.control_flow_graph_mut().create_variable();
        self.emit(Instruction::Alloc { dst });
        dst
    }

    fn make_constant(&mut self, value: Primitive) -> Operand {
        let const_id = self.module_mut().make_constant(value);
        let dst = self.create_alloc();
        self.emit(Instruction::LoadConst {
            dst,
            src: Operand::Constant(const_id),
        });
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

    fn unaryop(&mut self, op: Opcode, src: Operand) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::UnaryOp {
            op,
            dst: result,
            src,
        });

        result
    }

    fn binop(&mut self, op: Opcode, lhs: Operand, rhs: Operand) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::BinaryOp {
            op,
            dst: result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, dst: Operand, src: Operand) {
        self.emit(Instruction::Store { dst, src })
    }

    fn get_property(&mut self, object: Operand, property: &str) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::PropertyGet {
            dst: result,
            object,
            property: property.to_string(),
        });

        result
    }

    fn set_property(&mut self, object: Operand, property: &str, value: Operand) {
        self.emit(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    fn call_property(&mut self, object: Operand, property: String, args: Vec<Operand>) -> Operand {
        let dst = self.create_alloc();

        self.emit(Instruction::PropertyCall {
            object,
            property,
            args,
            result: dst,
        });

        dst
    }

    fn load_external_variable(&mut self, name: String) -> Operand {
        let result = self.create_alloc();

        let const_id = self.module_mut().make_constant(Primitive::String(name));

        self.emit(Instruction::LoadEnv {
            dst: result,
            name: Operand::Constant(const_id),
        });

        result
    }

    fn load_argument(&mut self, index: usize) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::LoadArg { dst: result, index });

        result
    }

    fn make_call(&mut self, func: Operand, args: Vec<Operand>) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::Call { func, args, result });

        result
    }

    fn br(&mut self, dst_blk: BlockId) {
        self.emit(Instruction::Br {
            dst: Operand::Block(dst_blk),
        });
    }

    fn br_if(&mut self, condition: Operand, true_blk: BlockId, false_blk: BlockId) {
        self.emit(Instruction::BrIf {
            condition,
            true_blk: Operand::Block(true_blk),
            false_blk: Operand::Block(false_blk),
        });
    }
    fn return_(&mut self, value: Option<Operand>) {
        self.emit(Instruction::Return { value });
    }

    fn make_iterator(&mut self, iter: Operand) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::MakeIterator { iter, result });

        result
    }

    fn iterator_has_next(&mut self, iter: Operand) -> Operand {
        let result = self.create_alloc();

        self.emit(Instruction::IteratorHasNext { iter, result });

        result
    }

    fn iterate_next(&mut self, iter: Operand) -> Operand {
        let next = self.create_alloc();
        self.emit(Instruction::IterateNext { iter, next });

        next
    }

    fn range(&mut self, begin: Operand, end: Operand, bounded: bool) -> Operand {
        let result = self.create_alloc();
        self.emit(Instruction::Range {
            begin,
            end,
            bounded,
            result,
        });
        result
    }

    fn new_array(&mut self, size: Option<usize>) -> Operand {
        let array = self.create_alloc();
        self.emit(Instruction::NewArray { dst: array, size });
        array
    }

    fn array_push(&mut self, array: Operand, value: Operand) -> Operand {
        self.emit(Instruction::ArrayPush { array, value });
        array
    }

    fn new_map(&mut self) -> Operand {
        let map = self.create_alloc();
        self.emit(Instruction::NewMap { dst: map });
        map
    }

    fn index_get(&mut self, object: Operand, index: Operand) -> Operand {
        let dst = self.create_alloc();
        self.emit(Instruction::IndexGet { dst, object, index });
        dst
    }

    fn index_set(&mut self, object: Operand, index: Operand, value: Operand) {
        self.emit(Instruction::IndexSet {
            object,
            index,
            value,
        });
    }

    fn slice(&mut self, object: Operand, op: Opcode) -> Operand {
        let dst = self.create_alloc();
        self.emit(Instruction::Slice { dst, object, op });
        dst
    }

    fn await_promise(&mut self, promise: Operand) -> Operand {
        let dst = self.create_alloc();
        self.emit(Instruction::Await { dst, promise });
        dst
    }
}

#[derive(Debug)]
pub struct ModuleBuilder<'a> {
    module: &'a mut Inst,
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(module: &'a mut Inst) -> Self {
        Self { module }
    }
}

impl<'a> InstBuilder for ModuleBuilder<'a> {
    fn module(&self) -> &Inst {
        &self.module
    }

    fn module_mut(&mut self) -> &mut Inst {
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
    module: &'long mut Inst,
    function: &'short mut Function,
}

impl<'long, 'short: 'long> FunctionBuilder<'long, 'short> {
    pub fn new(module: &'long mut Inst, function: &'short mut Function) -> Self {
        Self { module, function }
    }
}

impl<'long, 'short: 'long> InstBuilder for FunctionBuilder<'long, 'short> {
    fn module(&self) -> &Inst {
        &self.module
    }

    fn module_mut(&mut self) -> &mut Inst {
        &mut self.module
    }

    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.function.control_flow_graph
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.function.control_flow_graph
    }
}
