use std::collections::HashMap;

use super::instruction::*;
use super::types::*;

pub struct Context {
    /// The function we're compiling.
    pub func: Function,

    /// The control flow graph of `func`.
    pub cfg: ControlFlowGraph,
}

#[derive(Debug)]
pub struct Module {
    pub constants: Vec<crate::value::Primitive>,
    functions: Vec<Function>,
    func_map: HashMap<Name, FunctionId>,
    entry: Option<FunctionId>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: Vec::new(),
            func_map: HashMap::new(),
            entry: None,
        }
    }

    pub fn make_context(func: Function) -> Context {
        Context { func, cfg: ControlFlowGraph::new() }
    }

    pub fn make_constant(&mut self, value: crate::value::Primitive) -> ValueId {
        let id = ValueId::Constant(self.constants.len());
        self.constants.push(value);
        id
    }

    pub fn declare_function(
        &mut self,
        name: impl Into<Name>,
        params: Vec<FunctionParam>,
    ) -> FunctionId {
        let id = FunctionId::new(self.functions.len());
        self.functions.push(Function::new(name, params));
        id
    }

    pub fn define_function(&mut self, id: FunctionId, func: Function) {
        if !func.name.is_anonymous() {
            self.func_map.insert(func.name.clone(), id);
        }

        self.functions[id.as_usize()] = func;

        // compile function into IR
    }

    pub fn set_entry(&mut self, id: FunctionId) {
        self.entry = Some(id);
    }
}

pub struct Builder<'a> {
    context: &'a mut Context,
    module: &'a mut Module,
}

impl<'a> Builder<'a> {
    pub fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.context.cfg
    }

    pub fn make_constant(&mut self, value: crate::value::Primitive) -> ValueId {
        self.module.make_constant(value)
    }

    pub fn make_inst_value(&mut self) -> ValueId {
        self.control_flow_graph_mut().make_inst_value()
    }

    pub fn create_block(&mut self, label: impl Into<Option<String>>) -> BlockId {
        self.control_flow_graph_mut().create_block(label)
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().switch_to_block(block);
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().set_entry_block(block);
    }

    pub fn binop(&mut self, op: Opcode, lhs: ValueId, rhs: ValueId) -> ValueId {
        let result = self.control_flow_graph_mut().make_inst_value();

        self.control_flow_graph_mut().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    pub fn assign(&mut self, object: ValueId, value: ValueId) {
        self.control_flow_graph_mut()
            .emit(Instruction::Store { object, value })
    }

    pub fn get_property(&mut self, object: ValueId, property: &str) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyGet {
                result,
                object,
                property: property.to_string(),
            });

        result
    }

    pub fn set_property(&mut self, object: ValueId, property: &str, value: ValueId) {
        self.control_flow_graph_mut()
            .emit(Instruction::PropertySet {
                object,
                property: property.to_string(),
                value,
            });
    }

    pub fn call_property(&mut self, object: ValueId, property: String, args: Vec<ValueId>) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyCall {
                object,
                property,
                args,
            });

        result
    }

    pub fn load_external_variable(&mut self, name: String) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    pub fn load_argument(&mut self, index: usize) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    pub fn make_call(&mut self, func: ValueId, args: Vec<ValueId>) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::Call { result, func, args });

        result
    }

    pub fn br_if(&mut self, condition: ValueId, then_blk: BlockId, else_blk: BlockId) {
        self.control_flow_graph_mut().emit(Instruction::BrIf {
            condition,
            then_blk,
            else_blk,
        });
    }

    pub fn br(&mut self, target: BlockId) {
        self.control_flow_graph_mut()
            .emit(Instruction::Br { target });
    }

    pub fn return_(&mut self, value: Option<ValueId>) {
        self.control_flow_graph_mut()
            .emit(Instruction::Return { value });
    }

    pub fn make_iterator(&mut self, iter: ValueId) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::MakeIterator { iter, result });

        result
    }

    pub fn iterate_next(&mut self, iter: ValueId, next: ValueId, after_blk: BlockId) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::IterateNext {
                iter,
                next,
                after_blk,
            });

        result
    }

    pub fn range(&mut self, begin: ValueId, end: ValueId) -> ValueId {
        let result = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::Range { begin, end, result });
        result
    }

    pub fn new_array(&mut self, size: Option<usize>) -> ValueId {
        let array = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::NewArray { array, size });
        array
    }

    pub fn array_push(&mut self, array: ValueId, value: ValueId) -> ValueId {
        self.control_flow_graph_mut()
            .emit(Instruction::ArrayPush { array, value });
        array
    }

    // fn new_object(&mut self) -> ValueRef {

    // }
}

pub struct FunctionBuilder<'a> {
    func: &'a mut Function,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func }
    }
}

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
    pub inst_values: Vec<ValueId>,
    pub entry: Option<BlockId>,
    pub blocks: Vec<Block>,
    instructions: Instructions,
    current_block: Option<BlockId>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            inst_values: Vec::new(),
            blocks: Vec::new(),
            instructions: Instructions::new(),
            entry: None,
            current_block: None,
        }
    }

    pub fn create_block(&mut self, label: impl Into<Option<String>>) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block::new(label.into()));
        BlockId::new(id)
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
        self.blocks[block.as_usize()].emit(instruction);
    }

    pub fn make_inst_value(&mut self) -> ValueId {
        let idx = self.inst_values.len();
        self.inst_values.push(ValueId::Inst(idx));
        ValueId::Inst(idx)
    }
}
