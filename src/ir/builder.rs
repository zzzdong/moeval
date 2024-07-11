use indexmap::IndexSet;
use std::fmt;

use super::instruction::*;
use super::types::*;

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: Name,
}

impl FuncParam {
    pub fn new(name: impl Into<Name>) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub name: Name,
    pub params: Vec<FuncParam>,
}

impl FuncSignature {
    pub fn new(name: impl Into<Name>, params: Vec<FuncParam>) -> Self {
        Self {
            name: name.into(),
            params,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub signature: FuncSignature,
    pub flow_graph: FlowGraph,
}

impl Function {
    pub fn new(id: FunctionId, signature: FuncSignature) -> Self {
        Self {
            id,
            signature,
            flow_graph: FlowGraph::default(),
        }
    }

    pub fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.flow_graph.get_block(id)
    }

    pub fn get_entry_block(&self) -> Option<&Block> {
        match self.flow_graph.entry {
            Some(id) => self.flow_graph.get_block(id),
            None => None,
        }
    }
}

pub struct Context {
    /// The flow graph we are building.
    pub flow_graph: FlowGraph,
}

impl Context {
    pub fn new() -> Self {
        Self {
            flow_graph: FlowGraph::new(),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub constants: IndexSet<Primitive>,
    pub functions: Vec<Function>,
    pub entry: Option<FunctionId>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            constants: IndexSet::new(),
            functions: Vec::new(),
            entry: None,
        }
    }

    pub fn get_function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.as_usize())
    }

    pub fn make_context(&self) -> Context {
        Context::new()
    }

    pub fn make_constant(&mut self, value: Primitive) -> ConstantId {
        match self.constants.get_index_of(&value) {
            Some(index) => ConstantId::new(index),
            None => {
                let id = ConstantId::new(self.constants.len());
                self.constants.insert(value);
                id
            }
        }
    }

    pub fn declare_function(&mut self, signature: FuncSignature) -> FunctionId {
        let id = FunctionId::new(self.functions.len());
        self.functions.push(Function::new(id, signature));
        id
    }

    pub fn define_function(&mut self, id: FunctionId, context: Context) {
        let Context { flow_graph, .. } = context;

        let func = &mut self.functions[id.as_usize()];

        let _ = std::mem::replace(&mut func.flow_graph, flow_graph);
    }

    pub fn set_entry(&mut self, id: FunctionId) {
        self.entry = Some(id);
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, constant) in self.constants.iter().enumerate() {
            writeln!(f, "#{i}:\t {constant:?}")?;
        }

        for func in self.functions.iter() {
            writeln!(
                f,
                "function[{}]@{}()",
                func.id.as_usize(),
                func.signature.name
            )?;
            for block in func.flow_graph.blocks.iter() {
                writeln!(f, "block#{}:", block.id.as_usize())?;
                for instruction in block.instructions.iter() {
                    writeln!(f, "\t{}", instruction)?;
                }
            }
        }

        Ok(())
    }
}

pub struct Builder<'a> {
    context: &'a mut Context,
    pub(crate) module: &'a mut Module,
}

impl<'a> Builder<'a> {
    pub fn new(context: &'a mut Context, module: &'a mut Module) -> Self {
        Self { context, module }
    }

    pub fn flow_graph(&mut self) -> &FlowGraph {
        &self.context.flow_graph
    }

    pub fn flow_graph_mut(&mut self) -> &mut FlowGraph {
        &mut self.context.flow_graph
    }

    pub fn make_constant(&mut self, value: Primitive) -> Address {
        let src = self.module.make_constant(value);
        let dst = self.create_alloc();
        self.flow_graph_mut()
            .emit(Instruction::LoadConst { dst, src });
        dst
    }

    pub fn create_alloc(&mut self) -> Address {
        self.flow_graph_mut().create_alloc()
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        self.flow_graph_mut().create_block(label)
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.flow_graph_mut().switch_to_block(block);
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.flow_graph_mut().set_entry_block(block);
    }

    pub fn unaryop(&mut self, op: Opcode, src: Address) -> Address {
        let result = self.flow_graph_mut().create_alloc();

        self.flow_graph_mut().emit(Instruction::UnaryOp {
            op,
            dst: result,
            src,
        });

        result
    }

    pub fn binop(&mut self, op: Opcode, lhs: Address, rhs: Address) -> Address {
        let result = self.flow_graph_mut().create_alloc();

        self.flow_graph_mut().emit(Instruction::BinaryOp {
            op,
            dst: result,
            lhs,
            rhs,
        });

        result
    }

    pub fn assign(&mut self, dst: Address, src: Address) {
        self.flow_graph_mut().emit(Instruction::Store { dst, src })
    }

    pub fn get_property(&mut self, object: Address, property: &str) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut().emit(Instruction::PropertyGet {
            dst: result,
            object,
            property: property.to_string(),
        });

        result
    }

    pub fn set_property(&mut self, object: Address, property: &str, value: Address) {
        self.flow_graph_mut().emit(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    pub fn call_property(
        &mut self,
        object: Address,
        property: String,
        args: Vec<Address>,
    ) -> Address {
        let dst = self.create_alloc();

        self.flow_graph_mut().emit(Instruction::PropertyCall {
            object,
            property,
            args,
            result: dst,
        });

        dst
    }

    pub fn load_external_variable(&mut self, name: String) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut()
            .emit(Instruction::LoadEnv { dst: result, name });

        result
    }

    pub fn load_argument(&mut self, index: usize) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut()
            .emit(Instruction::LoadArg { dst: result, index });

        result
    }

    pub fn make_call(&mut self, func: Address, args: Vec<Address>) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut()
            .emit(Instruction::Call { func, args, result });

        result
    }

    pub fn br_if(&mut self, condition: Address, true_blk: BlockId, false_blk: BlockId) {
        self.flow_graph_mut().emit(Instruction::BrIf {
            condition,
            true_blk,
            false_blk,
        });
    }

    pub fn br(&mut self, dst_blk: BlockId) {
        self.flow_graph_mut().emit(Instruction::Br { dst: dst_blk });
    }

    pub fn return_(&mut self, value: Option<Address>) {
        self.flow_graph_mut().emit(Instruction::Return { value });
    }

    pub fn make_iterator(&mut self, iter: Address) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut()
            .emit(Instruction::MakeIterator { iter, result });

        result
    }

    pub fn iterate_next(&mut self, iter: Address, next: Address, after_blk: BlockId) -> Address {
        let result = self.create_alloc();

        self.flow_graph_mut().emit(Instruction::IterateNext {
            iter,
            next,
            after_blk,
        });

        result
    }

    pub fn range(&mut self, begin: Address, end: Address, bounded: bool) -> Address {
        let result = self.create_alloc();
        self.flow_graph_mut().emit(Instruction::Range {
            begin,
            end,
            bounded,
            result,
        });
        result
    }

    pub fn new_array(&mut self, size: Option<usize>) -> Address {
        let array = self.create_alloc();
        self.flow_graph_mut()
            .emit(Instruction::NewArray { dst: array, size });
        array
    }

    pub fn array_push(&mut self, array: Address, value: Address) -> Address {
        self.flow_graph_mut()
            .emit(Instruction::ArrayPush { array, value });
        array
    }

    pub fn new_map(&mut self) -> Address {
        let map = self.create_alloc();
        self.flow_graph_mut().emit(Instruction::NewMap { dst: map });
        map
    }

    pub fn index_get(&mut self, object: Address, index: Address) -> Address {
        let dst = self.create_alloc();
        self.flow_graph_mut()
            .emit(Instruction::IndexGet { dst, object, index });
        dst
    }

    pub fn index_set(&mut self, object: Address, index: Address, value: Address) {
        self.flow_graph_mut().emit(Instruction::IndexSet {
            object,
            index,
            value,
        });
    }

    // fn new_object(&mut self) -> ValueRef {

    // }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub label: Name,
    pub instructions: Vec<Instruction>,
    // pub terminator: Option<Terminator>,
}

impl Block {
    pub fn new(id: BlockId, label: impl Into<Name>) -> Self {
        Self {
            id,
            label: label.into(),
            instructions: Vec::new(),
            // terminator: None,
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[derive(Debug, Clone, Default)]
pub struct FlowGraph {
    pub values: Vec<Address>,
    pub entry: Option<BlockId>,
    pub blocks: Vec<Block>,
    current_block: Option<BlockId>,
}

impl FlowGraph {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            blocks: Vec::new(),
            entry: None,
            current_block: None,
        }
    }

    pub fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id.as_usize())
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        let id = BlockId::new(self.blocks.len());
        self.blocks.push(Block::new(id, label.into()));
        id
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

    pub fn create_alloc(&mut self) -> Address {
        let idx = self.values.len();
        let v = Address::Stack(idx);
        self.values.push(v);
        self.emit(Instruction::Alloc { dst: v });
        v
    }
}
