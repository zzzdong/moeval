use super::instruction::*;
use super::types::*;

pub struct Builder<'a> {
    context: &'a mut FunctionContext,
    pub(crate) module: &'a mut Module,
}

impl<'a> Builder<'a> {
    pub fn new(context: &'a mut FunctionContext, module: &'a mut Module) -> Self {
        Self { context, module }
    }

    pub fn flow_graph(&mut self) -> &FlowGraph {
        &self.context.flow_graph
    }

    pub fn flow_graph_mut(&mut self) -> &mut FlowGraph {
        &mut self.context.flow_graph
    }

    fn emit(&mut self, inst: Instruction) {
        self.module.emit(
            self.context
                .flow_graph
                .current_block()
                .expect("no current block"),
            inst,
        );
    }

    pub fn make_constant(&mut self, value: Primitive) -> Address {
        let src = self.module.make_constant(value);
        let dst = self.create_alloc();
        self.emit(Instruction::LoadConst { dst, src });
        dst
    }

    pub fn create_alloc(&mut self) -> Address {
        let addr = Address::Stack(self.context.values.len());
        self.context.values.push(addr);
        self.emit(Instruction::Alloc { dst: addr });
        addr
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        self.module.create_block(label)
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.flow_graph_mut().switch_to_block(block);
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.flow_graph_mut().set_entry_block(block);
    }

    pub fn unaryop(&mut self, op: Opcode, src: Address) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::UnaryOp {
            op,
            dst: result,
            src,
        });

        result
    }

    pub fn binop(&mut self, op: Opcode, lhs: Address, rhs: Address) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::BinaryOp {
            op,
            dst: result,
            lhs,
            rhs,
        });

        result
    }

    pub fn assign(&mut self, dst: Address, src: Address) {
        self.emit(Instruction::Store { dst, src })
    }

    pub fn get_property(&mut self, object: Address, property: &str) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::PropertyGet {
            dst: result,
            object,
            property: property.to_string(),
        });

        result
    }

    pub fn set_property(&mut self, object: Address, property: &str, value: Address) {
        self.emit(Instruction::PropertySet {
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

        self.emit(Instruction::PropertyCall {
            object,
            property,
            args,
            result: dst,
        });

        dst
    }

    pub fn load_external_variable(&mut self, name: String) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::LoadEnv { dst: result, name });

        result
    }

    pub fn load_argument(&mut self, index: usize) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::LoadArg { dst: result, index });

        result
    }

    pub fn make_call(&mut self, func: Address, args: Vec<Address>) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::Call { func, args, result });

        result
    }

    pub fn br_if(&mut self, condition: Address, true_blk: BlockId, false_blk: BlockId) {
        self.emit(Instruction::BrIf {
            condition,
            true_blk,
            false_blk,
        });
    }

    pub fn br(&mut self, dst_blk: BlockId) {
        self.emit(Instruction::Br { dst: dst_blk });
    }

    pub fn return_(&mut self, value: Option<Address>) {
        self.emit(Instruction::Return { value });
    }

    pub fn make_iterator(&mut self, iter: Address) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::MakeIterator { iter, result });

        result
    }

    pub fn iterate_next(&mut self, iter: Address, next: Address, after_blk: BlockId) -> Address {
        let result = self.create_alloc();

        self.emit(Instruction::IterateNext {
            iter,
            next,
            after_blk,
        });

        result
    }

    pub fn range(&mut self, begin: Address, end: Address, bounded: bool) -> Address {
        let result = self.create_alloc();
        self.emit(Instruction::Range {
            begin,
            end,
            bounded,
            result,
        });
        result
    }

    pub fn new_array(&mut self, size: Option<usize>) -> Address {
        let array = self.create_alloc();
        self.emit(Instruction::NewArray { dst: array, size });
        array
    }

    pub fn array_push(&mut self, array: Address, value: Address) -> Address {
        self.emit(Instruction::ArrayPush { array, value });
        array
    }

    pub fn new_map(&mut self) -> Address {
        let map = self.create_alloc();
        self.emit(Instruction::NewMap { dst: map });
        map
    }

    pub fn index_get(&mut self, object: Address, index: Address) -> Address {
        let dst = self.create_alloc();
        self.emit(Instruction::IndexGet { dst, object, index });
        dst
    }

    pub fn index_set(&mut self, object: Address, index: Address, value: Address) {
        self.emit(Instruction::IndexSet {
            object,
            index,
            value,
        });
    }

    pub fn slice(&mut self, object: Address, op: Opcode) -> Address {
        let dst = self.create_alloc();
        self.emit(Instruction::Slice { dst, object, op });
        dst
    }

    pub fn await_promise(&mut self, promise: Address) -> Address {
        let dst = self.create_alloc();
        self.emit(Instruction::Await { dst, promise });
        dst
    }
}
