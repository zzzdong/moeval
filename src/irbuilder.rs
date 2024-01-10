use crate::instruction::*;

pub struct IRBuilder<'a> {
    module: &'a mut Module,
    current_block: Option<BlockId>,
}

impl<'a> IRBuilder<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
            current_block: None,
        }
    }

    pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        self.module.make_constant(value)
    }

    pub fn make_inst_value(&mut self) -> ValueRef {
        self.module.make_inst_value()
    }

    pub fn create_function(&mut self, name: Option<impl ToString>, entry: BlockId) -> ValueRef {
        self.module.create_function(name, entry)
    }

    pub fn create_block(&mut self, label: Option<impl ToString>) -> BlockId {
        self.module.create_block(label)
    }

    pub fn set_entry_block(&mut self, block: BlockId) {
        self.module.set_entry_block(block);
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn cfg(&mut self) -> &mut ControlFlowGraph {
        self.module.cfg_mut()
    }

    pub fn current_block(&self) -> BlockId {
        self.current_block.unwrap()
    }

    pub fn current_block_mut(&mut self) -> &mut Block {
        self.module.block_mut(self.current_block.unwrap())
    }

    pub fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    pub fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.current_block_mut()
            .emit(Instruction::Assign { object, value })
    }

    pub fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut().emit(Instruction::PropertyGet {
            result,
            object,
            property: property.to_string(),
        });

        result
    }

    pub fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.current_block_mut().emit(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    pub fn call_property(
        &mut self,
        object: ValueRef,
        property: String,
        args: Vec<ValueRef>,
    ) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut().emit(Instruction::PropertyCall {
            object,
            property,
            args,
        });

        result
    }

    pub fn load_external_variable(&mut self, name: String) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    pub fn load_argument(&mut self, index: usize) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    pub fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block_mut()
            .emit(Instruction::Call { result, func, args });

        result
    }

    pub fn br_if(&mut self, cond: ValueRef, then_blk: BlockId, else_blk: Option<BlockId>) {}

    pub fn return_(&mut self, value: Option<ValueRef>) {
        self.current_block_mut().emit(Instruction::Return { value });
    }
}
