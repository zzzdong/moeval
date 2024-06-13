use crate::instruction::*;

pub trait InstBuilder {
    fn control_flow_graph(&self) -> &ControlFlowGraph;

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph;

    fn make_constant(&mut self, value: crate::value::Value) -> ValueId {
        self.control_flow_graph_mut().make_constant(value)
    }

    fn make_inst_value(&mut self) -> ValueId {
        self.control_flow_graph_mut().make_inst_value()
    }

    fn create_block(&mut self, label: impl Into<Option<String>>) -> BlockId {
        self.control_flow_graph_mut().create_block(label)
    }

    fn switch_to_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().switch_to_block(block);
    }

    fn set_entry_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().set_entry_block(block);
    }

    fn binop(&mut self, op: Opcode, lhs: ValueId, rhs: ValueId) -> ValueId {
        let result = self.control_flow_graph_mut().make_inst_value();

        self.control_flow_graph_mut().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, object: ValueId, value: ValueId) {
        self.control_flow_graph_mut()
            .emit(Instruction::Store { object, value })
    }

    fn get_property(&mut self, object: ValueId, property: &str) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyGet {
                result,
                object,
                property: property.to_string(),
            });

        result
    }

    fn set_property(&mut self, object: ValueId, property: &str, value: ValueId) {
        self.control_flow_graph_mut()
            .emit(Instruction::PropertySet {
                object,
                property: property.to_string(),
                value,
            });
    }

    fn call_property(
        &mut self,
        object: ValueId,
        property: String,
        args: Vec<ValueId>,
    ) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyCall {
                object,
                property,
                args,
            });

        result
    }

    fn load_external_variable(&mut self, name: String) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    fn load_argument(&mut self, index: usize) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    fn make_call(&mut self, func: ValueId, args: Vec<ValueId>) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::Call { result, func, args });

        result
    }

    fn br_if(&mut self, condition: ValueId, then_blk: BlockId, else_blk: BlockId) {
        self.control_flow_graph_mut().emit(Instruction::BrIf {
            condition,
            then_blk,
            else_blk,
        });
    }

    fn br(&mut self, target: BlockId) {
        self.control_flow_graph_mut()
            .emit(Instruction::Br { target });
    }

    fn return_(&mut self, value: Option<ValueId>) {
        self.control_flow_graph_mut()
            .emit(Instruction::Return { value });
    }

    fn make_iterator(&mut self, iter: ValueId) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::MakeIterator { iter, result });

        result
    }

    fn iterate_next(&mut self, iter: ValueId, next: ValueId, after_blk: BlockId) -> ValueId {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::IterateNext {
                iter,
                next,
                after_blk,
            });

        result
    }

    fn range(&mut self, begin: ValueId, end: ValueId) -> ValueId {
        let result = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::Range { begin, end, result });
        result
    }

    fn new_array(&mut self, size: Option<usize>) -> ValueId {
        let array = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::NewArray { array, size });
        array
    }

    fn array_push(&mut self, array: ValueId, value: ValueId) -> ValueId {
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

impl<'a> InstBuilder for FunctionBuilder<'a> {
    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.func.dfg
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.func.dfg
    }
}
