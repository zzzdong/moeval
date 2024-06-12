use crate::instruction::*;

pub trait InstBuilder {
    fn control_flow_graph(&self) -> &ControlFlowGraph;

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph;

    fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        self.control_flow_graph_mut().make_constant(value)
    }

    fn make_inst_value(&mut self) -> ValueRef {
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

    fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.control_flow_graph_mut().make_inst_value();

        self.control_flow_graph_mut().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.control_flow_graph_mut()
            .emit(Instruction::Store { object, value })
    }

    fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyGet {
                result,
                object,
                property: property.to_string(),
            });

        result
    }

    fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.control_flow_graph_mut()
            .emit(Instruction::PropertySet {
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

        self.control_flow_graph_mut()
            .emit(Instruction::PropertyCall {
                object,
                property,
                args,
            });

        result
    }

    fn load_external_variable(&mut self, name: String) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    fn load_argument(&mut self, index: usize) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::Call { result, func, args });

        result
    }

    fn br_if(&mut self, condition: ValueRef, then_blk: BlockId, else_blk: BlockId) {
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

    fn return_(&mut self, value: Option<ValueRef>) {
        self.control_flow_graph_mut()
            .emit(Instruction::Return { value });
    }

    fn make_iterator(&mut self, iter: ValueRef) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::MakeIterator { iter, result });

        result
    }

    fn iterate_next(&mut self, iter: ValueRef, next: ValueRef, after_blk: BlockId) -> ValueRef {
        let result = self.make_inst_value();

        self.control_flow_graph_mut()
            .emit(Instruction::IterateNext {
                iter,
                next,
                after_blk,
            });

        result
    }

    fn range(&mut self, begin: ValueRef, end: ValueRef) -> ValueRef {
        let result = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::Range { begin, end, result });
        result
    }

    fn new_array(&mut self, size: Option<usize>) -> ValueRef {
        let array = self.make_inst_value();
        self.control_flow_graph_mut()
            .emit(Instruction::NewArray { array, size });
        array
    }

    fn array_push(&mut self, array: ValueRef, value: ValueRef) -> ValueRef {
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
