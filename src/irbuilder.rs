use crate::instruction::*;

pub trait InstBuilder {
    fn data_flow_graph(&self) -> &DataFlowGraph;

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph;

    fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        self.data_flow_graph_mut().make_constant(value)
    }

    fn make_inst_value(&mut self) -> ValueRef {
        self.data_flow_graph_mut().make_inst_value()
    }

    fn create_block(&mut self, label: impl Into<Option<String>>) -> BlockId {
        self.data_flow_graph_mut().create_block(label)
    }

    fn switch_to_block(&mut self, block: BlockId) {
        self.data_flow_graph_mut().switch_to_block(block);
    }

    fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.data_flow_graph_mut().make_inst_value();

        self.data_flow_graph_mut().emit(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.data_flow_graph_mut()
            .emit(Instruction::Store { object, value })
    }

    fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.data_flow_graph_mut().emit(Instruction::PropertyGet {
            result,
            object,
            property: property.to_string(),
        });

        result
    }

    fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.data_flow_graph_mut().emit(Instruction::PropertySet {
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

        self.data_flow_graph_mut().emit(Instruction::PropertyCall {
            object,
            property,
            args,
        });

        result
    }

    fn load_external_variable(&mut self, name: String) -> ValueRef {
        let result = self.make_inst_value();

        self.data_flow_graph_mut()
            .emit(Instruction::LoadEnv { result, name });

        result
    }

    fn load_argument(&mut self, index: usize) -> ValueRef {
        let result = self.make_inst_value();

        self.data_flow_graph_mut()
            .emit(Instruction::LoadArg { result, index });

        result
    }

    fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.data_flow_graph_mut()
            .emit(Instruction::Call { result, func, args });

        result
    }

    fn br_if(&mut self, condition: ValueRef, then_blk: BlockId, else_blk: Option<BlockId>) {
        self.data_flow_graph_mut().emit(Instruction::BrIf {
            condition,
            then_blk,
            else_blk,
        });
    }

    fn br(&mut self, target: BlockId) {
        self.data_flow_graph_mut().emit(Instruction::Br { target });
    }

    fn return_(&mut self, value: Option<ValueRef>) {
        self.data_flow_graph_mut()
            .emit(Instruction::Return { value });
    }
}

pub struct FunctionBuilder<'a> {
    pub module: &'a mut Module,
    pub func: Function,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(module: &'a mut Module, func: Function) -> Self {
        Self { module, func }
    }

    pub fn finalize(self) -> Function {
        self.func
    }
}

impl<'a> InstBuilder for FunctionBuilder<'a> {
    fn data_flow_graph(&self) -> &DataFlowGraph {
        &self.func.dfg
    }

    fn data_flow_graph_mut(&mut self) -> &mut DataFlowGraph {
        &mut self.func.dfg
    }
}
