use super::{types::Block, ControlFlowGraph, Inst, Instruction};

pub trait InstPass: std::fmt::Debug {
    fn run(&mut self, inst: Inst) -> Inst;
}

pub struct InstPassManager {
    passes: Vec<Box<dyn InstPass>>,
}

impl InstPassManager {
    pub fn new() -> Self {
        Self { passes: vec![] }
    }

    pub fn add_pass(&mut self, pass: impl InstPass + 'static) {
        self.passes.push(Box::new(pass));
    }

    pub fn run(&mut self, inst: Inst) -> Inst {
        self.passes
            .iter_mut()
            .fold(inst, |inst, pass| pass.run(inst))
    }
}

/// A simple instruction simplification pass.
/// removing unnecessary br instruction after a return.
#[derive(Debug)]
pub struct SimplifyPass;

impl SimplifyPass {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&mut self, inst: Inst) -> Inst {
        let mut inst = inst;
        self.simplify_graph(&mut inst.control_flow_graph);
        inst.functions.iter_mut().for_each(|func| {
            self.simplify_graph(&mut func.control_flow_graph);
        });
        inst
    }

    /// run the pass on the given control flow graph.
    fn simplify_graph(&mut self, graph: &mut ControlFlowGraph) {
        graph.blocks.iter_mut().for_each(|block| {
            self.simplify_block(block);
        })
    }

    /// Simplify a single block, when block ends with a br and it follows a return, remove the br.
    fn simplify_block(&mut self, block: &mut Block) {
        let len = block.instructions.len();

        if len >= 2 {
            if let (Some(second_last_inst), Some(last_inst)) = (
                block.instructions.get(len - 2),
                block.instructions.get(len - 1),
            ) {
                if matches!(second_last_inst, Instruction::Return { .. })
                    && matches!(last_inst, Instruction::Br { .. })
                {
                    block.instructions.pop();
                }
            }
        }
    }
}

impl InstPass for SimplifyPass {
    fn run(&mut self, inst: Inst) -> Inst {
        inst
    }
}
