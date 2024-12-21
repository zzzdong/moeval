use indexmap::IndexSet;
use petgraph::algo::{kosaraju_scc, toposort};
use petgraph::graph::{self, DiGraph};
use petgraph::visit::{DfsPostOrder, IntoNodeIdentifiers};

use super::builder::ControlFlowGraph;
use super::instruction::*;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive {
    #[default]
    Undefined,
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Boolean(b) => write!(f, "{}", b),
            Primitive::Byte(b) => write!(f, "{}", b),
            Primitive::Integer(i) => write!(f, "{}", i),
            Primitive::Float(ff) => write!(f, "{}", ff),
            Primitive::Char(c) => write!(f, "{}", c),
            Primitive::String(s) => write!(f, "{}", s),
            Primitive::Undefined => write!(f, "Undefined"),
        }
    }
}

impl Eq for Primitive {}

impl std::hash::Hash for Primitive {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Primitive::Boolean(b) => b.hash(state),
            Primitive::Byte(b) => b.hash(state),
            Primitive::Integer(i) => i.hash(state),
            Primitive::Float(f) => f.to_bits().hash(state),
            Primitive::Char(c) => c.hash(state),
            Primitive::String(s) => s.hash(state),
            Primitive::Undefined => 1.hash(state),
        }
    }
}

macro_rules! id_entity {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(usize);

        impl $name {
            pub fn new(id: usize) -> Self {
                Self(id)
            }

            pub fn id(&self) -> usize {
                self.0
            }

            pub fn as_usize(&self) -> usize {
                self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}<{}>", std::any::type_name::<$name>(), self.0)
            }
        }
    };
}

id_entity!(ConstantId);

id_entity!(VariableId);

id_entity!(InstId);

id_entity!(BlockId);

id_entity!(FunctionId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(pub(crate) Option<String>);

impl Name {
    pub fn new(name: impl ToString) -> Self {
        Self(Some(name.to_string()))
    }

    pub fn anonymous() -> Self {
        Self(None)
    }

    pub fn is_anonymous(&self) -> bool {
        self.0.is_none()
    }
}

impl From<Option<String>> for Name {
    fn from(value: Option<String>) -> Self {
        Self(value)
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl Default for Name {
    fn default() -> Self {
        Self::anonymous()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.as_deref().unwrap_or("<anonymous>"))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub(crate) id: BlockId,
    pub(crate) label: Name,
    pub(crate) instructions: Vec<Instruction>,
}

impl Block {
    pub fn new(id: BlockId, label: impl Into<Name>) -> Self {
        Self {
            id,
            label: label.into(),
            instructions: Vec::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

pub struct Instructions {
    instructions: Vec<Instruction>,
}

impl Instructions {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn from_control_flow_graph(control_flow_graph: ControlFlowGraph) -> Self {
        let mut instructions = Vec::new();

        let sorted_blocks = sort_graph_blocks(&control_flow_graph);

        let mut block_map = HashMap::new();

        let mut block_offset = 0;
        for block in &sorted_blocks {
            block_map.insert(block.id, block_offset);
            block_offset += block.instructions.len();
        }

        for block in sorted_blocks {
            for inst in block.instructions {
                instructions.push(rewrite_br_inst(inst, &block_map));
            }
        }

        Instructions { instructions }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn get(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    pub fn iter(&self) -> std::slice::Iter<Instruction> {
        self.instructions.iter()
    }
}

fn rewrite_block_operand(operand: Operand, block_map: &HashMap<BlockId, usize>) -> Operand {
    match operand {
        Operand::Block(block_id) => Operand::Location(block_map[&block_id]),
        _ => operand,
    }
}

fn rewrite_br_inst(inst: Instruction, block_map: &HashMap<BlockId, usize>) -> Instruction {
    match inst {
        Instruction::Br { dst } => Instruction::Br {
            dst: rewrite_block_operand(dst, &block_map),
        },
        Instruction::BrIf {
            condition,
            true_blk,
            false_blk,
        } => Instruction::BrIf {
            condition,
            true_blk: rewrite_block_operand(true_blk, &block_map),
            false_blk: rewrite_block_operand(false_blk, &block_map),
        },
        _ => inst,
    }
}

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
    pub control_flow_graph: ControlFlowGraph,
}

impl Function {
    pub fn new(id: FunctionId, signature: FuncSignature) -> Self {
        Self {
            id,
            signature,
            control_flow_graph: ControlFlowGraph::new(),
        }
    }
}

#[derive(Debug)]
pub struct Inst {
    pub constants: IndexSet<Primitive>,
    pub functions: Vec<Function>,
    pub control_flow_graph: ControlFlowGraph,
}

impl Inst {
    pub fn new() -> Self {
        Self {
            constants: IndexSet::new(),
            functions: Vec::new(),
            control_flow_graph: ControlFlowGraph::new(),
        }
    }

    pub fn get_function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.as_usize())
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

    pub fn define_function(&mut self, id: FunctionId, func: Function) {
        let old = &mut self.functions[id.as_usize()];

        let _ = std::mem::replace(old, func);
    }

    pub fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.control_flow_graph.blocks.get(id.as_usize())
    }

    pub fn into_module(self, name: impl Into<Name>) -> Module {
        let Inst {
            constants,
            functions,
            control_flow_graph,
        } = self;

        let main_insts = Instructions::from_control_flow_graph(control_flow_graph);

        let func_insts: HashMap<FunctionId, Instructions> = functions
            .into_iter()
            .map(|func| {
                (
                    func.id,
                    Instructions::from_control_flow_graph(func.control_flow_graph),
                )
            })
            .collect();

        let mut func_offset_map = HashMap::new();
        let mut func_offset = main_insts.instructions.len();

        for (func_id, insts) in &func_insts {
            func_offset_map.insert(*func_id, func_offset);
            func_offset += insts.instructions.len();
        }

        let mut instructions = Instructions::new();

        // rewrite function address
        for inst in main_insts.instructions {
            let inst = rewrite_inst_function_address(inst, &func_offset_map);
            instructions.push(inst);
        }

        for (func, insts) in func_insts {
            let func_offset = func_offset_map
                .get(&func)
                .cloned()
                .expect("function offset not found");
            for inst in insts.instructions {
                let inst = rewrite_inst_function_address(inst, &func_offset_map);

                // rewrite br offset
                let inst = rewrite_br_offset(inst, func_offset);

                instructions.push(inst);
            }
        }

        // 创建模块
        Module::new(name.into(), constants, instructions)
    }
}

fn rewrite_function_address_operand(
    operand: Operand,
    func_offset_map: &HashMap<FunctionId, usize>,
) -> Operand {
    match operand {
        Operand::Function(func_id) => {
            if let Some(offset) = func_offset_map.get(&func_id) {
                return Operand::Location(*offset);
            } else {
                panic!("Function ID not found in offset map");
            }
        }
        _ => operand,
    }
}

fn rewrite_inst_function_address(
    inst: Instruction,
    func_offset_map: &HashMap<FunctionId, usize>,
) -> Instruction {
    match inst {
        Instruction::Call { func, args, result } => {
            let args = args
                .into_iter()
                .map(|arg| rewrite_function_address_operand(arg, func_offset_map))
                .collect();
            let new_func = rewrite_function_address_operand(func, func_offset_map);
            Instruction::Call {
                func: new_func,
                args,
                result,
            }
        }
        Instruction::Store { dst, src } => {
            let new_src: Operand = rewrite_function_address_operand(src, func_offset_map);
            Instruction::Store { dst, src: new_src }
        }
        _ => inst,
    }
}

fn rewrite_br_offset(inst: Instruction, offset: usize) -> Instruction {
    match inst {
        Instruction::Br { dst } => Instruction::Br {
            dst: Operand::Location(dst.as_location().expect("expected offset") + offset),
        },
        Instruction::BrIf {
            condition,
            true_blk,
            false_blk,
        } => Instruction::BrIf {
            condition,
            true_blk: Operand::Location(true_blk.as_location().expect("expected offset") + offset),
            false_blk: Operand::Location(
                false_blk.as_location().expect("expected offset") + offset,
            ),
        },
        _ => inst,
    }
}

impl Default for Inst {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, constant) in self.constants.iter().enumerate() {
            writeln!(f, "#{i}:\t {constant:?}")?;
        }

        for block in self.control_flow_graph.blocks.iter() {
            writeln!(f, "block#{}@{}:", block.id.as_usize(), block.label)?;
            for instruction in block.instructions.iter() {
                writeln!(f, "\t{}", instruction)?;
            }
        }

        for func in self.functions.iter() {
            writeln!(
                f,
                "function[{}]@{}()",
                func.id.as_usize(),
                func.signature.name
            )?;
            for block in func.control_flow_graph.blocks.iter() {
                writeln!(f, "block#{}@{}:", block.id.as_usize(), block.label)?;
                for instruction in block.instructions.iter() {
                    writeln!(f, "\t{}", instruction)?;
                }
            }
        }

        Ok(())
    }
}

fn sort_graph_blocks(control_flow_graph: &ControlFlowGraph) -> Vec<Block> {
    let graph = &control_flow_graph.graph;
    let entry = control_flow_graph.entry().expect("no entry block");
    let start = control_flow_graph.block_node_map[&entry];

    let mut dfs = DfsPostOrder::new(graph, start);

    let mut sorted = Vec::new();

    while let Some(node) = dfs.next(graph) {
        let block_id = graph[node];
        sorted.push(block_id);
    }

    sorted.reverse();

    sorted
        .into_iter()
        .map(|block_id| {
            let block = control_flow_graph
                .get_block(block_id)
                .expect("no such block");
            block.clone()
        })
        .collect()
}

// fn sort_graph_blocks(control_flow_graph: &ControlFlowGraph) -> Vec<Block> {
//     let mut graph = Graph::<(), (), Directed>::new();
//     let mut node_map = HashMap::new();

//     // 创建图节点
//     for block in &control_flow_graph.blocks {
//         let node = graph.add_node(());
//         node_map.insert(block.id, node);
//     }

//     // 添加边
//     for block in &control_flow_graph.blocks {
//         for successor in &block.successors {
//             if let Some(&src_node) = node_map.get(&block.id) {
//                 if let Some(&dst_node) = node_map.get(successor) {
//                     graph.add_edge(src_node, dst_node, ());
//                 }
//             }
//         }
//     }

//     // 深度优先搜索并记录逆后序遍历
//     let mut visited = HashSet::new();
//     let mut post_order = Vec::new();

//     fn dfs(
//         node: NodeIndex,
//         graph: &Graph<(), (), Directed>,
//         visited: &mut HashSet<NodeIndex>,
//         post_order: &mut Vec<NodeIndex>,
//     ) {
//         if visited.contains(&node) {
//             return;
//         }
//         visited.insert(node);

//         for neighbor in graph.neighbors_directed(node, petgraph::Direction::Outgoing) {
//             dfs(neighbor, graph, visited, post_order);
//         }

//         post_order.push(node);
//     }

//     // 找到 entry 块并开始 DFS
//     if let Some(entry_id) = control_flow_graph.entry {
//         if let Some(&entry_node) = node_map.get(&entry_id) {
//             dfs(entry_node, &graph, &mut visited, &mut post_order);
//         }
//     }

//     // 将 post_order 转换为 Block 顺序
//     let mut sorted_blocks = Vec::new();
//     for &node in post_order.iter().rev() {
//         if let Some(block_id) = node_map
//             .iter()
//             .find_map(|(&k, &v)| if v == node { Some(k) } else { None })
//         {
//             if let Some(block) = control_flow_graph.blocks.iter().find(|b| b.id == block_id) {
//                 sorted_blocks.push(block.clone());
//             }
//         }
//     }

//     sorted_blocks
// }

pub struct Module {
    pub name: Name,
    pub constants: IndexSet<Primitive>,
    pub instructions: Instructions,
}

impl Module {
    pub fn new(
        name: impl Into<Name>,
        constants: IndexSet<Primitive>,
        instructions: Instructions,
    ) -> Self {
        Self {
            name: name.into(),
            constants,
            instructions,
        }
    }
}
impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, constant) in self.constants.iter().enumerate() {
            writeln!(f, "#{i:08}:\t {constant:?}")?;
        }

        for (i, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "{i:08}:\t {instruction}")?;
        }

        Ok(())
    }
}
