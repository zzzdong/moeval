use indexmap::IndexSet;

use super::builder::ControlFlowGraph;
use super::instruction::*;
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
    pub(crate) predecessors: Vec<BlockId>,
    pub(crate) successors: Vec<BlockId>,
}

impl Block {
    pub fn new(id: BlockId, label: impl Into<Name>) -> Self {
        Self {
            id,
            label: label.into(),
            instructions: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn append_successors(&mut self, successor: BlockId) {
        self.successors.push(successor);
    }

    pub fn append_predecessor(&mut self, predecessor: BlockId) {
        self.predecessors.push(predecessor);
    }
}

#[derive(Debug, Clone, Default)]
pub struct FlowGraph {
    blocks: Vec<BlockId>,
    current_block: Option<BlockId>,
}

impl FlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            current_block: None,
        }
    }

    pub fn entry(&self) -> Option<BlockId> {
        self.blocks.first().copied()
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    pub(crate) fn add_block(&mut self, blk_id: BlockId) {
        self.blocks.push(blk_id)
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

// pub struct FunctionContext {
//     pub values: Vec<Address>,
//     pub flow_graph: FlowGraph,
// }

// impl Default for FunctionContext {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl FunctionContext {
//     pub fn new() -> Self {
//         Self {
//             values: Vec::new(),
//             flow_graph: FlowGraph::new(),
//         }
//     }
// }

#[derive(Debug)]
pub struct Module {
    pub constants: IndexSet<Primitive>,
    pub functions: Vec<Function>,
    pub control_flow_graph: ControlFlowGraph,
}

impl Module {
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

    pub fn define_function(&mut self, id: FunctionId, func_control_flow_graph: ControlFlowGraph) {
        let func = &mut self.functions[id.as_usize()];

        let _ = std::mem::replace(&mut func.control_flow_graph, func_control_flow_graph);
    }

    pub fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.control_flow_graph.blocks.get(id.as_usize())
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Module {
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

        // for func in self.functions.iter() {
        //     writeln!(
        //         f,
        //         "function[{}]@{}()",
        //         func.id.as_usize(),
        //         func.signature.name
        //     )?;
        //     for block in func.flow_graph.blocks.iter() {
        //         let block = self.get_block(*block).unwrap();
        //         writeln!(f, "block#{}@{}:", block.id.as_usize(), block.label)?;
        //         for instruction in block.instructions.iter() {
        //             writeln!(f, "\t{}", instruction)?;
        //         }
        //     }
        // }

        Ok(())
    }
}
