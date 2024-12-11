mod builder;
mod instruction;
mod types;

pub use builder::{FunctionBuilder, InstBuilder, ModuleBuilder, ControlFlowGraph};
pub use instruction::{Instruction, Opcode, Variable};
pub use types::{BlockId, FuncParam, FuncSignature, Function, FunctionId, Module, Name, Primitive};
