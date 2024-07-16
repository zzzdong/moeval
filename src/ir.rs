mod builder;
mod instruction;
mod types;

pub use builder::Builder;
pub use instruction::{Address, Instruction, Opcode};
pub use types::{BlockId, FuncParam, FuncSignature, FunctionId, Module, Primitive};
