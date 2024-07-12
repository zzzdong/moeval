mod builder;
mod instruction;
mod types;

pub use builder::Builder;
pub use instruction::{Address, Instruction, Opcode};
pub use types::{
    Block, BlockId, FuncParam, FuncSignature, Function, FunctionId, Module, Primitive,
};
