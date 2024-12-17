mod builder;
mod instpass;
mod instruction;
mod types;

pub use builder::{ControlFlowGraph, FunctionBuilder, InstBuilder, ModuleBuilder};
pub use instpass::{InstPass, InstPassManager, SimplifyPass};
pub use instruction::{Instruction, Opcode, Operand};
pub use types::{
    BlockId, FuncParam, FuncSignature, Function, FunctionId, Inst, Instructions, Module, Name,
    Primitive,
};
