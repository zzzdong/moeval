use log::debug;

use crate::{
    instruction::{Instruction, Module, OpCode, Operand},
    instruction::{Register, StackSlot, VirtReg},
    parser::Parser,
    Error, Value,
};

use super::{
    exprrewriter::ExprRewriter, irbuilder::IRBuilder, virtregrewriter::VirtRegRewriter,
    ModuleRewriter,
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&self, source: &str) -> Result<Module, Error> {
        let expr = Parser::parse(source)?;

        debug!("before:\n {:?}", expr);
        // simplify expr
        let rewriter = ExprRewriter::new();
        let expr = rewriter.rewrite(expr);
        debug!("after:\n {:?}", expr);

        let module = IRBuilder::build_expr(expr);

        debug!("before VirtRegRewriter:\n {}", module);
        let mut rewriter = VirtRegRewriter::new(&[
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
        ]);

        let module = rewriter.rewrite(module);

        debug!("after VirtRegRewriter:\n {}", module);

        Ok(module)
    }
}
