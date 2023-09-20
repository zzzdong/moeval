use std::collections::HashMap;

use crate::{
    instruction::{Instruction, Module, OpCode, Operand},
    instruction::{Register, StackOffset, VirtReg},
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

        println!("before: {:?}", expr);
        // simplify expr
        let rewriter = ExprRewriter::new();
        let expr = rewriter.rewrite(expr);

        println!("after: {:?}", expr);

        let module = IRBuilder::build_expr(expr);

        println!("before VirtRegRewriter: {}", module);
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

        println!("after VirtRegRewriter: {:?}", module);

        Ok(module)
    }
}
