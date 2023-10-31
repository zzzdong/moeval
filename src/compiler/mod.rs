mod exprrewriter;
mod irbuilder;
pub(crate) mod irmodule;
mod virtregrewriter;

use crate::{
    compiler::{
        exprrewriter::ExprRewriter, irbuilder::IRBuilder, irmodule::IRModule,
        virtregrewriter::VirtRegRewriter,
    },
    instruction::{Module, Register},
    parser::Parser,
    Error,
};
use log::debug;

trait ModuleRewriter {
    fn rewrite(&mut self, module: Module) -> Module;
}

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

    pub fn compile_ir(&self, source: &str) -> Result<IRModule, Error> {
        let expr = Parser::parse(source)?;

        debug!("expr:\n {:?}", expr);

        let module = irmodule::IRBuilder::build_expr(expr);
        Ok(module)
    }
}
