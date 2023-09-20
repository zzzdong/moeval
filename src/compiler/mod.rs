mod compiler;
mod exprrewriter;
mod irbuilder;
mod virtregrewriter;

pub use compiler::Compiler;

use crate::instruction::Module;

trait ModuleRewriter {
    fn rewrite(&mut self, module: Module) -> Module;
}
