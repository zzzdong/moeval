use std::collections::BTreeMap;

use crate::{Value, irbuilder::{Instruction, IRModule, ValueRef}, Error};






#[derive(Debug, Default)]
pub struct CompilerOption {

}

pub struct Compiler {
    opt: CompilerOption,
    units: Vec<IRModule>,
}



impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            opt: CompilerOption::default(),
            units: Vec::new(),
        }
    }


    pub fn compile(&mut self, source: &str) -> Result<Program, Error> {
        unimplemented!()
    }
}