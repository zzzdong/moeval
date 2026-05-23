use crate::compiler::Compiler;
use crate::runtime::{Environment, VM};
use crate::{Error, Value};

pub fn eval(script: &str, env: Environment) -> Result<Option<Value>, Error> {
    let compiler = Compiler::new();
    let module = compiler.compile(script, &env)?;

    let mut vm = VM::new(module, env);

    let ret = vm.run()?;

    Ok(ret.map(|v| v.take()))
}

pub struct Interpreter {}

impl Interpreter {
    pub fn eval(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();
        let module = compiler.compile(script, &env)?;

        let mut vm = VM::new(module, env);

        let ret = vm.run()?;

        Ok(ret.map(|v| v.take()))
    }
}