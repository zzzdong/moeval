use crate::{
    error::Error,
    irbuilder::{IRBuilder, VirtRegRewriter},
    opcode::{Instruction, OpCode, Operand, Instructions},
    parser::Parser,
    value::Value,
    vm::{Environment, Register, Vm},
};

pub struct Interpreter {
    vm: Vm,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { vm: Vm::new() }
    }

    pub fn eval(source: &str, env: &Environment) ->Result<Value, Error> {
        let mut interpreter = Interpreter::new();
        interpreter.run(source, env)
    }

    pub fn run(&mut self, source: &str, env: &Environment) -> Result<Value, Error> {
        let expr = Parser::parse(source)?;

        let mut compiler = IRBuilder::new();
        compiler.build_expr(expr);

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

        let mut instructions = rewriter.rewrite(compiler.instructions());

        match instructions.last() {
            Some(inst) => {
                if let Operand::Register(r) = inst.operand0 {
                    instructions.push(Instruction::two(
                        OpCode::Move,
                        Operand::Register(Register::R0),
                        Operand::Register(r),
                    ))
                }
            }
            None => {
                return Ok(Value::Null);
            }
        }

        self.vm.execute(instructions, env)?;

        Ok(self.vm.get_register(Register::R0))
    }
}


#[cfg(test)]
mod test {
    use super::*;


    #[test]
    fn test_interpreter() {
        let source = r#"1+2+3+4+5+6+7+8+9"#;

        let mut env = Environment::new();

        let ret = Interpreter::eval(source, &env).unwrap();

        assert_eq!(ret, Value::Integer(45));
    }
}