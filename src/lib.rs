mod ast;
mod compiler;
mod error;
mod eval;
mod instruction;
mod interpreter;
mod lexer;
mod parser;
mod value;
mod vm;

pub use error::Error;
pub use value::Value;
pub use vm::Environment;
pub use vm::Vm;

pub fn eval(input: &str, env: &Environment) -> Result<Value, error::Error> {
    crate::interpreter::Interpreter::eval(input, env)
}

#[cfg(test)]
mod tests {
    use super::{eval, Environment, Value};

    #[test]
    fn test_eval() {
        let env = Environment::new();
        let result = eval("1 + 2", &env);
        assert_eq!(result.unwrap(), Value::Integer(3));
    }
}
