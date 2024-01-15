use std::collections::HashMap;

use crate::{
    compiler::Compiler,
    instruction::{BlockId, ControlFlowGraph, Instruction, Module, Opcode, ValueRef},
    value::Value,
};

#[derive(Debug)]
pub enum RuntimeError {
    InvalidOperation(String),
}

impl RuntimeError {
    pub fn invalid_operation(msg: impl Into<String>) -> Self {
        RuntimeError::InvalidOperation(msg.into())
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
        }
    }
}

impl std::error::Error for RuntimeError {}


pub struct Environment {
    symbols: HashMap<String, Value>,
}

#[derive(Debug, Default)]
struct StackFrame {
    pub variables: Variables,
}

#[derive(Debug)]
struct Stack {
    frames: Vec<StackFrame>,
}

impl Stack {
    fn new() -> Self {
        Self { frames: vec![] }
    }

    fn push_frame(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    fn top(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    fn top_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }

    fn get_value(&self, index: ValueRef) -> &Value {
        self.top().unwrap().variables.get_value(index)
    }

    fn set_value(&mut self, index: ValueRef, value: Value) {
        self.top_mut().unwrap().variables.set_value(index, value);
    }

    fn get_argument(&self, index: usize) -> Value {
        self.top().unwrap().variables.get_arg(index)
    }

    fn insert_argument(&mut self, value: Value) {
        self.top_mut().unwrap().variables.insert_arg(value);
    }
}

pub struct Evaluator {
    stack: Stack,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
        }
    }

    pub fn eval(&mut self, script: &str) -> Result<Option<Value>, RuntimeError> {
        let module = Compiler::compile(script).unwrap();

        module.debug();

        let dfg = module.control_flow_graph();

        let variables = Variables::from_cfg(dfg);
        self.stack.push_frame(StackFrame { variables });

        return self.eval_function(dfg, &module);
    }

    pub fn eval_function(
        &mut self,
        dfg: &ControlFlowGraph,
        module: &Module,
    ) -> Result<Option<Value>, RuntimeError> {
        let mut next_block = dfg.entry();
        while let Some(block) = next_block.take() {
            for inst in dfg.instructions(block) {
                // println!("-> {:?}", inst);
                match inst {
                    Instruction::Br { target } => {
                        next_block = Some(*target);
                        break;
                    }
                    Instruction::BrIf {
                        condition,
                        then_blk,
                        else_blk,
                    } => {
                        let cond = self.stack.get_value(*condition);
                        match cond {
                            Value::Boolean(b) => {
                                if *b {
                                    next_block = Some(*then_blk);
                                } else {
                                    next_block = Some(*else_blk);
                                }
                            }
                            _ => unreachable!("unsupported operate"),
                        }
                    }
                    Instruction::BinaryOp {
                        op,
                        result,
                        lhs,
                        rhs,
                    } => {
                        let lhs = self.stack.get_value(*lhs);
                        let rhs = self.stack.get_value(*rhs);
                        let ret = match op {
                            Opcode::Add => Operate::add(lhs, rhs),
                            Opcode::Sub => Operate::sub(lhs, rhs),
                            Opcode::Mul => Operate::mul(lhs, rhs),
                            Opcode::Div => Operate::div(lhs, rhs),
                            Opcode::Mod => Operate::modulo(lhs, rhs),
                            Opcode::Greater => Operate::gt(lhs, rhs),
                            Opcode::GreaterEqual => Operate::ge(lhs, rhs),
                            Opcode::Less => Operate::lt(lhs, rhs),
                            Opcode::LessEqual => Operate::le(lhs, rhs),
                            Opcode::Equal => Operate::eq(lhs, rhs),
                            Opcode::NotEqual => Operate::ne(lhs, rhs),
                            Opcode::And => Operate::and(lhs, rhs),
                            Opcode::Or => Operate::or(lhs, rhs),
                            _ => unreachable!("unsupported op {op:?}"),
                        };

                        self.stack.set_value(*result, ret?);
                    }
                    Instruction::Call { func, args, result } => match func {
                        ValueRef::Function(id) => {
                            let func = module.get_function(*id);

                            let args: Vec<Value> = args
                                .iter()
                                .map(|arg| self.stack.get_value(*arg).clone())
                                .collect();

                            self.stack.push_frame(StackFrame {
                                variables: Variables::from_cfg(func),
                            });

                            for arg in args {
                                // println!("on call, arg: {:?}", &arg);
                                self.stack.insert_argument(arg);
                            }
                            let ret = self.eval_function(func, module)?;
                            // println!("on call, ret: {:?}", &ret);

                            self.stack.pop_frame();

                            self.stack
                                .set_value(*result, ret.clone().unwrap_or_default());
                        }
                        _ => unreachable!("must call a function"),
                    },
                    Instruction::Return { value } => {
                        // println!("on return, value: {:?} {:?}", &value, self.stack.get_value(value.unwrap()));
                        let value = value.map(|v| self.stack.get_value(v)).cloned();
                        return Ok(value);
                    }
                    Instruction::Store { object, value } => {
                        let value = self.stack.get_value(*value);
                        self.stack.set_value(*object, value.clone());
                    }
                    Instruction::LoadArg { index, result } => {
                        let value = self.stack.get_argument(*index);
                        self.stack.set_value(*result, value.clone());
                    }
                    _ => unimplemented!("unimplemented: {inst:?}"),
                }
            }
        }
        Ok(None)
    }
}

#[derive(Debug, Default)]
struct Variables {
    constants: Vec<Value>,
    inst_value: Vec<Value>,
    stack_value: Vec<Value>,
}

impl Variables {
    fn new(constants: Vec<Value>, inst_value: Vec<Value>, stack_value: Vec<Value>) -> Self {
        Self {
            constants,
            inst_value,
            stack_value,
        }
    }

    fn insert_arg(&mut self, arg: Value) {
        self.stack_value.push(arg);
    }

    fn get_arg(&self, index: usize) -> Value {
        self.stack_value[index].clone()
    }

    fn from_cfg(dfg: &ControlFlowGraph) -> Self {
        let mut values = Vec::new();
        for _ in 0..dfg.inst_values.len() {
            values.push(Value::default());
        }

        Variables::new(dfg.constants.clone(), values, Vec::new())
    }

    fn get_value(&self, index: ValueRef) -> &Value {
        match index {
            ValueRef::Constant(i) => &self.constants[i as usize],
            ValueRef::Inst(i) => &self.inst_value[i as usize],
            _ => unimplemented!(),
        }
    }

    fn set_value(&mut self, index: ValueRef, value: Value) {
        match index {
            ValueRef::Constant(i) => self.constants[i as usize] = value,
            ValueRef::Inst(i) => self.inst_value[i as usize] = value,
            _ => unimplemented!(),
        }
    }

    fn get_value_mut(&mut self, index: ValueRef) -> &mut Value {
        match index {
            ValueRef::Constant(i) => &mut self.constants[i as usize],
            ValueRef::Inst(i) => &mut self.inst_value[i as usize],
            _ => unimplemented!(),
        }
    }
}

pub trait Operate
where
    Self: Sized,
{
    fn add(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn sub(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn mul(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn div(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn modulo(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn pow(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn lt(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn gt(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn le(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn ge(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn eq(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn ne(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn and(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn or(&self, other: &Self) -> Result<Self, RuntimeError>;
    fn not(&self) -> Result<Self, RuntimeError>;
    // fn call(&self, args: &[Self]) -> Result<Option<Self>, RuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let mut eval = Evaluator::new();

        let script = r#"
        fn fib(n) {
            if n < 1 {
                return 0;
            }
            if n <= 2 {
                return 1;
            }

            return fib(n - 1) + fib(n - 2);
        }

        return fib(20);
        "#;

        let retval = eval.eval(&script).unwrap();

        println!("{:?}", retval);
    }
}
