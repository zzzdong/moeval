use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use log::debug;

use crate::value::{Range, ValueRef};
use crate::{
    compiler::Compiler,
    instruction::{ControlFlowGraph, Instruction, Module, Opcode, ValueId},
    value::Value,
};

#[derive(Debug)]
pub enum RuntimeError {
    InvalidOperation(String),
    SymbolNotFound(String),
}

impl RuntimeError {
    pub fn symbol_not_found(name: impl ToString) -> Self {
        RuntimeError::SymbolNotFound(name.to_string())
    }

    pub fn invalid_operation(msg: impl Into<String>) -> Self {
        RuntimeError::InvalidOperation(msg.into())
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            RuntimeError::SymbolNotFound(name) => write!(f, "Symbol not found: {}", name),
        }
    }
}

impl std::error::Error for RuntimeError {}

pub struct Environment {
    symbols: HashMap<String, ValueRef>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: impl ToString, value: Value) {
        self.symbols.insert(name.to_string(), ValueRef::new(value));
    }

    pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    where
        F: Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError> + 'static,
    {
        self.define(name, Value::Function(Box::new(func)));
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        self.symbols.get(name.as_ref()).cloned()
    }
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

    fn load_value(&self, index: ValueId) -> ValueRef {
        self.top().unwrap().variables.load_value(index)
    }

    fn store_value(&mut self, index: ValueId, value: ValueRef) {
        self.top_mut().unwrap().variables.store_value(index, value);
    }

    fn load_argument(&self, index: usize) -> ValueRef {
        self.top().unwrap().variables.load_arg(index)
    }

    fn store_argument(&mut self, value: ValueRef) {
        self.top_mut().unwrap().variables.store_arg(value);
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

    pub fn eval(&mut self, script: &str, env: &Environment) -> Result<Option<Value>, RuntimeError> {
        let module = Compiler::compile(script).unwrap();

        // module.debug();

        let dfg = module.control_flow_graph();

        let variables = Variables::from_cfg(dfg);
        self.stack.push_frame(StackFrame { variables });

        self.eval_function(dfg, &module, env)
            .map(|r| r.map(|v| v.take()))
    }

    pub fn eval_function(
        &mut self,
        dfg: &ControlFlowGraph,
        module: &Module,
        env: &Environment,
    ) -> Result<Option<ValueRef>, RuntimeError> {
        let mut next_block = dfg.entry();
        while let Some(block) = next_block.take() {
            for inst in dfg.instructions(block) {
                debug!("-> {inst:?}");
                match inst {
                    Instruction::LoadEnv { result, name } => match env.get(name) {
                        Some(value) => self.stack.store_value(*result, value),
                        None => {
                            return Err(RuntimeError::symbol_not_found(name));
                        }
                    },
                    Instruction::Br { target } => {
                        next_block = Some(*target);
                        break;
                    }
                    Instruction::BrIf {
                        condition,
                        then_blk,
                        else_blk,
                    } => {
                        let cond = self.stack.load_value(*condition);
                        let cond = cond.borrow();
                        match *cond {
                            Value::Boolean(b) => {
                                if b {
                                    next_block = Some(*then_blk);
                                    break;
                                } else {
                                    next_block = Some(*else_blk);
                                    break;
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
                        let lhs = self.stack.load_value(*lhs);
                        let rhs = self.stack.load_value(*rhs);

                        let lhs = lhs.borrow();
                        let rhs = rhs.borrow();

                        let lhs = lhs.deref();
                        let rhs = rhs.deref();

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

                        self.stack.store_value(*result, ret.map(ValueRef::new)?);
                    }
                    Instruction::Call { func, args, result } => match func {
                        ValueId::Function(id) => {
                            let func = module.get_function(*id);

                            let args: Vec<ValueRef> =
                                args.iter().map(|arg| self.stack.load_value(*arg)).collect();

                            self.stack.push_frame(StackFrame {
                                variables: Variables::from_cfg(func),
                            });

                            for arg in args {
                                self.stack.store_argument(arg);
                            }
                            let ret = self.eval_function(func, module, env)?;

                            self.stack.pop_frame();

                            self.stack.store_value(*result, ret.unwrap_or_default());
                        }
                        ValueId::Inst(_) => {
                            let args: Vec<ValueRef> =
                                args.iter().map(|arg| self.stack.load_value(*arg)).collect();

                            let func = self.stack.load_value(*func);
                            let mut func = func.borrow_mut();

                            if let Some(ret) = func.call(&args)? {
                                self.stack.store_value(*result, ValueRef::new(ret));
                            }
                        }
                        _ => unreachable!("must call a function"),
                    },
                    Instruction::Return { value } => {
                        let value = value.map(|v| self.stack.load_value(v));
                        return Ok(value);
                    }
                    Instruction::Store { object, value } => {
                        let value = self.stack.load_value(*value);
                        self.stack.store_value(*object, value.clone());
                    }
                    Instruction::LoadArg { index, result } => {
                        let value = self.stack.load_argument(*index);
                        self.stack.store_value(*result, value.clone());
                    }
                    Instruction::Range { begin, end, result } => {
                        let value =
                            Range::new(self.stack.load_value(*begin), self.stack.load_value(*end))?;
                        self.stack
                            .store_value(*result, ValueRef::new(Value::Range(value)));
                    }
                    Instruction::MakeIterator { iter, result } => {
                        let iter = self.stack.load_value(*iter);
                        let mut iter = iter.borrow_mut();

                        match iter.iterator()? {
                            Some(iterator) => {
                                self.stack
                                    .store_value(*result, ValueRef::new(Value::Iterator(iterator)));
                            }
                            None => {
                                return Err(RuntimeError::invalid_operation(
                                    "make iterator: not iterable",
                                ));
                            }
                        }
                    }
                    Instruction::IterateNext {
                        iter,
                        next,
                        after_blk,
                    } => {
                        let iterator = self.stack.load_value(*iter);
                        let mut iterator = iterator.borrow_mut();

                        match iterator.deref_mut() {
                            Value::Iterator(iterator) => match iterator.next() {
                                Some(element) => {
                                    self.stack.store_value(*next, element);
                                }
                                None => {
                                    next_block = Some(*after_blk);
                                    break;
                                }
                            },
                            _ => {
                                return Err(RuntimeError::invalid_operation(
                                    "iterate next: not iterator",
                                ))
                            }
                        }
                    }
                    Instruction::NewArray { array, size } => {
                        let arr: Vec<ValueRef> = match size {
                            Some(s) => Vec::with_capacity(*s),
                            None => Vec::new(),
                        };

                        self.stack
                            .store_value(*array, ValueRef::new(Value::Array(arr)));
                    }
                    Instruction::ArrayPush { array, value } => {
                        let element = self.stack.load_value(*value);
                        let arr = self.stack.load_value(*array);
                        let mut arr = arr.borrow_mut();
                        match arr.deref_mut() {
                            Value::Array(array) => {
                                array.push(element);
                            }
                            _ => {
                                return Err(RuntimeError::invalid_operation(
                                    "array push: not array",
                                ));
                            }
                        }
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
    constants: Vec<ValueRef>,
    inst_value: Vec<ValueRef>,
    stack_value: Vec<ValueRef>,
}

impl Variables {
    fn new(
        constants: Vec<ValueRef>,
        inst_value: Vec<ValueRef>,
        stack_value: Vec<ValueRef>,
    ) -> Self {
        Self {
            constants,
            inst_value,
            stack_value,
        }
    }

    fn store_arg(&mut self, arg: ValueRef) {
        self.stack_value.push(arg);
    }

    fn load_arg(&self, index: usize) -> ValueRef {
        self.stack_value[index].clone()
    }

    fn from_cfg(dfg: &ControlFlowGraph) -> Self {
        let mut values = Vec::new();
        for _ in 0..dfg.inst_values.len() {
            values.push(ValueRef::new(Value::default()));
        }

        let constants = dfg
            .constants
            .iter()
            .cloned()
            .map(|v| ValueRef::new(v.into()))
            .collect();

        Variables::new(constants, values, Vec::new())
    }

    fn load_value(&self, index: ValueId) -> ValueRef {
        match index {
            ValueId::Constant(i) => self.constants[i].clone(),
            ValueId::Inst(i) => self.inst_value[i].clone(),
            _ => unimplemented!(),
        }
    }

    fn store_value(&mut self, index: ValueId, value: ValueRef) {
        match index {
            ValueId::Constant(i) => self.constants[i] = value,
            ValueId::Inst(i) => self.inst_value[i] = value,
            _ => unimplemented!(),
        }
    }
}

pub trait Operate {
    fn add(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn sub(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn mul(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn div(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn modulo(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn pow(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn lt(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn gt(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn le(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn ge(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn eq(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn ne(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn and(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn or(&self, other: &Value) -> Result<Value, RuntimeError>;
    fn not(&self) -> Result<Value, RuntimeError>;

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn call_member(
        &mut self,
        member: &str,
        this: ValueRef,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn iterator(&mut self) -> Result<Option<Box<dyn Iterator<Item = ValueRef>>>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fib(args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        let n = args[0].borrow();

        let n: i64 = n.deref().try_into()?;

        if n < 1 {
            return Ok(Some(0.into()));
        }
        if n <= 2 {
            return Ok(Some(1.into()));
        }

        let a = fib(&[ValueRef::new((n - 1).into())])?.unwrap();
        let b = fib(&[ValueRef::new((n - 2).into())])?.unwrap();

        Ok(Some(a + b))
    }

    fn println(args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        let s = args
            .iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", s);

        Ok(None)
    }

    #[test]
    fn test_eval_for_range() {
        let env = Environment::new();
        let mut eval = Evaluator::new();

        let script = r#"
        let sum = 0;
        for i in 0..=10 {
            sum += i;
        }
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap().unwrap();

        assert_eq!(retval, Value::Integer(55));
    }

    #[test]
    fn test_eval_array() {
        let env = Environment::new();
        let mut eval = Evaluator::new();

        let script = r#"
        let sum = 0;
        let array = [1, 2, 3, 4, 5];
        for i in array {
            sum += i;
        }
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap().unwrap();

        assert_eq!(retval, Value::Integer(15));
    }

    #[test]
    fn test_eval_env() {
        let mut env = Environment::new();
        let mut eval = Evaluator::new();

        env.define_function("fib", fib);

        let script = r#"
        let sum = 0;
        for i in 1..=10 {
            sum += fib(i);
        }
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap();

        println!("{:?}", retval);
    }

    #[test]
    fn test_eval() {
        let mut env = Environment::new();
        let mut eval = Evaluator::new();

        env.define_function("println", println);

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

        let sum = 0;
        for i in 1..=10 {
            sum += fib(i);
        }
        
        println("-->", sum);
        "#;

        let retval = eval.eval(script, &env).unwrap();

        println!("ret: {:?}", retval);
    }
}
