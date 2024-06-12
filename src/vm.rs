use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;

use crate::{
    compiler::Compiler,
    instruction::{ControlFlowGraph, Instruction, Module, Opcode, ValueRef},
    value::{Range, Value},
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
    symbols: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: impl ToString, value: Value) {
        self.symbols.insert(name.to_string(), value);
    }

    pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    where
        F: Fn(&[Value]) -> Result<Option<Value>, RuntimeError> + 'static,
    {
        self.define(name, Value::Function(Rc::new(RefCell::new(Box::new(func)))));
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&Value> {
        self.symbols.get(name.as_ref())
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

    fn get_value(&self, index: ValueRef) -> &Value {
        self.top().unwrap().variables.get_value(index)
    }

    fn get_value_mut(&mut self, index: ValueRef) -> &mut Value {
        self.top_mut().unwrap().variables.get_value_mut(index)
    }

    fn take_value(&mut self, index: ValueRef) -> Value {
        self.top_mut().unwrap().variables.take_value(index)
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

    pub fn eval(&mut self, script: &str, env: &Environment) -> Result<Option<Value>, RuntimeError> {
        let module = Compiler::compile(script).unwrap();

        // module.debug();

        let dfg = module.control_flow_graph();

        let variables = Variables::from_cfg(dfg);
        self.stack.push_frame(StackFrame { variables });

        self.eval_function(dfg, &module, env)
    }

    pub fn eval_function(
        &mut self,
        dfg: &ControlFlowGraph,
        module: &Module,
        env: &Environment,
    ) -> Result<Option<Value>, RuntimeError> {
        let mut next_block = dfg.entry();
        while let Some(block) = next_block.take() {
            for inst in dfg.instructions(block) {
                debug!("-> {inst:?}");
                match inst {
                    Instruction::LoadEnv { result, name } => match env.get(name) {
                        Some(value) => self.stack.set_value(*result, value.clone()),
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
                        let cond = self.stack.get_value(*condition);
                        match cond {
                            Value::Boolean(b) => {
                                if *b {
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
                                .map(|arg| self.stack.get_value(*arg))
                                .cloned()
                                .collect();

                            self.stack.push_frame(StackFrame {
                                variables: Variables::from_cfg(func),
                            });

                            for arg in args {
                                // println!("on call, arg: {:?}", &arg);
                                self.stack.insert_argument(arg.clone());
                            }
                            let ret = self.eval_function(func, module, env)?;
                            // println!("on call, ret: {:?}", &ret);

                            self.stack.pop_frame();

                            self.stack.set_value(*result, ret.unwrap_or_default());
                        }
                        ValueRef::Inst(_) => {
                            let args: Vec<Value> = args
                                .iter()
                                .map(|arg| self.stack.get_value(*arg))
                                .cloned()
                                .collect();

                            let func = self.stack.get_value_mut(*func);

                            if let Some(ret) = func.call(&args)? {
                                self.stack.set_value(*result, ret);
                            }
                        }
                        _ => unreachable!("must call a function"),
                    },
                    Instruction::Return { value } => {
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
                    Instruction::Range { begin, end, result } => {
                        let value = Range::new(
                            self.stack.get_value(*begin).clone(),
                            self.stack.get_value(*end).clone(),
                        )?;
                        self.stack.set_value(*result, Value::Range(value));
                    }
                    Instruction::MakeIterator { iter, result } => {
                        let iter = self.stack.get_value(*iter);

                        match iter.iterator()? {
                            Some(iterator) => {
                                self.stack.set_value(*result, Value::Iterator(iterator));
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
                        let iterator = self.stack.get_value_mut(*iter);

                        match iterator {
                            Value::Iterator(iterator) => match iterator.next() {
                                Some(element) => {
                                    self.stack.set_value(*next, element);
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
                        let arr: Vec<Value> = match size {
                            Some(s) => Vec::with_capacity(*s),
                            None => Vec::new(),
                        };

                        self.stack.set_value(*array, Value::Array(arr));
                    }
                    Instruction::ArrayPush { array, value } => {
                        let element = self.stack.take_value(*value);
                        let arr = self.stack.get_value_mut(*array);
                        match arr {
                            Value::Array(array) => {
                                array.push(element);
                            }
                            _ => {
                                println!("=> {arr:?}");
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
            ValueRef::Constant(i) => &self.constants[i],
            ValueRef::Inst(i) => &self.inst_value[i],
            _ => unimplemented!(),
        }
    }

    fn set_value(&mut self, index: ValueRef, value: Value) {
        match index {
            ValueRef::Constant(i) => self.constants[i] = value,
            ValueRef::Inst(i) => self.inst_value[i] = value,
            _ => unimplemented!(),
        }
    }

    fn get_value_mut(&mut self, index: ValueRef) -> &mut Value {
        match index {
            ValueRef::Constant(i) => &mut self.constants[i],
            ValueRef::Inst(i) => &mut self.inst_value[i],
            _ => unimplemented!(),
        }
    }

    fn take_value(&mut self, index: ValueRef) -> Value {
        match index {
            ValueRef::Inst(i) => {
                std::mem::replace(self.inst_value.get_mut(i).unwrap(), Value::Undefined)
            }
            ValueRef::Constant(i) => self.constants[i].clone(),
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

    fn call(&mut self, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn call_member(
        &mut self,
        member: &str,
        this: &mut Value,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn iterator(&self) -> Result<Option<Box<dyn Iterator<Item = Value>>>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }

    fn iterate_next(&mut self) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation("unimplement"))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn fib(args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        let n = args[0].clone();
        if n < 1 {
            return Ok(Some(0.into()));
        }
        if n <= 2 {
            return Ok(Some(1.into()));
        }

        let a = fib(&[n.clone() - 1])?.unwrap();
        let b = fib(&[n - 2])?.unwrap();

        Ok(Some(a + b))
    }

    fn println(args: &[Value]) -> Result<Option<Value>, RuntimeError> {
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

        let retval = eval.eval(script, &env).unwrap();

        assert_eq!(retval, Some(Value::Integer(55)));
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

        let retval = eval.eval(script, &env).unwrap();

        assert_eq!(retval, Some(Value::Integer(15)));
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
        
        println(sum);
        "#;

        let retval = eval.eval(script, &env).unwrap();

        println!("{:?}", retval);
    }
}
