use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::ops::{Add, Deref, DerefMut};

use indexmap::IndexMap;

use crate::error::{Error, RuntimeError};
use crate::ir::builder::{Block, FlowGraph, Function, Module};
use crate::ir::{BlockId, FunctionId};
// use crate::object::{NativeFunction, Value, ValueRef};
use crate::{
    compiler::Compiler,
    ir::instruction::{Address, Instruction, Opcode},
};

pub struct Environment {
    symbols: HashMap<String, ValueRef>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: impl ToString, value: impl Into<Value>) {
        self.symbols.insert(name.to_string(), ValueRef::new(value.into()));
    }

    pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    where
        F: Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError> + 'static,
    {
        let name = name.to_string();
        self.define(name.clone(), NativeFunction::new(name, Box::new(func)));
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&ValueRef> {
        self.symbols.get(name.as_ref())
    }
}

#[derive(Debug, Default)]
struct StackFrame {
    pub values: Vec<ValueRef>,
    pub args: Vec<ValueRef>,
}

impl StackFrame {
    fn new() -> Self {
        Self {
            values: Vec::with_capacity(16),
            args: vec![],
        }
    }

    fn alloc(&mut self, addr: Address) -> ValueRef {
        match addr {
            Address::Stack(index) => {
                if index >= self.values.len() {
                    self.values
                        .resize_with(index + 1, || ValueRef::new(Value::default()));
                }
                self.values[index].clone()
            }
            _ => unreachable!("Invalid address"),
        }
    }

    fn load_value(&self, addr: Address) -> ValueRef {
        match addr {
            Address::Stack(index) => self.values[index].clone(),
            Address::Function(func) => ValueRef::new(Value::Function(func)),
        }
    }

    fn store_value(&mut self, addr: Address, value: ValueRef) {
        match addr {
            Address::Stack(index) => self.values[index] = value,
            _ => unreachable!("Invalid address"),
        }
    }
}

#[derive(Debug)]
struct Stack {
    frames: Vec<StackFrame>,
}

impl Stack {
    fn new() -> Self {
        Self { frames: vec![] }
    }

    fn alloc_frame(&mut self) {
        self.frames.push(StackFrame::new());
    }

    fn exit_frame(&mut self) {
        self.frames.pop();
    }

    fn top(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    fn top_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }

    fn load_value(&self, addr: Address) -> ValueRef {
        self.top().unwrap().load_value(addr)
    }

    fn store_value(&mut self, addr: Address, value: ValueRef) {
        self.top_mut().unwrap().store_value(addr, value);
    }

    fn load_argument(&self, index: usize) -> ValueRef {
        self.top().unwrap().args[index].clone()
    }

    fn store_argument(&mut self, value: ValueRef) {
        self.top_mut().unwrap().args.push(value);
    }

    fn alloc(&mut self, addr: Address) {
        self.top_mut().unwrap().alloc(addr);
    }
}

enum ControlFlow {
    Block(BlockId),
    Return(Option<ValueRef>),
}

struct Context<'a> {
    constants: Vec<ValueRef>,
    stack: Stack,
    module: &'a Module,
    env: &'a Environment,
}

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, script: &str, env: &Environment) -> Result<Option<Value>, Error> {
        let module = Compiler::compile(script)?;

        log::debug!("module {module}");

        let constants = module
            .constants
            .iter()
            .map(|v| ValueRef::new(v.clone().into()))
            .collect();

        let mut context = Context {
            constants,
            stack: Stack::new(),
            module: &module,
            env,
        };

        let entry = module
            .entry
            .ok_or(RuntimeError::invalid_operation("No entry point"))?;

        let func = module
            .get_function(entry)
            .ok_or(RuntimeError::invalid_operation("Function not found"))?;

        self.eval_function(&mut context, func)
            .map(|r| r.map(|v| v.take()))
    }

    fn eval_function(
        &mut self,
        ctx: &mut Context,
        func: &Function,
    ) -> Result<Option<ValueRef>, RuntimeError> {
        ctx.stack.alloc_frame();
        self.eval_call(ctx, func)
    }

    fn eval_block(
        &mut self,
        block: &Block,
        ctx: &mut Context,
    ) -> Result<ControlFlow, RuntimeError> {
        let mut instructions = block.instructions.iter();

        while let Some(inst) = instructions.next() {
            let inst = inst.clone();

            log::debug!("inst: {inst:?}");

            match inst {
                Instruction::Alloc { dst } => {
                    ctx.stack.alloc(dst);
                }
                Instruction::Store { dst, src } => {
                    let v = ctx.stack.load_value(src);
                    ctx.stack.store_value(dst, v);
                }
                Instruction::LoadConst { dst, src } => {
                    let v = ctx.constants[src.as_usize()].clone();
                    ctx.stack.store_value(dst, v);
                }
                Instruction::LoadEnv {
                    dst: result,
                    ref name,
                } => match ctx.env.get(name) {
                    Some(value) => ctx.stack.store_value(result, value),
                    None => {
                        return Err(RuntimeError::symbol_not_found(name));
                    }
                },
                Instruction::LoadArg { index, dst } => {
                    let value = ctx.stack.load_argument(index);
                    ctx.stack.store_value(dst, value.clone());
                }
                Instruction::BinaryOp {
                    op,
                    dst: result,
                    lhs,
                    rhs,
                } => {
                    let lhs = ctx.stack.load_value(lhs);
                    let rhs = ctx.stack.load_value(rhs);

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
                    };

                    ctx.stack.store_value(result, ret.map(ValueRef::new)?);
                }

                Instruction::Range { begin, end, result } => {
                    let value = Range::new(ctx.stack.load_value(begin), ctx.stack.load_value(end))?;
                    ctx.stack
                        .store_value(result, ValueRef::new(Value::Range(value)));
                }
                Instruction::MakeIterator { iter, result } => {
                    let iter = ctx.stack.load_value(iter);
                    let mut iter = iter.borrow_mut();

                    match iter.iterator()? {
                        Some(iterator) => {
                            ctx.stack
                                .store_value(result, ValueRef::new(Value::Iterator(iterator)));
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
                    let iterator = ctx.stack.load_value(iter);
                    let mut iterator = iterator.borrow_mut();

                    match iterator.deref_mut() {
                        Value::Iterator(iterator) => match iterator.next() {
                            Some(element) => {
                                ctx.stack.store_value(next, element);
                            }
                            None => {
                                return Ok(ControlFlow::Block(after_blk));
                            }
                        },
                        _ => {
                            return Err(RuntimeError::invalid_operation(
                                "iterate next: not iterator",
                            ))
                        }
                    }
                }
                Instruction::NewArray { dst, size } => {
                    let arr: Vec<ValueRef> = match size {
                        Some(s) => Vec::with_capacity(s),
                        None => Vec::new(),
                    };

                    ctx.stack.store_value(dst, ValueRef::new(Value::Array(arr)));
                }
                Instruction::ArrayPush { array, value } => {
                    let element = ctx.stack.load_value(value);
                    let arr = ctx.stack.load_value(array);
                    let mut arr = arr.borrow_mut();
                    match arr.deref_mut() {
                        Value::Array(array) => {
                            array.push(element);
                        }
                        _ => {
                            return Err(RuntimeError::invalid_operation("array push: not array"));
                        }
                    }
                }
                Instruction::NewMap { dst } => {
                    let map = Value::Map(IndexMap::new());
                    ctx.stack.store_value(dst, ValueRef::new(map));
                }
                Instruction::IndexGet { dst, object, index } => {
                    let object = ctx.stack.load_value(object);
                    let object = object.borrow();
                    let index = ctx.stack.load_value(index);
                    let element = object.index_get(index)?;
                    ctx.stack.store_value(dst, element);
                }
                Instruction::IndexSet {
                    object,
                    index,
                    value,
                } => {
                    let object = ctx.stack.load_value(object);
                    let mut object = object.borrow_mut();
                    let index = ctx.stack.load_value(index);
                    let value = ctx.stack.load_value(value);
                    object.index_set(index, value)?;
                }
                Instruction::Br { dst } => {
                    return Ok(ControlFlow::Block(dst));
                }
                Instruction::BrIf {
                    condition,
                    true_blk,
                    false_blk,
                } => {
                    let cond = ctx.stack.load_value(condition);
                    let cond = cond.borrow();
                    match *cond {
                        Value::Boolean(b) => {
                            if b {
                                return Ok(ControlFlow::Block(true_blk));
                            } else {
                                return Ok(ControlFlow::Block(false_blk));
                            }
                        }
                        _ => {
                            return Err(RuntimeError::invalid_operation(
                                "BrIf condition must be boolean",
                            ))
                        }
                    }
                }
                Instruction::Call { func, args, result } => match func {
                    Address::Function(id) => {
                        self.call_function(ctx, id, args, result)?;
                    }
                    Address::Stack(_) => {
                        match ctx.stack.load_value(func).borrow_mut().deref_mut() {
                            Value::Function(id) => {
                                self.call_function(ctx, *id, args, result)?;
                            }
                            callable => {
                                let args: Vec<ValueRef> =
                                    args.iter().map(|arg| ctx.stack.load_value(*arg)).collect();

                                ctx.stack.alloc_frame();

                                let ret = callable.call(&args)?;

                                ctx.stack.exit_frame();
                                if let Some(ret) = ret {
                                    ctx.stack.store_value(result, ValueRef::new(ret));
                                }
                            }
                        }
                    }
                },
                Instruction::Return { value } => {
                    let value = value.map(|v| ctx.stack.load_value(v));
                    return Ok(ControlFlow::Return(value));
                }

                Instruction::PropertyCall {
                    object,
                    property,
                    args,
                    result,
                } => {
                    let object = ctx.stack.load_value(object);

                    let mut obj = object.borrow_mut();

                    match obj.deref_mut() {
                        Value::Object(object) => {
                            let args: Vec<ValueRef> =
                                args.iter().map(|arg| ctx.stack.load_value(*arg)).collect();

                            ctx.stack.alloc_frame();

                            let ret = object.property_call(&property, &args)?;

                            ctx.stack.exit_frame();
                            if let Some(ret) = ret {
                                ctx.stack.store_value(result, ValueRef::new(ret));
                            }
                        }
                        Value::Array(arr) => match property.as_str() {
                            "push" => {
                                let element = ctx.stack.load_value(args[0]);
                                arr.push(element);
                            }
                            "len" => {
                                ctx.stack.store_value(
                                    result,
                                    ValueRef::new(Value::Integer(arr.len() as i64)),
                                );
                            }
                            "enumerate" => {
                                let arr: Vec<ValueRef> = arr
                                    .clone()
                                    .into_iter()
                                    .enumerate()
                                    .map(|(i, ele)| {
                                        Value::Tuple(vec![Value::Integer(i as i64).into(), ele])
                                            .into()
                                    })
                                    .collect();
                                ctx.stack.store_value(
                                    result,
                                    ValueRef::new(Value::Array(Vec::from(arr))),
                                );
                            }
                            _ => {
                                return Err(RuntimeError::invalid_operation(
                                    "array property call: not implemented",
                                ))
                            }
                        },
                        _ => {
                            return Err(RuntimeError::invalid_operation(
                                "property call: not implemented",
                            ))
                        }
                    }
                }

                _ => unimplemented!("unimplemented: {inst:?}"),
            }
        }

        Ok(ControlFlow::Return(None))
    }

    fn call_function(
        &mut self,
        ctx: &mut Context,
        id: FunctionId,
        args: Vec<Address>,
        result: Address,
    ) -> Result<(), RuntimeError> {
        let func = ctx
            .module
            .get_function(id)
            .ok_or(RuntimeError::invalid_operation("Function not found"))?;

        let args: Vec<ValueRef> = args.iter().map(|arg| ctx.stack.load_value(*arg)).collect();

        ctx.stack.alloc_frame();

        for arg in args {
            ctx.stack.store_argument(arg);
        }

        let ret = self.eval_call(ctx, func)?;

        ctx.stack.exit_frame();

        ctx.stack
            .store_value(result, ret.unwrap_or(ValueRef::new(Value::default())));

        Ok(())
    }

    fn eval_call(
        &mut self,
        ctx: &mut Context,
        func: &Function,
    ) -> Result<Option<ValueRef>, RuntimeError> {
        let mut next_block = func.get_entry_block();

        while let Some(block) = next_block.take() {
            match self.eval_block(block, ctx)? {
                ControlFlow::Block(next) => {
                    next_block = func.get_block(next);
                }
                ControlFlow::Return(value) => {
                    return Ok(value);
                }
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Trace)
            .is_test(true)
            .try_init();
    }

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
            .join("");

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
    fn test_eval_for() {
        // init();

        let mut env = Environment::new();
        let mut eval = Evaluator::new();
        env.define_function("println", println);

        let script = r#"

        let arr = [1, 2, 3, 4, 5];
        for ele in arr {
            println("->", ele);
        }

        for (i, ele) in arr.enumerate() {
            println("[", i, "]->", i);
        }

        let map = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
        for (k, v) in map {
            println(k, "->", v);
        }
        "#;

        let retval = eval.eval(script, &env);

        assert!(retval.is_ok());
    }

    #[test]
    fn test_eval_array() {
        let mut env = Environment::new();
        let mut eval = Evaluator::new();

        env.define_function("println", println);

        let script = r#"
        let sum = 0;
        let array = [1, 2, 3, 4, 5];

        println("array.len = ", array.len());

        for (i, ele) in array.enumerate() {
            println("array[", i, "]=", ele);
        }

        for i in array {
            sum += i;
        }

        println("sum = ", sum);
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap().unwrap();

        assert_eq!(retval, Value::Integer(15));
    }

    #[test]
    fn test_eval_map() {
        let env = Environment::new();
        let mut eval = Evaluator::new();

        let script = r#"
        let sum = 0;
        let map = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
        for ele in map {
            sum += ele[1];
        }
        sum += map["a"];
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap().unwrap();

        assert_eq!(retval, Value::Integer(16));
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

        println!("ret: {:?}", retval);
    }

    #[test]
    fn test_eval() {
        init();

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

        let f = fib;

        let sum = 0;
        for i in 1..=10 {
            sum += fib(i);
        }
        
        println("-->", sum);
        return sum;
        "#;

        let retval = eval.eval(script, &env).unwrap();

        println!("ret: {:?}", retval);
    }
}
