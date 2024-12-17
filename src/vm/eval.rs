use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::Deref;

use futures::future::BoxFuture;
use futures::FutureExt;

use super::object::*;
use super::value::{Value, ValueRef};
use super::RuntimeError;
use crate::compiler::Compiler;
use crate::error::Error;
use crate::ir::{
    BlockId, ControlFlowGraph, FunctionId, Inst, Instruction, Instructions, Module, Opcode, Operand,
};

pub struct Environment {
    symbols: HashMap<String, ValueRef>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: impl ToString, value: impl Into<Value>) {
        self.symbols
            .insert(name.to_string(), ValueRef::new(value.into()));
    }

    // pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    // where
    //     F: Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError> + 'static,
    // {
    //     let name = name.to_string();
    //     self.define(name.clone(), NativeFunction::new(name, Box::new(func)));
    // }

    pub fn define_function<Args: 'static>(
        &mut self,
        name: impl ToString,
        callable: impl Callable<Args>,
    ) {
        self.define(
            name.to_string(),
            NativeFunction::new(name, Box::new(callable.into_function())),
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        self.symbols.get(name.as_ref()).cloned()
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

    fn alloc(&mut self, addr: Operand) -> ValueRef {
        let index = addr.id().as_usize();

        if index >= self.values.len() {
            self.values
                .resize_with(index + 1, || ValueRef::new(Value::default()));
        }
        self.values[index].clone()
    }

    fn load_value(&self, addr: Operand) -> ValueRef {
        let index = addr.id().as_usize();
        self.values[index].clone()
    }

    fn store_value(&mut self, addr: Operand, value: ValueRef) {
        let index = addr.id().as_usize();
        self.values[index] = value;
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
        log::debug!("alloc frame");
        self.frames.push(StackFrame::new());
    }

    fn dealloc_frame(&mut self) {
        self.frames.pop();
    }

    fn top(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    fn top_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }

    fn load_value(&self, addr: Operand) -> ValueRef {
        self.top().unwrap().load_value(addr)
    }

    fn store_value(&mut self, addr: Operand, value: ValueRef) {
        self.top_mut().unwrap().store_value(addr, value);
    }

    fn load_argument(&self, index: usize) -> ValueRef {
        log::debug!(
            "stack_depth: {} load argument {:?}",
            self.frames.len(),
            index
        );
        self.top().unwrap().args[index].clone()
    }

    fn store_argument(&mut self, value: ValueRef) {
        log::debug!(
            "stack_depth: {} store argument: {:?}",
            self.frames.len(),
            value
        );
        self.top_mut().unwrap().args.push(value);
    }

    fn alloc(&mut self, addr: Operand) {
        self.top_mut().unwrap().alloc(addr);
    }
}

struct CallStack {
    next_pc: usize,
    result: Operand,
}

impl CallStack {
    pub fn new(next_pc: usize, result: Operand) -> Self {
        Self { next_pc, result }
    }
}

pub struct Interpreter {
    constants: Vec<ValueRef>,
    instructions: Instructions,
    env: Environment,
    stack: Stack,
    pc: usize,
    call_stack: Vec<CallStack>,
}

impl Interpreter {
    pub fn new(module: Module, env: Environment) -> Self {
        let Module {
            name,
            constants,
            instructions,
        } = module;

        let constants = constants
            .into_iter()
            .map(|v| ValueRef::new(Value::from_primitive(v.clone())))
            .collect();

        Self {
            constants,
            instructions,
            env,
            stack: Stack::new(),
            pc: 0,
            call_stack: Vec::new(),
        }
    }

    pub fn eval_script(script: &str, env: Environment) -> Result<Option<ValueRef>, Error> {
        let compiler = Compiler::new();

        let module = compiler.compile(script)?;

        log::debug!("module {module}");

        let mut interpreter = Interpreter::new(module, env);

        let ret = interpreter.run()?;

        Ok(ret)
    }

    pub fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        self.stack.alloc_frame();

        loop {
            let inst = self.instructions.get(self.pc);

            log::debug!("inst: {:04}-{inst:?}", self.pc);

            let inst = inst.clone();

            match inst {
                Instruction::Alloc { dst } => {
                    self.stack.alloc(dst);
                }
                Instruction::Store { dst, src } => {
                    let v = self.load_value(src)?;
                    self.stack.store_value(dst, v);
                }
                Instruction::LoadConst { dst, src } => {
                    let v = self.load_value(src)?;
                    self.stack.store_value(dst, v);
                }
                Instruction::LoadEnv {
                    dst: result,
                    ref name,
                } => {
                    let name = name
                        .as_constant()
                        .and_then(|const_id| self.constants.get(const_id.as_usize()))
                        .ok_or(RuntimeError::invalid_operand(*name))?;

                    match name.get().downcast_ref::<String>() {
                        Some(name) => {
                            let value = self
                                .env
                                .get(name)
                                .ok_or(RuntimeError::symbol_not_found(name))?;
                            self.stack.store_value(result, value);
                        }
                        _ => return Err(RuntimeError::invalid_operand(result)),
                    };
                }
                Instruction::LoadArg { index, dst } => {
                    let value = self.stack.load_argument(index);
                    self.stack.store_value(dst, value.clone());
                }
                Instruction::UnaryOp { op, dst, src } => {
                    let src = self.stack.load_value(src);
                    let src = src.get();
                    let src = src.deref();
                    match op {
                        Opcode::Neg | Opcode::Not => {
                            let ret = Object::negate(src)?;
                            self.stack.store_value(dst, ValueRef::new(ret));
                        }
                        _ => unreachable!("Invalid opcode"),
                    };
                }
                Instruction::BinaryOp {
                    op,
                    dst: result,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.stack.load_value(lhs);
                    let rhs = self.stack.load_value(rhs);

                    let lhs = lhs.get();
                    let rhs = rhs.get();

                    let lhs = lhs.deref();
                    // let rhs = rhs.deref();

                    let ret = match op {
                        Opcode::Add => Object::add(lhs, rhs)?,
                        Opcode::Sub => Object::sub(lhs, rhs)?,
                        Opcode::Mul => Object::mul(lhs, rhs)?,
                        Opcode::Div => Object::div(lhs, rhs)?,
                        Opcode::Mod => Object::modulo(lhs, rhs)?,
                        Opcode::Greater => {
                            Value::new(Object::compare(lhs, rhs)? == Ordering::Greater)
                        }
                        Opcode::GreaterEqual => {
                            let ord = Object::compare(lhs, rhs)?;
                            Value::new(ord == Ordering::Greater || ord == Ordering::Equal)
                        }
                        Opcode::Less => Value::new(Object::compare(lhs, rhs)? == Ordering::Less),
                        Opcode::LessEqual => {
                            let ord = Object::compare(lhs, rhs)?;
                            Value::new(ord == Ordering::Less || ord == Ordering::Equal)
                        }
                        Opcode::Equal => Value::new(Object::compare(lhs, rhs)? == Ordering::Equal),
                        Opcode::NotEqual => {
                            Value::new(Object::compare(lhs, rhs)? != Ordering::Equal)
                        }
                        Opcode::And => Object::logic_and(lhs, rhs)?,
                        Opcode::Or => Object::logic_or(lhs, rhs)?,
                        _ => unreachable!("unreachable"),
                    };

                    self.stack.store_value(result, ValueRef::new(ret));
                }
                Instruction::Range {
                    begin,
                    end,
                    bounded,
                    result,
                } => {
                    let value = Range::new(
                        self.stack.load_value(begin),
                        self.stack.load_value(end),
                        bounded,
                    )?;
                    self.stack
                        .store_value(result, ValueRef::new(Value::new(value)));
                }
                Instruction::Slice { dst, object, op } => {
                    let index = match op {
                        Opcode::Range { begin, end } => {
                            let begin = self.stack.load_value(begin);
                            let end = self.stack.load_value(end);
                            SliceIndex::range(begin, end)?
                        }
                        Opcode::RangeInclusive { begin, end } => {
                            let begin = self.stack.load_value(begin);
                            let end = self.stack.load_value(end);
                            SliceIndex::range_inclusive(begin, end)?
                        }
                        Opcode::RangeFull => SliceIndex::range_full(),
                        Opcode::RangeFrom { begin } => {
                            let begin = self.stack.load_value(begin);
                            SliceIndex::range_from(begin)?
                        }
                        Opcode::RangeTo { end } => {
                            let end = self.stack.load_value(end);
                            SliceIndex::range_to(end)?
                        }
                        Opcode::RangeToInclusive { end } => {
                            let end = self.stack.load_value(end);
                            SliceIndex::range_to_inclusive(end)?
                        }
                        _ => unreachable!("unreachable"),
                    };
                    let object = self.stack.load_value(object);
                    let object = object.get();
                    let object = object.deref();
                    let value = object.index_get(&Value::new(index))?;
                    self.stack.store_value(dst, value);
                }
                Instruction::MakeIterator { iter, result } => {
                    let mut iter = self.stack.load_value(iter);
                    let iter = iter.get_mut();

                    let iterator = iter.make_iterator()?;

                    self.stack
                        .store_value(result, ValueRef::new(Value::new(Enumerator::new(iterator))));
                }
                Instruction::IteratorHasNext { iter, result } => {
                    let mut iterator = self.stack.load_value(iter);
                    let iterator = iterator.get_mut();
                    let enumerator = iterator.try_downcast_mut::<Enumerator>()?;

                    let has_next = enumerator.iterator_has_next()?;

                    self.stack
                        .store_value(result, ValueRef::new(Value::new(has_next)));
                }
                Instruction::IterateNext { iter, next } => {
                    let mut iterator = self.stack.load_value(iter);
                    let iterator = iterator.get_mut();

                    let enumerator = iterator.try_downcast_mut::<Enumerator>()?;

                    let element = enumerator.iterate_next()?;
                    self.stack.store_value(next, element);
                }

                Instruction::NewArray { dst, size } => {
                    let arr: Vec<ValueRef> = match size {
                        Some(s) => Vec::with_capacity(s),
                        None => Vec::new(),
                    };

                    self.stack
                        .store_value(dst, ValueRef::new(Value::new(Array::new(arr))));
                }
                Instruction::ArrayPush { array, value } => {
                    let element = self.stack.load_value(value);
                    let mut arr = self.stack.load_value(array);
                    let arr = arr.get_mut();

                    let array = arr.try_downcast_mut::<Array>()?;

                    array.push(element);
                }
                Instruction::NewMap { dst } => {
                    self.stack
                        .store_value(dst, ValueRef::new(Value::new(Map::new())));
                }
                Instruction::IndexGet { dst, object, index } => {
                    let object = self.stack.load_value(object);
                    let object = object.get();
                    let index = self.stack.load_value(index);
                    let value = object.index_get(index.get())?;
                    self.stack.store_value(dst, value);
                }
                Instruction::IndexSet {
                    object,
                    index,
                    value,
                } => {
                    let mut object = self.stack.load_value(object);
                    let object = object.get_mut();
                    let index = self.stack.load_value(index);
                    let value = self.stack.load_value(value);
                    object.index_set(index.get(), value)?;
                }

                Instruction::Br { dst } => {
                    self.set_pc(dst);
                    continue;
                }
                Instruction::BrIf {
                    condition,
                    true_blk,
                    false_blk,
                } => {
                    let cond = self.stack.load_value(condition);
                    let cond = cond.get();
                    let cond = cond.try_downcast_ref::<bool>()?;

                    if *cond {
                        self.set_pc(true_blk);
                        continue;
                    } else {
                        self.set_pc(false_blk);
                        continue;
                    }
                }
                Instruction::Call { func, args, result } => match func {
                    Operand::Offset(offset) => {
                        self.call_function(func, &args, result)?;
                        continue;
                    }
                    Operand::Variable(_) => {
                        let mut func = self.stack.load_value(func);
                        let callable = func.get_mut();
                        if let Some(CallOffset(offset)) = callable.downcast_mut::<CallOffset>() {
                            self.call_function(Operand::Offset(*offset), &args, result)?;
                            continue;
                        } else if let Some(callable) =
                            func.get_mut().downcast_mut::<NativeFunction>()
                        {
                            let args: Vec<ValueRef> =
                                args.iter().map(|arg| self.stack.load_value(*arg)).collect();
                            let ret = callable.call(&args)?;
                            if let Some(ret) = ret {
                                self.stack.store_value(result, ret.into());
                            }
                        } else {
                            return Err(RuntimeError::symbol_not_found(
                                "function not found".to_string(),
                            ));
                        }
                    }
                    _ => {
                        unreachable!("unsupported call instruction {func:?}")
                    }
                },
                Instruction::PropertyCall {
                    object,
                    property,
                    args,
                    result,
                } => {
                    let mut object = self.stack.load_value(object);

                    let obj = object.get_mut();

                    let args: Vec<ValueRef> =
                        args.iter().map(|arg| self.stack.load_value(*arg)).collect();

                    self.stack.alloc_frame();

                    let ret = obj.property_call(&property, &args)?;

                    self.stack.dealloc_frame();
                    if let Some(ret) = ret {
                        self.stack.store_value(result, ValueRef::new(ret));
                    }
                }
                Instruction::Return { value } => {
                    let value = value.map(|v| self.stack.load_value(v));
                    if self.call_stack.is_empty() {
                        return Ok(value);
                    }
                    match self.call_stack.pop() {
                        Some(stack) => {
                            self.pc = stack.next_pc;
                            self.stack.dealloc_frame();
                            self.stack
                                .store_value(stack.result, value.unwrap_or_default());
                            continue;
                        }
                        None => {
                            return Err(RuntimeError::internal("call stack underflow").into());
                        }
                    }
                }
                Instruction::Await { promise, dst } => {
                    let promise = self.stack.load_value(promise);

                    let mut promise = promise.clone();

                    let (tx, rx) = tokio::sync::oneshot::channel();

                    tokio::spawn(async move {
                        let promise = promise.get_mut();
                        let promise = promise.try_downcast_mut::<Promise>().unwrap();
                        let ret = promise.await;
                        let _ = tx.send(ret);
                    });

                    match rx.blocking_recv() {
                        Ok(result) => {
                            self.stack.store_value(dst, result.into());
                        }
                        Err(e) => return Err(RuntimeError::internal("await failed")),
                    }
                }
                _ => unimplemented!("unimplemented: {inst:?}"),
            }

            self.pc += 1;
        }
    }

    fn call_function(
        &mut self,
        callee: Operand,
        args: &[Operand],
        result: Operand,
    ) -> Result<(), RuntimeError> {
        let args: Vec<ValueRef> = args.iter().map(|arg| self.stack.load_value(*arg)).collect();

        self.stack.alloc_frame();

        for arg in args {
            self.stack.store_argument(arg);
        }

        self.call_stack.push(CallStack::new(self.next_pc(), result));

        self.set_pc(callee);

        Ok(())
    }

    fn load_value(&self, addr: Operand) -> Result<ValueRef, RuntimeError> {
        match addr {
            Operand::Constant(const_id) => Ok(self.constants[const_id.as_usize()].clone()),
            Operand::Variable(_) => Ok(self.stack.load_value(addr)),
            Operand::Offset(offset) => Ok(ValueRef::new(CallOffset(offset).into())),
            _ => Err(RuntimeError::invalid_operand(addr)),
        }
    }

    fn set_pc(&mut self, operand: Operand) {
        self.pc = operand.as_offset().expect("expected offset branch target");
    }

    fn next_pc(&self) -> usize {
        self.pc + 1
    }
}

enum ControlFlow {
    Block(BlockId),
    Return(Option<ValueRef>),
}

pub struct Evaluator {
    constants: Vec<ValueRef>,
    stack: Stack,
    module: Inst,
    env: Environment,
    current_control_flow_graph: Option<ControlFlowGraph>,
}

impl Evaluator {
    pub fn new(module: Inst, env: Environment) -> Self {
        let constants = module
            .constants
            .iter()
            .map(|v| ValueRef::new(Value::from_primitive(v.clone())))
            .collect();

        Self {
            constants,
            stack: Stack::new(),
            module,
            env,
            current_control_flow_graph: None,
        }
    }

    pub fn eval_script(script: &str, env: Environment) -> Result<Option<ValueRef>, Error> {
        let compiler = Compiler::new();

        let module = compiler.compile(script)?;

        log::debug!("module {module}");

        unimplemented!();

        // let mut this = Evaluator::new(module, env);

        // let ret = this.eval()?;

        // Ok(ret)
    }

    fn eval(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        self.stack.alloc_frame();

        let control_flow_graph = self.module.control_flow_graph.clone();

        let entry = control_flow_graph.entry();

        self.switch_control_flow_graph(control_flow_graph);

        let ret = self.eval_blocks(entry)?;

        self.stack.dealloc_frame();

        Ok(ret)
    }

    fn switch_control_flow_graph(
        &mut self,
        control_flow_graph: ControlFlowGraph,
    ) -> Option<ControlFlowGraph> {
        std::mem::replace(
            &mut self.current_control_flow_graph,
            Some(control_flow_graph),
        )
    }

    fn eval_function(
        &mut self,
        func: FunctionId,
        args: Vec<ValueRef>,
    ) -> Result<Option<ValueRef>, RuntimeError> {
        self.stack.alloc_frame();

        for arg in args {
            self.stack.store_argument(arg);
        }
        let ret = self.eval_call(func)?;

        self.stack.dealloc_frame();

        Ok(ret)
    }

    fn load_value(&self, addr: Operand) -> Result<ValueRef, RuntimeError> {
        match addr {
            Operand::Constant(const_id) => Ok(self.constants[const_id.as_usize()].clone()),
            Operand::Variable(_) => Ok(self.stack.load_value(addr)),
            _ => Err(RuntimeError::invalid_operand(addr)),
        }
    }

    fn eval_block(&mut self, block: BlockId) -> Result<ControlFlow, RuntimeError> {
        let block = self
            .current_control_flow_graph
            .as_ref()
            .and_then(|cfg| cfg.get_block(block))
            .cloned()
            .ok_or(RuntimeError::internal("block not found"))?;

        let instructions = block.instructions.iter();

        for inst in instructions {
            let inst = inst.clone();

            log::debug!("inst: {inst:?}");

            match inst {
                Instruction::Alloc { dst } => {
                    self.stack.alloc(dst);
                }
                Instruction::Store { dst, src } => {
                    let v = self.load_value(src)?;
                    self.stack.store_value(dst, v);
                }
                Instruction::LoadConst { dst, src } => {
                    let v = self.load_value(src)?;
                    self.stack.store_value(dst, v);
                }
                Instruction::LoadEnv {
                    dst: result,
                    ref name,
                } => {
                    let name = name
                        .as_constant()
                        .and_then(|const_id| self.constants.get(const_id.as_usize()))
                        .ok_or(RuntimeError::invalid_operand(*name))?;

                    match name.get().downcast_ref::<String>() {
                        Some(name) => {
                            let value = self
                                .env
                                .get(name)
                                .ok_or(RuntimeError::symbol_not_found(name))?;
                            self.stack.store_value(result, value);
                        }
                        _ => return Err(RuntimeError::invalid_operand(result)),
                    };
                }
                Instruction::LoadArg { index, dst } => {
                    let value = self.stack.load_argument(index);
                    self.stack.store_value(dst, value.clone());
                }
                Instruction::UnaryOp { op, dst, src } => {
                    let src = self.stack.load_value(src);
                    let src = src.get();
                    let src = src.deref();
                    match op {
                        Opcode::Neg | Opcode::Not => {
                            let ret = Object::negate(src)?;
                            self.stack.store_value(dst, ValueRef::new(ret));
                        }
                        _ => unreachable!("Invalid opcode"),
                    };
                }
                Instruction::BinaryOp {
                    op,
                    dst: result,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.stack.load_value(lhs);
                    let rhs = self.stack.load_value(rhs);

                    let lhs = lhs.get();
                    let rhs = rhs.get();

                    let lhs = lhs.deref();
                    // let rhs = rhs.deref();

                    let ret = match op {
                        Opcode::Add => Object::add(lhs, rhs)?,
                        Opcode::Sub => Object::sub(lhs, rhs)?,
                        Opcode::Mul => Object::mul(lhs, rhs)?,
                        Opcode::Div => Object::div(lhs, rhs)?,
                        Opcode::Mod => Object::modulo(lhs, rhs)?,
                        Opcode::Greater => {
                            Value::new(Object::compare(lhs, rhs)? == Ordering::Greater)
                        }
                        Opcode::GreaterEqual => {
                            let ord = Object::compare(lhs, rhs)?;
                            Value::new(ord == Ordering::Greater || ord == Ordering::Equal)
                        }
                        Opcode::Less => Value::new(Object::compare(lhs, rhs)? == Ordering::Less),
                        Opcode::LessEqual => {
                            let ord = Object::compare(lhs, rhs)?;
                            Value::new(ord == Ordering::Less || ord == Ordering::Equal)
                        }
                        Opcode::Equal => Value::new(Object::compare(lhs, rhs)? == Ordering::Equal),
                        Opcode::NotEqual => {
                            Value::new(Object::compare(lhs, rhs)? != Ordering::Equal)
                        }
                        Opcode::And => Object::logic_and(lhs, rhs)?,
                        Opcode::Or => Object::logic_or(lhs, rhs)?,
                        _ => unreachable!("unreachable"),
                    };

                    self.stack.store_value(result, ValueRef::new(ret));
                }
                Instruction::Range {
                    begin,
                    end,
                    bounded,
                    result,
                } => {
                    let value = Range::new(
                        self.stack.load_value(begin),
                        self.stack.load_value(end),
                        bounded,
                    )?;
                    self.stack
                        .store_value(result, ValueRef::new(Value::new(value)));
                }
                Instruction::Slice { dst, object, op } => {
                    let index = match op {
                        Opcode::Range { begin, end } => {
                            let begin = self.stack.load_value(begin);
                            let end = self.stack.load_value(end);
                            SliceIndex::range(begin, end)?
                        }
                        Opcode::RangeInclusive { begin, end } => {
                            let begin = self.stack.load_value(begin);
                            let end = self.stack.load_value(end);
                            SliceIndex::range_inclusive(begin, end)?
                        }
                        Opcode::RangeFull => SliceIndex::range_full(),
                        Opcode::RangeFrom { begin } => {
                            let begin = self.stack.load_value(begin);
                            SliceIndex::range_from(begin)?
                        }
                        Opcode::RangeTo { end } => {
                            let end = self.stack.load_value(end);
                            SliceIndex::range_to(end)?
                        }
                        Opcode::RangeToInclusive { end } => {
                            let end = self.stack.load_value(end);
                            SliceIndex::range_to_inclusive(end)?
                        }
                        _ => unreachable!("unreachable"),
                    };
                    let object = self.stack.load_value(object);
                    let object = object.get();
                    let object = object.deref();
                    let value = object.index_get(&Value::new(index))?;
                    self.stack.store_value(dst, value);
                }
                Instruction::MakeIterator { iter, result } => {
                    let mut iter = self.stack.load_value(iter);
                    let iter = iter.get_mut();

                    let iterator = iter.make_iterator()?;

                    self.stack
                        .store_value(result, ValueRef::new(Value::new(Enumerator::new(iterator))));
                }
                Instruction::IteratorHasNext { iter, result } => {
                    let mut iterator = self.stack.load_value(iter);
                    let iterator = iterator.get_mut();
                    let enumerator = iterator.try_downcast_mut::<Enumerator>()?;

                    let has_next = enumerator.iterator_has_next()?;

                    self.stack
                        .store_value(result, ValueRef::new(Value::new(has_next)));
                }
                Instruction::IterateNext { iter, next } => {
                    let mut iterator = self.stack.load_value(iter);
                    let iterator = iterator.get_mut();

                    let enumerator = iterator.try_downcast_mut::<Enumerator>()?;

                    let element = enumerator.iterate_next()?;
                    self.stack.store_value(next, element);
                }

                Instruction::NewArray { dst, size } => {
                    let arr: Vec<ValueRef> = match size {
                        Some(s) => Vec::with_capacity(s),
                        None => Vec::new(),
                    };

                    self.stack
                        .store_value(dst, ValueRef::new(Value::new(Array::new(arr))));
                }
                Instruction::ArrayPush { array, value } => {
                    let element = self.stack.load_value(value);
                    let mut arr = self.stack.load_value(array);
                    let arr = arr.get_mut();

                    let array = arr.try_downcast_mut::<Array>()?;

                    array.push(element);
                }
                Instruction::NewMap { dst } => {
                    self.stack
                        .store_value(dst, ValueRef::new(Value::new(Map::new())));
                }
                Instruction::IndexGet { dst, object, index } => {
                    let object = self.stack.load_value(object);
                    let object = object.get();
                    let index = self.stack.load_value(index);
                    let value = object.index_get(index.get())?;
                    self.stack.store_value(dst, value);
                }
                Instruction::IndexSet {
                    object,
                    index,
                    value,
                } => {
                    let mut object = self.stack.load_value(object);
                    let object = object.get_mut();
                    let index = self.stack.load_value(index);
                    let value = self.stack.load_value(value);
                    object.index_set(index.get(), value)?;
                }

                Instruction::Br { dst } => {
                    return Ok(ControlFlow::Block(
                        dst.as_block().ok_or(RuntimeError::invalid_operand(dst))?,
                    ));
                }
                Instruction::BrIf {
                    condition,
                    true_blk,
                    false_blk,
                } => {
                    let cond = self.stack.load_value(condition);
                    let cond = cond.get();
                    let cond = cond.try_downcast_ref::<bool>()?;

                    if *cond {
                        return Ok(ControlFlow::Block(
                            true_blk
                                .as_block()
                                .ok_or(RuntimeError::invalid_operand(true_blk))?,
                        ));
                    } else {
                        return Ok(ControlFlow::Block(
                            false_blk
                                .as_block()
                                .ok_or(RuntimeError::invalid_operand(false_blk))?,
                        ));
                    }
                }
                Instruction::Call { func, args, result } => match func {
                    Operand::Function(func_id) => {
                        self.call_function(func_id, args, result)?;
                    }
                    Operand::Variable(_) => {
                        let mut func = self.stack.load_value(func);
                        let callable = func.get_mut();
                        if let Some(UserFunction(id)) = callable.downcast_mut::<UserFunction>() {
                            self.call_function(*id, args, result)?;
                        } else if let Some(callable) =
                            func.get_mut().downcast_mut::<NativeFunction>()
                        {
                            let args: Vec<ValueRef> =
                                args.iter().map(|arg| self.stack.load_value(*arg)).collect();
                            let ret = callable.call(&args)?;
                            if let Some(ret) = ret {
                                self.stack.store_value(result, ret.into());
                            }
                        } else {
                            return Err(RuntimeError::symbol_not_found(
                                "function not found".to_string(),
                            ));
                        }
                    }
                    _ => {
                        unreachable!("unreachable")
                    }
                },
                Instruction::PropertyCall {
                    object,
                    property,
                    args,
                    result,
                } => {
                    let mut object = self.stack.load_value(object);

                    let obj = object.get_mut();

                    let args: Vec<ValueRef> =
                        args.iter().map(|arg| self.stack.load_value(*arg)).collect();

                    self.stack.alloc_frame();

                    let ret = obj.property_call(&property, &args)?;

                    self.stack.dealloc_frame();
                    if let Some(ret) = ret {
                        self.stack.store_value(result, ValueRef::new(ret));
                    }
                }
                Instruction::Return { value } => {
                    let value = value.map(|v| self.stack.load_value(v));
                    return Ok(ControlFlow::Return(value));
                }
                Instruction::Await { promise, dst } => {
                    let promise = self.stack.load_value(promise);

                    let mut promise = promise.clone();

                    let (tx, rx) = tokio::sync::oneshot::channel();

                    tokio::spawn(async move {
                        let promise = promise.get_mut();
                        let promise = promise.try_downcast_mut::<Promise>().unwrap();
                        let ret = promise.await;
                        let _ = tx.send(ret);
                    });

                    match rx.blocking_recv() {
                        Ok(result) => {
                            self.stack.store_value(dst, result.into());
                        }
                        Err(e) => return Err(RuntimeError::internal("await failed")),
                    }
                }
                _ => unimplemented!("unimplemented: {inst:?}"),
            }
        }

        Ok(ControlFlow::Return(None))
    }

    fn call_function(
        &mut self,
        id: FunctionId,
        args: Vec<Operand>,
        result: Operand,
    ) -> Result<(), RuntimeError> {
        let args: Vec<ValueRef> = args.iter().map(|arg| self.stack.load_value(*arg)).collect();

        let ret = self.eval_function(id, args)?;

        self.stack
            .store_value(result, ret.unwrap_or(ValueRef::new(Value::default())));

        Ok(())
    }

    fn eval_call(&mut self, func: FunctionId) -> Result<Option<ValueRef>, RuntimeError> {
        let func = self
            .module
            .get_function(func)
            .ok_or(RuntimeError::symbol_not_found("function not found"))?;
        let mut next_block = func.control_flow_graph.entry();

        let old = self.switch_control_flow_graph(func.control_flow_graph.clone());

        while let Some(block) = next_block.take() {
            match self.eval_block(block)? {
                ControlFlow::Block(next) => {
                    next_block = Some(next);
                }
                ControlFlow::Return(value) => {
                    self.switch_control_flow_graph(old.expect("missing control flow graph"));
                    return Ok(value);
                }
            }
        }

        self.switch_control_flow_graph(old.expect("missing control flow graph"));

        Ok(None)
    }

    fn eval_blocks(&mut self, entry: Option<BlockId>) -> Result<Option<ValueRef>, RuntimeError> {
        let mut next_block = entry;

        while let Some(block) = next_block.take() {
            match self.eval_block(block)? {
                ControlFlow::Block(next) => {
                    next_block = Some(next);
                }
                ControlFlow::Return(value) => {
                    return Ok(value);
                }
            }
        }

        Ok(None)
    }
}
