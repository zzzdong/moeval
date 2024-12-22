use std::{cmp::Ordering, ops::Deref};

use super::{eval::Stack, Environment, Value, ValueRef};
use crate::{compiler::Compiler, ir::{Instruction, Instructions, Module, Opcode, Operand}, vm::object::{CallLocation, Enumerator, Range, SliceIndex}, Array, Error, Map, NativeFunction, Object, Promise, RuntimeError};

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
        futures::executor::block_on(Self::eval_script_async(script, env))
    }

    pub async fn eval_script_async(
        script: &str,
        env: Environment,
    ) -> Result<Option<ValueRef>, Error> {
        let compiler = Compiler::new();

        let module = compiler.compile(script)?;

        log::trace!("module {module}");

        let mut interpreter = Interpreter::new(module, env);

        let ret = interpreter.run().await?;

        Ok(ret)
    }

    pub async fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        self.stack.alloc_frame();

        loop {
            let inst = match self.instructions.get(self.pc).cloned() {
                Some(inst) => inst,
                None => return Ok(None),
            };

            log::trace!("running: {:08}-{inst:?}", self.pc);

            match inst {
                Instruction::Halt => return Ok(None),
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
                    Operand::Location(_location) => {
                        self.call_function(func, &args, result)?;
                        continue;
                    }
                    Operand::Variable(_) => {
                        let mut func = self.stack.load_value(func);
                        let callable = func.get_mut();
                        if let Some(CallLocation(loc)) = callable.downcast_mut::<CallLocation>()
                        {
                            self.call_function(Operand::Location(*loc), &args, result)?;
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
                            println!("variable({:?}) is not a function", func);
                            return Err(RuntimeError::internal(format!(
                                "variable({:?}) is not a function",
                                func
                            )));
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
                    let mut promise = self.stack.load_value(promise);
                    let promise = promise.get_mut();
                    let promise = promise.try_downcast_mut::<Promise>().unwrap();
                    let ret = promise.await;
                    self.stack.store_value(dst, ret.into());
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
            Operand::Location(offset) => Ok(ValueRef::new(CallLocation(offset).into())),
            _ => Err(RuntimeError::invalid_operand(addr)),
        }
    }

    fn set_pc(&mut self, operand: Operand) {
        self.pc = operand
            .as_location()
            .expect("expected offset branch target");
    }

    fn next_pc(&self) -> usize {
        self.pc + 1
    }
}
