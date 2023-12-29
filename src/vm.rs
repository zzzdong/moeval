use std::collections::HashMap;

use crate::value::Value;

pub struct Environment {
    symbols: HashMap<String, Value>,
}

#[derive(Debug, Default)]
struct StackFrame {
    symbols: HashMap<String, Value>,
}

struct Stack {
    frames: Vec<StackFrame>,
}

impl Stack {
    fn new() -> Self {
        Self {
            frames: vec![StackFrame::default()],
        }
    }

    fn get_symbol(&mut self, name: &str) -> Option<&mut Value> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(value) = frame.symbols.get_mut(name) {
                return Some(value);
            }
        }

        None
    }

    fn define_symbol(&mut self, name: String, value: Value) {
        self.frames.last_mut().unwrap().symbols.insert(name, value);
    }

    fn push(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }

    fn pop(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    fn top(&self) -> Option<&StackFrame> {
        self.frames.last()
    }
}

pub struct Evaluator {
    stack: Stack,
    retval: Option<Value>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            retval: None,
        }
    }

    pub fn eval(&mut self, script: &str) -> Option<Value> {
        unimplemented!()
    }
}
