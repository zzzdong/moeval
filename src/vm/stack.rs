use super::{CallLocation, Operand, Value, ValueRef};

#[derive(Debug)]
pub struct Stack {
    frames: Vec<StackFrame>,
}

impl Stack {
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    pub fn alloc_frame(&mut self) {
        self.frames.push(StackFrame::new());
    }

    pub fn dealloc_frame(&mut self) {
        self.frames.pop();
    }

    fn top(&self) -> Option<&StackFrame> {
        self.frames.last()
    }

    fn top_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames.last_mut()
    }

    pub fn load_value(&self, addr: Operand) -> ValueRef {
        self.top().unwrap().load_value(addr)
    }

    pub fn store_value(&mut self, addr: Operand, value: ValueRef) {
        self.top_mut().unwrap().store_value(addr, value);
    }

    pub fn load_argument(&self, index: usize) -> ValueRef {
        self.top().unwrap().args[index].clone()
    }

    pub fn store_argument(&mut self, value: ValueRef) {
        self.top_mut().unwrap().args.push(value);
    }

    pub fn alloc(&mut self, addr: Operand) {
        self.top_mut().unwrap().alloc(addr);
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
        match addr {
            Operand::Variable(var) => {
                let index = var.as_usize();
                self.values[index].clone()
            }
            Operand::Location(loc) => ValueRef::new(CallLocation::new(loc).into()),
            _ => unreachable!(),
        }
    }

    fn store_value(&mut self, addr: Operand, value: ValueRef) {
        let index = addr.id().as_usize();
        self.values[index] = value;
    }
}
