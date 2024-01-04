use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum ValueRef {
    /// Constants value, immediate value
    Constant(usize),
    /// Variable reference
    Variable(usize),
    /// Value from instruction
    Inst(usize),
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(usize);

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    /// Add
    Add,
    /// Sub
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    Member,
}

#[derive(Debug)]
pub enum Instruction {
    BinaryOp {
        op: Opcode,
        result: ValueRef,
        lhs: ValueRef,
        rhs: ValueRef,
    },
    LoadArg {
        index: usize,
        result: ValueRef,
    },
    Call {
        func: ValueRef,
        args: Vec<ValueRef>,
        result: ValueRef,
    },
    Block {
        block_id: BlockId,
        instruction_index: usize,
    },
    LoadEnv {
        result: ValueRef,
        name: String,
    },
    PropertyGet {
        result: ValueRef,
        object: ValueRef,
        property: String,
    },
    PropertySet {
        object: ValueRef,
        property: String,
        value: ValueRef,
    },
    PropertyCall {
        object: ValueRef,
        property: String,
        args: Vec<ValueRef>,
    },
    Assign {
        object: ValueRef,
        value: ValueRef,
    }
}

#[derive(Debug)]
pub struct Module {
    instructions: Vec<Instruction>,
    blocks: BTreeMap<BlockId, Block>,
    entry: Option<usize>,

    variables: BTreeMap<String, usize>,
    constants: Vec<crate::value::Value>,
    inst_values: Vec<ValueRef>,
    // functions: BTreeMap<String, Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            blocks: BTreeMap::new(),
            entry: None,
            variables: BTreeMap::new(),
            constants: Vec::new(),
            inst_values: Vec::new(),
        }
    }

    pub fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions.push(Instruction::BinaryOp {
            op,
            result,
            lhs,
            rhs,
        });

        result
    }

    pub fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.instructions.push(Instruction::Assign { object, value })
    }

    pub fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions.push(Instruction::PropertyGet {
            result,
            object,
            property: property.to_string(),
        });

        result
    }

    pub fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.instructions.push(Instruction::PropertySet {
            object,
            property: property.to_string(),
            value,
        });
    }

    pub fn call_property(&mut self, object: ValueRef, property: String, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions.push(Instruction::PropertyCall { object, property, args  });

        result
    }

    pub fn load_external_variable(&mut self, name: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions.push(Instruction::LoadEnv {
            result,
            name: name.to_string(),
        });

        result
    }

    pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        let index = self.constants.len();
        self.constants.push(value);
        ValueRef::Constant(index)
    }

    pub fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.instructions.push(Instruction::Call {
            result,
            func,
            args,
        });

        result
    }

    pub fn make_variable(&mut self, name: &str) -> ValueRef {
        let idx = self.variables.len();
        let idx = self.variables.entry(name.to_owned()).or_insert(idx);
        ValueRef::Variable(*idx)
    }

    pub fn make_inst_value(&mut self) -> ValueRef {
        let idx: usize = self.inst_values.len();
        self.inst_values.push(ValueRef::Inst(idx));
        ValueRef::Inst(idx)
    }

    pub fn debug(&self) {
        println!("constants:");
        for (i, c) in self.constants.iter().enumerate() {
            println!("  {}: {:?}", i, c);
        }
        println!("variables:");
        for (name, idx) in self.variables.iter() {
            println!("  {}: {}", name, idx);
        }
        println!("instructions:");
        for i in 0..self.instructions.len() {
            println!("  {}: {:?}", i, self.instructions[i]);
        }
    }
}

pub struct Function {
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Block {
    instructions: Vec<Instruction>,
}
