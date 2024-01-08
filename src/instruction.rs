use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy)]
pub enum ValueRef {
    /// Constants value, immediate value
    Constant(usize),
    /// Variable reference
    Variable(usize),
    /// Value from instruction
    Inst(usize),
    /// Function
    Function(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    },
    Return {
        value: Option<ValueRef>,
    },
}

#[derive(Debug)]
pub struct Module {
    blocks: BTreeMap<BlockId, Block>,
    entry: Option<BlockId>,

    variables: BTreeMap<String, usize>,
    constants: Vec<crate::value::Value>,
    inst_values: Vec<ValueRef>,
    functions: Vec<Function>,
    current_block: Option<BlockId>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            blocks: BTreeMap::new(),
            entry: None,
            variables: BTreeMap::new(),
            constants: Vec::new(),
            inst_values: Vec::new(),
            current_block: None,
            functions: Vec::new(),
        }
    }

    pub fn create_block(&mut self, label: Option<impl ToString>) -> BlockId {
        let block_id = BlockId(self.blocks.len());
        self.blocks.insert(
            block_id,
            Block {
                label: label.map(|s| s.to_string()),
                instructions: Vec::new(),
            },
        );
        block_id
    }

    pub fn switch_to_block(&mut self, block_id: BlockId) -> InstBuilder<'_> {
        self.current_block = Some(block_id);
        InstBuilder {
            module: self,
            block: block_id,
        }
    }

    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    pub fn ins(&mut self) -> InstBuilder<'_> {
        let block = self.current_block.unwrap();
        InstBuilder {
            module: self,
            block,
        }
    }

    // pub fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::BinaryOp {
    //         op,
    //         result,
    //         lhs,
    //         rhs,
    //     });

    //     result
    // }

    // pub fn assign(&mut self, object: ValueRef, value: ValueRef) {
    //     self.instructions.push(Instruction::Assign { object, value })
    // }

    // pub fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::PropertyGet {
    //         result,
    //         object,
    //         property: property.to_string(),
    //     });

    //     result
    // }

    // pub fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
    //     self.instructions.push(Instruction::PropertySet {
    //         object,
    //         property: property.to_string(),
    //         value,
    //     });
    // }

    // pub fn call_property(&mut self, object: ValueRef, property: String, args: Vec<ValueRef>) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::PropertyCall { object, property, args  });

    //     result
    // }

    // pub fn load_external_variable(&mut self, name: String) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::LoadEnv {
    //         result,
    //         name: name,
    //     });

    //     result
    // }

    // pub fn load_argument(&mut self, index: usize) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::LoadArg {
    //         result,
    //         index,
    //     });

    //     result
    // }

    // pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
    //     let index = self.constants.len();
    //     self.constants.push(value);
    //     ValueRef::Constant(index)
    // }

    // pub fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
    //     let result = self.make_inst_value();

    //     self.instructions.push(Instruction::Call {
    //         result,
    //         func,
    //         args,
    //     });

    //     result
    // }

    // pub fn make_variable(&mut self, name: &str) -> ValueRef {
    //     let idx = self.variables.len();
    //     let idx = self.variables.entry(name.to_owned()).or_insert(idx);
    //     ValueRef::Variable(*idx)
    // }

    // pub fn make_inst_value(&mut self) -> ValueRef {
    //     let idx: usize = self.inst_values.len();
    //     self.inst_values.push(ValueRef::Inst(idx));
    //     ValueRef::Inst(idx)
    // }

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
        for block in self.blocks.values() {
            println!("{:?}", block.label);
            for inst in &block.instructions {
                println!("  {:?}", inst);
            }
            println!("");
        }
    }
}

pub struct InstBuilder<'a> {
    module: &'a mut Module,
    block: BlockId,
}

impl<'a> InstBuilder<'a> {
    fn current_block(&mut self) -> &mut Block {
        self.module.blocks.get_mut(&self.block).unwrap()
    }

    pub fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::BinaryOp {
                op,
                result,
                lhs,
                rhs,
            });

        result
    }

    pub fn assign(&mut self, object: ValueRef, value: ValueRef) {
        self.current_block()
            .instructions
            .push(Instruction::Assign { object, value })
    }

    pub fn get_property(&mut self, object: ValueRef, property: &str) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::PropertyGet {
                result,
                object,
                property: property.to_string(),
            });

        result
    }

    pub fn set_property(&mut self, object: ValueRef, property: &str, value: ValueRef) {
        self.current_block()
            .instructions
            .push(Instruction::PropertySet {
                object,
                property: property.to_string(),
                value,
            });
    }

    pub fn call_property(
        &mut self,
        object: ValueRef,
        property: String,
        args: Vec<ValueRef>,
    ) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::PropertyCall {
                object,
                property,
                args,
            });

        result
    }

    pub fn load_external_variable(&mut self, name: String) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::LoadEnv { result, name: name });

        result
    }

    pub fn load_argument(&mut self, index: usize) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::LoadArg { result, index });

        result
    }

    pub fn make_call(&mut self, func: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        let result = self.make_inst_value();

        self.current_block()
            .instructions
            .push(Instruction::Call { result, func, args });

        result
    }

    pub fn return_(&mut self, value: Option<ValueRef>) {
        self.current_block()
            .instructions
            .push(Instruction::Return { value });
    }

    pub fn make_constant(&mut self, value: crate::value::Value) -> ValueRef {
        let index = self.module.constants.len();
        self.module.constants.push(value);
        ValueRef::Constant(index)
    }

    pub fn make_variable(&mut self, name: &str) -> ValueRef {
        let idx = self.module.variables.len();
        let idx = self.module.variables.entry(name.to_owned()).or_insert(idx);
        ValueRef::Variable(*idx)
    }

    pub fn make_inst_value(&mut self) -> ValueRef {
        let idx: usize = self.module.inst_values.len();
        self.module.inst_values.push(ValueRef::Inst(idx));
        ValueRef::Inst(idx)
    }

    pub fn make_function(&mut self, entry: BlockId) -> ValueRef {
        let idx = self.module.functions.len();
        self.module.functions.push(Function { entry });
        ValueRef::Function(idx)
    }
}

#[derive(Debug)]
pub struct Function {
    entry: BlockId,
}

#[derive(Debug)]
pub struct Block {
    label: Option<String>,
    instructions: Vec<Instruction>,
}
