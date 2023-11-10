use std::collections::{HashMap, BTreeMap};

use crate::{error::Error, opcode::Opcode, opcode::Ops, value::Value, compiler::Program, irbuilder::{Instruction, InstructionData, ValueRef}};

pub struct Environment {
    map: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            map: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.map.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.map.get(name).cloned()
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.map.get_mut(name)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

pub struct Vm {
    pub registers: [Value; 8],
    pub stack: Vec<Value>,
    pub stack_pointer: usize,
    pub frame_pointer: usize,
    pub program_counter: usize,
    pub values: BTreeMap<ValueRef, Value>,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            registers: [
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
                Value::Null,
            ],
            stack: Vec::with_capacity(4 * 1024),
            stack_pointer: 0,
            frame_pointer: 0,
            program_counter: 0,
            values: BTreeMap::new(),
        }
    }

    pub fn execute(&mut self, program: Program, env: &Environment) -> Result<Value, Error> {
        for inst in program.instructions {
            let Instruction {
                data, result
            } = inst;



            match data {
                InstructionData::Binary { op, lhs, rhs } => {
                    let lhs = self.load_operand(lhs);
                    let rhs = self.load_operand(rhs);

                    let ret = match op {
                        Opcode::Add => Ops::add(lhs, rhs),
                        Opcode::Sub => Ops::sub(lhs, rhs),
                        Opcode::Mul => Ops::mul(lhs, rhs),
                        Opcode::Div => Ops::div(lhs, rhs),
                        Opcode::Mod => Ops::mod_(lhs, rhs),
                        Opcode::IfEqual => Ops::eq(lhs, rhs),
                        Opcode::IfNotEqual => Ops::ne(lhs, rhs),

                        Opcode::IfGreater => Ops::gt(lhs, rhs),
                        Opcode::IfGreaterOrEqual => Ops::gte(lhs, rhs),
                        Opcode::IfLess => Ops::lt(lhs, rhs),
                        Opcode::IfLessOrEqual => Ops::lte(lhs, rhs),
                        Opcode::And => Ops::and(lhs, rhs),
                        Opcode::Or => Ops::or(lhs, rhs),
                        Opcode::In => Ops::in_(lhs, rhs),
                        Opcode::Index => Ops::index(lhs, rhs),
                        Opcode::Matches => Ops::matches(lhs, rhs),
                        _ => {
                            unimplemented!()
                        }
                    };

                    self.store_value(result.unwrap(), ret?);
                }
                _ => {
                    unimplemented!()
                }
            }

        }

        Ok(Value::Null)
    }

    fn load_operand(&self, operand: ValueRef) -> &Value {
        self.values.get(&operand).expect("load operand failed")
    }

    fn store_value(&mut self, value_ref: ValueRef, value: Value) {
        self.values.insert(value_ref, value);
    }

}

impl Default for Vm {
    fn default() -> Self {
        Vm::new()
    }
}
