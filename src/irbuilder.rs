use std::{fmt, collections::{BTreeMap, HashMap}};

use crate::ast::*;
use crate::{value::Value, opcode::Opcode};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ValueData {
    /// immediate value
    Constant(Value),
    /// value generate by inst, usually be a result.
    Inst,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ValueRef(pub usize);

impl ValueRef {
    fn new(idx: usize) -> Self {
        ValueRef(idx)
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct IRModule {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) values: BTreeMap<ValueRef, ValueData>,
    pub(crate) variables: BTreeMap<String, ValueRef>,
}

impl IRModule {
    fn new() -> Self {
        IRModule {
            instructions: Vec::new(),
            values: BTreeMap::new(),
            variables: BTreeMap::new(),
        }
    }

    fn binop(&mut self, op: Opcode, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Binary { op, lhs, rhs };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn unaryop(&mut self, op: Opcode, value: ValueRef) -> ValueRef {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Unary { op, operand: value };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn fn_call(&mut self, func: ValueRef, args: &[ValueRef]) -> ValueRef {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Call {
            func,
            args: args.to_vec(),
        };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn make_array(&mut self, elements: &[ValueRef]) -> ValueRef {
        let array = self.make_value(ValueData::Inst);
        let data = InstructionData::NewArray {
            size: self.make_constant(Value::Integer(elements.len() as i64)),
        };
        self.instructions.push(Instruction::new(data, Some(array)));

        for element in elements {
            self.instructions.push(Instruction::new(
                InstructionData::ArrayPush {
                    array,
                    element: *element,
                },
                None,
            ));
        }

        array
    }

    fn make_dictionary(&mut self, fields: &[(ValueRef, ValueRef)]) -> ValueRef {
        let dict = self.make_value(ValueData::Inst);
        let data = InstructionData::NewDictionary;
        self.instructions.push(Instruction::new(data, Some(dict)));

        for (key, value) in fields {
            self.instructions.push(Instruction::new(
                InstructionData::DictionaryPut {
                    object: dict,
                    key: *key,
                    value: *value,
                },
                None,
            ));
        }

        dict
    }

    fn make_constant(&mut self, value: Value) -> ValueRef {
        let idx = self.values.len();
        self.values
            .insert(ValueRef::new(idx), ValueData::Constant(value));
        ValueRef::new(idx)
    }

    fn make_value(&mut self, value: ValueData) -> ValueRef {
        let idx = self.values.len();
        self.values.insert(ValueRef::new(idx), value);
        ValueRef::new(idx)
    }

    fn make_variable(&mut self, name: String, value: ValueRef) -> ValueRef {
        self.variables.insert(name, value);
        value
    }

    fn lookup_variable(&mut self, name: &str) -> Option<ValueRef> {
        self.variables.get(name).cloned()
    }

    fn load_env(&mut self, name: &str) -> ValueRef {
        let result = self.make_value(ValueData::Inst);
        let name = self.make_constant(Value::String(name.to_string()));
        let data = InstructionData::LoadEnv { name };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn codegen(&self) -> Vec<String> {
        let mut code = vec![];
        for instruction in &self.instructions {
            code.push(self.ir_code(instruction));
        }

        code
    }

    fn ir_code(&self, instruction: &Instruction) -> String {
        let op = match &instruction.data {
            InstructionData::Binary { op, lhs, rhs } => self.op_code(op, &[lhs, rhs]),
            InstructionData::Unary { op, operand } => self.op_code(op, &[operand]),
            InstructionData::LoadEnv { name } => self.op_code(&Opcode::LoadEnv, &[name]),
            InstructionData::Call { func, args } => {
                unimplemented!()
            }
            InstructionData::Return { value } => {
                unimplemented!()
            }
            InstructionData::NewArray { size } => self.op_code(&Opcode::NewArray, &[size]),
            InstructionData::ArrayPush { array, element } => {
                self.op_code(&Opcode::ArrayPush, &[array, element])
            }
            InstructionData::NewDictionary => self.op_code(&Opcode::NewDictionary, &[]),
            InstructionData::DictionaryPut { object, key, value } => {
                self.op_code(&Opcode::DictionaryPut, &[object, key, value])
            }
        };

        if let Some(ret) = instruction.result {
            format!("{} = {}", self.value_str(&ret), op)
        } else {
            op
        }
    }

    fn op_code(&self, op: &Opcode, operands: &[&ValueRef]) -> String {
        format!(
            "{:?} {}",
            op,
            operands
                .iter()
                .map(|v| self.value_str(&v))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }

    fn value_str(&self, value: &ValueRef) -> String {
        match self.values.get(value).unwrap() {
            ValueData::Constant(constant) => format!("{:?}", constant),
            ValueData::Inst => format!("%{}", value.0),
        }
    }

    pub fn value_data(&self, value: &ValueRef) -> &ValueData {
        self.values.get(value).unwrap()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Instruction {
    pub data: InstructionData,
    pub result: Option<ValueRef>,
}

impl Instruction {
    fn new(data: InstructionData, result: Option<ValueRef>) -> Self {
        Instruction { data, result }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum InstructionData {
    Binary {
        op: Opcode,
        lhs: ValueRef,
        rhs: ValueRef,
    },
    Unary {
        op: Opcode,
        operand: ValueRef,
    },
    LoadEnv {
        name: ValueRef,
    },
    Call {
        func: ValueRef,
        args: Vec<ValueRef>,
    },
    Return {
        value: ValueRef,
    },
    NewArray {
        size: ValueRef,
    },
    ArrayPush {
        array: ValueRef,
        element: ValueRef,
    },
    NewDictionary,
    DictionaryPut {
        object: ValueRef,
        key: ValueRef,
        value: ValueRef,
    },
}

pub struct IRBuilder {
    module: IRModule,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            module: IRModule::new(),
        }
    }

    pub fn build_expr(expr: Expression) -> IRModule {
        let mut builder = IRBuilder::new();
        builder.compile_expr(expr);
        let IRBuilder { module } = builder;
        module
    }

    fn compile_expr(&mut self, expr: Expression) -> ValueRef {
        match expr {
            Expression::BinaryOperation(BinaryOperationExpression { op, left, right }) => {
                let op = Opcode::try_from(op).unwrap();
                self.compile_binop(op, *left, *right)
            }
            Expression::UnaryOperation(UnaryOperationExpression { op, expr }) => {
                let operand = self.compile_expr(*expr);
                let op = Opcode::try_from(op).unwrap();
                self.module.unaryop(op, operand)
            }
            Expression::Grouped(GroupedExpression(expr)) => self.compile_expr(*expr),
            Expression::Literal(lit) => {
                let v = match lit {
                    LiteralExpression::Integer(i) => Value::Integer(i),
                    LiteralExpression::Float(f) => Value::Float(f),
                    LiteralExpression::String(s) => Value::String(s),
                    LiteralExpression::Boolean(b) => Value::Boolean(b),
                    LiteralExpression::Char(c) => Value::Char(c),
                    LiteralExpression::Null => Value::Null,
                    LiteralExpression::Undefined => Value::Undefined,
                };

                self.module.make_constant(v)
            }
            Expression::Identifier(IdentifierExpression { name }) => {
                if let Some(var) = self.module.lookup_variable(&name) {
                    return var;
                }
                let var = self.module.make_value(ValueData::Inst);
                self.module.make_variable(name, var)
            }
            Expression::Array(ArrayExpression { elements }) => {
                let mut array = Vec::new();
                for element in elements {
                    let element = self.compile_expr(element);
                    array.push(element);
                }

                self.module.make_array(&array)
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let mut dict = Vec::new();
                for kv in elements {
                    let key = self.compile_identfier(kv.key, false);
                    let value = self.compile_expr(*kv.value);
                    dict.push((key, value));
                }

                self.module.make_dictionary(&dict)
            }
            Expression::Variable(VariableExpression { name }) => self.module.load_env(&name),
            _ => {
                unimplemented!("compile_expr: {:?}", expr)
            }
        }
    }

    fn compile_binop(&mut self, op: Opcode, lhs: Expression, rhs: Expression) -> ValueRef {
        let lhs = self.compile_expr(lhs);

        match (op, rhs) {
            (Opcode::Call, Expression::Identifier(ident)) => {
                let member = self.compile_identfier(ident, false);
                self.module.fn_call(lhs, &[member])
            }
            (op, rhs) => {
                let rhs = self.compile_expr(rhs);
                self.module.binop(op, lhs, rhs)
            }
        }
    }

    fn compile_identfier(&mut self, ident: IdentifierExpression, maybe_var: bool) -> ValueRef {
        if maybe_var {
            if let Some(var) = self.module.lookup_variable(&ident.name) {
                return var;
            }
        }

        self.module.make_constant(Value::String(ident.name))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::ast::Expression;
    use crate::parser::Parser;

    #[test]
    fn test_compile_expr() {
        let inputs = vec![
            r#"a + b * c + 100 - e + f % g / 2 + a#,
            r#"user.Role in admins || user.Id == comment.UserId"#,
            r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#,
            r#"$user.Group in ["admin", "moderator"] || $user.Id == $comment.UserId"#,
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let module = IRBuilder::build_expr(expr);

            println!("{:?}", module.instructions);

            for line in module.codegen() {
                println!("{:?};", line);
            }

            println!("====",);
        }
    }
}
