use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    sync::Arc,
};

use crate::value::Value;
use crate::{ast::*, instruction::Opcode};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ValueData {
    /// immediate value
    Constant(Value),
    /// value generate by inst, usually be a result.
    Inst,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IRValue(usize);

impl IRValue {
    fn new(idx: usize) -> Self {
        IRValue(idx)
    }
}

impl fmt::Display for IRValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct IRModule {
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) values: BTreeMap<IRValue, ValueData>,
    pub(crate) variables: HashMap<String, IRValue>,
}

impl IRModule {
    fn new() -> Self {
        IRModule {
            instructions: Vec::new(),
            values: BTreeMap::new(),
            variables: HashMap::new(),
        }
    }

    fn binop(&mut self, op: Opcode, lhs: IRValue, rhs: IRValue) -> IRValue {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Binary { op, lhs, rhs };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn unaryop(&mut self, op: Opcode, value: IRValue) -> IRValue {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Unary { op, operand: value };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn fn_call(&mut self, func: IRValue, args: &[IRValue]) -> IRValue {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Call {
            func,
            args: args.to_vec(),
        };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn make_array(&mut self, elements: &[IRValue]) -> IRValue {
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

    fn make_dictionary(&mut self, fields: &[(IRValue, IRValue)]) -> IRValue {
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

    fn make_constant(&mut self, value: Value) -> IRValue {
        let idx = self.values.len();
        self.values
            .insert(IRValue::new(idx), ValueData::Constant(value));
        IRValue::new(idx)
    }

    fn make_value(&mut self, value: ValueData) -> IRValue {
        let idx = self.values.len();
        self.values.insert(IRValue::new(idx), value);
        IRValue::new(idx)
    }

    fn make_variable(&mut self, name: String, value: IRValue) -> IRValue {
        self.variables.insert(name, value);
        value
    }

    fn lookup_variable(&mut self, name: &str) -> Option<IRValue> {
        self.variables.get(name).cloned()
    }

    fn load_env(&mut self, name: &str) -> IRValue {
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

    fn op_code(&self, op: &Opcode, operands: &[&IRValue]) -> String {
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

    fn value_str(&self, value: &IRValue) -> String {
        match self.values.get(value).unwrap() {
            ValueData::Constant(constant) => format!("{:?}", constant),
            ValueData::Inst => format!("%{}", value.0),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Instruction {
    pub data: InstructionData,
    pub result: Option<IRValue>,
}

impl Instruction {
    fn new(data: InstructionData, result: Option<IRValue>) -> Self {
        Instruction { data, result }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum InstructionData {
    Binary {
        op: Opcode,
        lhs: IRValue,
        rhs: IRValue,
    },
    Unary {
        op: Opcode,
        operand: IRValue,
    },
    LoadEnv {
        name: IRValue,
    },
    Call {
        func: IRValue,
        args: Vec<IRValue>,
    },
    Return {
        value: IRValue,
    },
    NewArray {
        size: IRValue,
    },
    ArrayPush {
        array: IRValue,
        element: IRValue,
    },
    NewDictionary,
    DictionaryPut {
        object: IRValue,
        key: IRValue,
        value: IRValue,
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

    fn compile_expr(&mut self, expr: Expression) -> IRValue {
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

    fn compile_binop(&mut self, op: Opcode, lhs: Expression, rhs: Expression) -> IRValue {
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

    fn compile_identfier(&mut self, ident: IdentifierExpression, maybe_var: bool) -> IRValue {
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
