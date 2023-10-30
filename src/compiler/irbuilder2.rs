use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    sync::Arc,
};

use crate::{ast::*, instruction::Opcode, value::Primitive};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ValueData {
    /// immediate value
    Constant(Primitive),
    /// value generate by inst, usually be a result.
    Inst,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value(usize);

impl Value {
    fn new(idx: usize) -> Self {
        Value(idx)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct IRBuilder {
    instructions: Vec<Instruction>,
    values: BTreeMap<Value, ValueData>,
    variables: HashMap<String, Value>,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            instructions: Vec::new(),
            values: BTreeMap::new(),
            variables: HashMap::new(),
        }
    }

    fn binop(&mut self, op: Opcode, lhs: Value, rhs: Value) -> Value {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Binary { op, lhs, rhs };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn unaryop(&mut self, op: Opcode, value: Value) -> Value {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Unary { op, operand: value };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn fn_call(&mut self, func: Value, args: &[Value]) -> Value {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::Call {
            func,
            args: args.to_vec(),
        };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }

    fn make_array(&mut self, elements: &[Value]) -> Value {
        let array = self.make_value(ValueData::Inst);
        let data = InstructionData::NewArray {
            size: elements.len(),
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

    fn make_dictionary(&mut self, fields: &[(Value, Value)]) -> Value {
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

    fn make_constant(&mut self, value: Primitive) -> Value {
        let idx = self.values.len();
        self.values
            .insert(Value::new(idx), ValueData::Constant(value));
        Value::new(idx)
    }

    fn make_value(&mut self, value: ValueData) -> Value {
        let idx = self.values.len();
        self.values.insert(Value::new(idx), value);
        Value::new(idx)
    }

    fn make_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn lookup_variable(&mut self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    fn load_env(&mut self, name: &str) -> Value {
        let result = self.make_value(ValueData::Inst);
        let data = InstructionData::LoadEnv {
            name: name.to_string(),
        };
        self.instructions.push(Instruction::new(data, Some(result)));
        result
    }
}

#[derive(Debug, Clone)]
struct Instruction {
    data: InstructionData,
    result: Option<Value>,
}

impl Instruction {
    pub fn new(data: InstructionData, result: Option<Value>) -> Self {
        Instruction { data, result }
    }
}

#[derive(Debug, Clone)]
enum InstructionData {
    Binary {
        op: Opcode,
        lhs: Value,
        rhs: Value,
    },
    Unary {
        op: Opcode,
        operand: Value,
    },
    Call {
        func: Value,
        args: Vec<Value>,
    },
    LoadEnv {
        name: String,
    },
    LoadVar {
        name: String,
    },
    LoadConst {
        value: Primitive,
    },
    StoreVar {
        name: String,
    },
    StoreConst {
        value: Primitive,
    },
    Return {
        value: Value,
    },
    NewArray {
        size: usize,
    },
    ArrayPush {
        array: Value,
        element: Value,
    },
    NewDictionary,
    DictionaryPut {
        object: Value,
        key: Value,
        value: Value,
    },
}

pub struct CodeGen {
    ir_builer: IRBuilder,
}

impl CodeGen {
    pub fn new() -> Self {
        CodeGen {
            ir_builer: IRBuilder::new(),
        }
    }

    pub fn compile(&mut self, expr: Expression) {
        self.compile_expr(expr);
    }

    fn compile_expr(&mut self, expr: Expression) -> Value {
        match expr {
            Expression::BinaryOperation(BinaryOperationExpression { op, left, right }) => {
                let op = Opcode::try_from(op).unwrap();
                self.compile_binop(op, *left, *right)
            }
            Expression::UnaryOperation(UnaryOperationExpression { op, expr }) => {
                let operand = self.compile_expr(*expr);
                let op = Opcode::try_from(op).unwrap();
                self.ir_builer.unaryop(op, operand)
            }
            Expression::Grouped(GroupedExpression(expr)) => self.compile_expr(*expr),
            Expression::Literal(lit) => {
                let v = match lit {
                    LiteralExpression::Integer(i) => Primitive::Integer(i),
                    LiteralExpression::Float(f) => Primitive::Float(f),
                    LiteralExpression::String(s) => Primitive::String(Arc::new(s)),
                    LiteralExpression::Boolean(b) => Primitive::Bool(b),
                    LiteralExpression::Char(c) => Primitive::Char(c),
                    LiteralExpression::Null => Primitive::Null,
                    LiteralExpression::Undefined => Primitive::Undefined,
                };

                self.ir_builer.make_constant(v)
            }
            Expression::Identifier(IdentifierExpression { name }) => {
                if let Some(var) = self.ir_builer.lookup_variable(&name) {
                    return var;
                }
                self.ir_builer
                    .make_constant(Primitive::String(Arc::new(name)))
            }
            Expression::Array(ArrayExpression { elements }) => {
                let mut array = Vec::new();
                for element in elements {
                    let element = self.compile_expr(element);
                    array.push(element);
                }

                self.ir_builer.make_array(&array)
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let mut dict = Vec::new();
                for kv in elements {
                    let key = self.compile_identfier(kv.key, false);
                    let value = self.compile_expr(*kv.value);
                    dict.push((key, value));
                }

                self.ir_builer.make_dictionary(&dict)
            }
            Expression::Variable(VariableExpression { name }) => self.ir_builer.load_env(&name),
            _ => {
                unimplemented!("compile_expr: {:?}", expr)
            }
        }
    }

    fn compile_binop(&mut self, op: Opcode, lhs: Expression, rhs: Expression) -> Value {
        let lhs = self.compile_expr(lhs);

        match (op, rhs) {
            (Opcode::Call, Expression::Identifier(ident)) => {
                let member = self.compile_identfier(ident, false);
                self.ir_builer.fn_call(lhs, &[member])
            }
            (op, rhs) => {
                let rhs = self.compile_expr(rhs);
                self.ir_builer.binop(op, lhs, rhs)
            }
        }
    }

    fn compile_identfier(&mut self, ident: IdentifierExpression, maybe_var: bool) -> Value {
        if maybe_var {
            if let Some(var) = self.ir_builer.lookup_variable(&ident.name) {
                return var;
            }
        }

        self.ir_builer
            .make_constant(Primitive::String(Arc::new(ident.name.to_string())))
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

            let mut cg = CodeGen::new();

            cg.compile(expr);

            println!("{:?}", cg.ir_builer.instructions);

            println!("====",);
        }
    }
}
