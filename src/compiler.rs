use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::opcode::OpCode;
use crate::{ast::*, value::Value};

#[derive(Debug, Clone)]
struct IROpCode {
    index: usize,
    opcode: OpCode,
    operand0: Operand,
    operand1: Operand,
    operand2: Operand,
}

impl fmt::Display for IROpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {} {} {}",
            self.opcode, self.operand0, self.operand1, self.operand2
        )
    }
}

#[derive(Debug, Clone)]
enum Operand {
    Immed(Value),
    Register(usize),
    Var(usize), // for ir pass
    None,
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Immed(Value) => write!(f, "{}", Value),
            Operand::Register(i) => write!(f, "r{}", i),
            Operand::Var(i) => write!(f, "v{}", i),
            Operand::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IRVar(usize);

impl fmt::Display for IRVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct IROp(String);

impl fmt::Display for IROp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for IROp {
    fn from(value: &str) -> Self {
        IROp(value.to_string())
    }
}

struct IRStackFrame(HashMap<String, IRVar>);

struct IRStack {
    stack: VecDeque<IRStackFrame>,
}

impl IRStack {
    fn new() -> Self {
        IRStack {
            stack: VecDeque::new(),
        }
    }

    fn push(&mut self) {
        self.stack.push_back(IRStackFrame(HashMap::new()));
    }

    fn pop(&mut self) {
        self.stack.pop_back();
    }

    fn get(&self, name: &str) -> Option<IRVar> {
        for frame in self.stack.iter().rev() {
            if let Some(var) = frame.0.get(name) {
                return Some(*var);
            }
        }

        None
    }

    fn set(&mut self, name: &str, var: IRVar) {
        self.stack
            .back_mut()
            .unwrap()
            .0
            .insert(name.to_string(), var);
    }
}

pub struct IRCompiler {
    stack: IRStack,
    var: usize,
    code: Vec<String>,
    static_data: Vec<String>,
    opcodes: Vec<IROpCode>,
}

impl IRCompiler {
    fn new() -> Self {
        let mut stack = IRStack::new();
        stack.push();
        IRCompiler {
            stack,
            var: 0,
            code: Vec::new(),
            static_data: Vec::new(),
            opcodes: Vec::new(),
        }
    }

    fn compile_expr(&mut self, expr: Expression) -> Operand {
        match expr {
            Expression::Literal(lit) => self.create_literal_operand(lit),
            Expression::Identifier(IdentifierExpression { name }) => match self.stack.get(&name) {
                Some(var) => Operand::Var(var.0),
                None => {
                    let dest = self.create_var();
                    self.emit(OpCode::LoadEnv, &[dest.clone(), Operand::Immed(Value::String(name))]);

                    dest
                }
            },
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Member => {
                        let lhs = self.compile_expr(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            // load member
                            let dest = self.create_var();
                            self.emit(OpCode::LoadMember, &[dest.clone(), lhs, Operand::Immed(Value::String(name))]);
                            dest
                        } else {
                            unreachable!("unexpect rhs{:?} on access", right);
                        }
                    }
                    _ => {
                        let lhs = self.compile_expr(*left);
                        let rhs = self.compile_expr(*right);
                        let op = OpCode::try_from(op).unwrap();
                        let dest = self.create_var();

                        self.emit(op, &[dest.clone(), lhs, rhs]);

                        dest
                    }
                }
            }
            Expression::UnaryOperation(UnaryOperationExpression::Negation(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.create_var();
                self.emit(OpCode::Negate, &[dest.clone(), operand]);
                dest
            }
            Expression::Grouped(GroupedExpression(expr)) => self.compile_expr(*expr),
            Expression::Array(ArrayExpression { elements }) => {
                let array = self.create_var();
                self.emit(OpCode::NewArray, &[array.clone()]);
                for element in elements {
                    let item = self.compile_expr(element);
                    self.emit(OpCode::ArrayPush, &[array.clone(), item]);
                }
                array
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let dict = self.create_var();
                self.emit(OpCode::NewDictionary, &[dict.clone()]);
                for kv in elements {
                    let key = Operand::Immed(Value::String(kv.key.name));
                    let value = self.compile_expr(*kv.value);
                    self.emit(OpCode::DictionaryPut, &[dict.clone(), key, value]);
                }
                dict
            }

            e => {
                unreachable!("expr {:?}", e);
            }
        }
    }

    fn create_var(&mut self) -> Operand {
        self.var += 1;
        Operand::Var(self.var)
    }

    fn create_literal_operand(&mut self, lit: LiteralExpression) -> Operand {
        match lit {
            LiteralExpression::Null => Operand::Immed(Value::Null),
            LiteralExpression::Boolean(b) => Operand::Immed(Value::Bool(b)),
            LiteralExpression::Integer(i) => Operand::Immed(Value::Integer(i)),
            LiteralExpression::Float(f) => Operand::Immed(Value::Float(f)),
            LiteralExpression::Char(c) => Operand::Immed(Value::Char(c)),
            LiteralExpression::String(s) => Operand::Immed(Value::String(s)),
        }
    }

    fn insert_static(&mut self, s: String) -> usize {
        match self.static_data.iter().position(|x| x == &s) {
            Some(idx) => idx,
            None => {
                let idx = self.static_data.len();
                self.static_data.push(s);
                idx
            }
        }
    }

    fn emit(&mut self, opcode: OpCode, operands: &[Operand]) {
        let idx = self.opcodes.len();

        let opcode = IROpCode {
            index: idx,
            opcode,
            operand0: operands.get(0).unwrap_or(&Operand::None).clone(),
            operand1: operands.get(1).unwrap_or(&Operand::None).clone(),
            operand2: operands.get(2).unwrap_or(&Operand::None).clone(),
        };

        self.opcodes.push(opcode);
    }

    fn print_code(&self) {
        for code in &self.opcodes {
            println!("{};", code);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn test_ircompile() {
        let inputs = vec![
            r#"a + b * c + 100 / 2"#,
            r#"user.Group in groups || user.Id == comment.UserId"#,
            // r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let mut compiler = IRCompiler::new();
            compiler.compile_expr(expr);

            compiler.print_code();
            println!("====",);
        }
    }
}
