use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::{
    ast::{
        BinaryOperation, BinaryOperationExpression, Expression, IdentifierExpression,
        LiteralExpression,
    },
    value::Value,
};

pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    Ret,
}

pub enum OpCode {
    LoadConst {
        dest: Register,
        name: String,
    },
    LoadVar {
        dest: Register,
        name: String,
    },
    Add {
        dest: Register,
        left: Register,
        right: Register,
    },
    Sub {
        dest: Register,
        left: Register,
        right: Register,
    },
    Mul {
        dest: Register,
        left: Register,
        right: Register,
    },
    Div {
        dest: Register,
        left: Register,
        right: Register,
    },
    Mod {
        dest: Register,
        left: Register,
        right: Register,
    },
    Push(Register),
    Pop(Register),
    Jump,
    JumpIfZero,
    JumpIfNotZero,
    JumpIfEqual,
    JumpIfNotEqual,
    JumpIfLess,
    JumpIfLessOrEqual,
    JumpIfGreater,
    JumpIfGreaterOrEqual,
    Call,
    Return,
}

// #[derive(Debug, Clone)]
// pub struct IRConst(Value);

// impl fmt::Display for IRConst {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

#[derive(Debug, Clone, Copy)]
pub struct IRVar(usize);

impl fmt::Display for IRVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
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

impl TryFrom<BinaryOperation> for IROp {
    type Error = ();

    fn try_from(op: BinaryOperation) -> Result<Self, Self::Error> {
        Ok(match op {
            BinaryOperation::Addition => IROp("add".to_string()),
            BinaryOperation::Subtraction => IROp("sub".to_string()),
            BinaryOperation::Multiplication => IROp("mul".to_string()),
            BinaryOperation::Division => IROp("div".to_string()),
            BinaryOperation::Modulus => IROp("mod".to_string()),
            BinaryOperation::Power => IROp("pow".to_string()),
            BinaryOperation::Equal => IROp("eq".to_string()),
            BinaryOperation::NotEqual => IROp("neq".to_string()),
            BinaryOperation::LessThan => IROp("lt".to_string()),
            BinaryOperation::LessThanOrEqual => IROp("leq".to_string()),
            BinaryOperation::GreaterThan => IROp("gt".to_string()),
            BinaryOperation::GreaterThanOrEqual => IROp("geq".to_string()),
            BinaryOperation::And => IROp("and".to_string()),
            BinaryOperation::Or => IROp("or".to_string()),
            BinaryOperation::In => IROp("in".to_string()),
            BinaryOperation::Matches => IROp("matches".to_string()),
            BinaryOperation::Access => IROp("loadfield".to_string()),
            _ => {
                return Err(());
            }
        })
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
}

impl IRCompiler {
    fn new() -> Self {
        let mut stack = IRStack::new();
        stack.push();
        IRCompiler {
            stack,
            var: 0,
            code: Vec::new(),
        }
    }

    fn compile_expr(&mut self, expr: Expression) -> IRVar {
        match expr {
            Expression::Literal(lit) => {
                let var = self.create_var();
                self.emit(format!("{} {} {}", IROp::from("loadconst"), var, lit));
                var
            }
            Expression::Identifier(IdentifierExpression { name }) => match self.stack.get(&name) {
                Some(var) => var,
                None => {
                    let var = self.create_var();
                    self.emit(format!("{} {} {}", IROp::from("loadenv"), var, name));
                    var
                }
            },
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Access => {
                        let lhs = self.compile_expr(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            let var = self.create_var();
                            self.emit(format!(
                                "{} {} {} {}",
                                IROp::from("loadfield"),
                                var,
                                lhs,
                                name
                            ));
                            var
                        } else {
                            unreachable!("unexpect rhs{:?} on access", right);
                        }
                    }
                    _ => {
                        let lhs = self.compile_expr(*left);
                        let rhs = self.compile_expr(*right);
                        let op = IROp::try_from(op).unwrap();
                        let var = self.create_var();

                        self.emit(format!("{} {} {} {}", op, var, lhs, rhs));

                        var
                    }
                }
            }

            e => {
                unreachable!("expr {:?}", e);
            }
        }
    }

    fn create_var(&mut self) -> IRVar {
        self.var += 1;
        IRVar(self.var)
    }

    fn emit(&mut self, code: impl ToString) {
        self.code.push(code.to_string());
    }

    fn code(&self) -> String {
        self.code.join(";\n")
    }
}

pub struct Compiler {}

impl Compiler {
    // pub fn compile(expr: Expression) -> Vec<OpCode> {

    // }
}

// pub fn compile(expr: Expression) -> Vec<OpCode> {
//     let mut opcods = vec![];

//     match expr {
//         Expression::Identifier(IdentifierExpression { name }) => opcods.push(OpCode::Push),
//         Expression::BinaryOperation(BinaryOperationExpression { op, left, right }) => {
//             let left_opcods = compile(*left);
//             let right_opcods = compile(*right);

//             opcods.extend(left_opcods);
//             opcods.extend(right_opcods);

//             match op {
//                 // TODO: implement more operators
//                 "+" => {
//                     opcods.push(OpCode::Add);
//                 }
//             }
//         }
//         _ => {
//             unreachable!();
//         }
//     };

//     opcods
// }

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

            let code = compiler.code();

            println!("code:\n{}\n====", code);
        }
    }
}
