use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::ast::*;
use crate::opcode::OpCode;

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

#[derive(Debug, Clone, Copy)]
enum Operand {
    Static(usize),
    ImmedI(i64),
    ImmedF(f64),
    Register(usize),
    Var(usize), // for ir pass
    None,
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Static(i) => write!(f, "s{}", i),
            Operand::ImmedI(i) => write!(f, "{}", i),
            Operand::ImmedF(ff) => write!(f, "{}", ff),
            Operand::Register(i) => write!(f, "r{}", i),
            Operand::Var(i) => write!(f, "v{}", i),
            Operand::None => write!(f, ""),
        }
    }
}

// impl From<LiteralExpression> for Operand {
//     fn from(expr: LiteralExpression) -> Self {
//         match expr {
//             LiteralExpression::Literal(Literal::Integer(i)) => Operand::ImmedI(i),
//             LiteralExpression::Literal(Literal::Float(f)) => Operand::ImmedF(f),
//             _ => panic!("not a literal expression"),
//         }
//     }
// }

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
                    let idx = self.insert_static(name);
                    self.emit(OpCode::LoadStatic, &[dest, Operand::Static(idx)]);

                    dest
                }
            },
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Member => {
                        let lhs = self.compile_expr(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            // load field name
                            let field_name = self.create_var();
                            let idx = self.insert_static(name);
                            self.emit(OpCode::LoadStatic, &[field_name, Operand::Static(idx)]);
                            // load member
                            let dest = self.create_var();
                            self.emit(OpCode::LoadMember, &[dest, lhs, field_name]);
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

                        self.emit(op, &[dest, lhs, rhs]);

                        dest
                    }
                }
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
            LiteralExpression::Null => Operand::ImmedI(0),
            LiteralExpression::Boolean(true) => Operand::ImmedI(0),
            LiteralExpression::Boolean(false) => Operand::ImmedI(1),
            LiteralExpression::Integer(i) => Operand::ImmedI(i),
            LiteralExpression::Float(f) => Operand::ImmedF(f),
            LiteralExpression::Char(c) => Operand::ImmedI(c as i64),
            LiteralExpression::String(s) => {
                let idx = self.insert_static(s);
                Operand::Static(idx)
            }
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
