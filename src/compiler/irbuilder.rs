use std::collections::hash_map::Entry;
use std::collections::HashMap;

use crate::ast::*;
use crate::instruction::{Instruction, Module, OpCode, Operand};
use crate::instruction::{Register, StackSlot, VirtReg};
use crate::value::Primitive;

#[derive(Debug, Clone)]
struct VirtRegAllocator {
    count: usize,
}

impl VirtRegAllocator {
    pub fn new() -> Self {
        VirtRegAllocator { count: 0 }
    }

    pub fn alloc(&mut self) -> VirtReg {
        let var = self.count;
        self.count += 1;
        VirtReg(var)
    }
}

#[derive(Debug, Clone)]
pub struct IRBuilder {
    vr_allocator: VirtRegAllocator,
    symbol_table: HashMap<String, VirtReg>,
    instructions: Vec<Instruction>,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            vr_allocator: VirtRegAllocator::new(),
            instructions: Vec::new(),
            symbol_table: HashMap::new(),
        }
    }

    pub fn build_expr(expr: Expression) -> Module {
        let mut builder = IRBuilder::new();
        let ret = builder.build_expr_inner(expr);
        builder.emit(OpCode::Return, &[ret]);
        builder.instructions()
    }

    fn build_expr_inner(&mut self, expr: Expression) -> Operand {
        match expr {
            Expression::Literal(lit) => self.create_literal_operand(lit),
            Expression::Identifier(IdentifierExpression { name }) => {
                match self.symbol_table.get(&name) {
                    Some(vreg) => Operand::VirtReg(*vreg),
                    None => {
                        let dest = self.alloc_virt_reg();
                        self.emit(
                            OpCode::LoadEnv,
                            &[
                                dest.clone(),
                                Operand::Immed(Primitive::String(name.clone().into())),
                            ],
                        );

                        if let Operand::VirtReg(vreg) = dest {
                            self.symbol_table.insert(name, vreg);
                        }

                        dest
                    }
                }
            }
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Member => {
                        let lhs = self.build_expr_inner(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            // load member
                            let dest = self.alloc_virt_reg();
                            self.emit(
                                OpCode::LoadMember,
                                &[
                                    dest.clone(),
                                    lhs,
                                    Operand::Immed(Primitive::String(name.into())),
                                ],
                            );
                            dest
                        } else {
                            unreachable!("unexpect rhs{:?} on access", right);
                        }
                    }
                    _ => {
                        let lhs = self.build_expr_inner(*left);
                        let rhs = self.build_expr_inner(*right);
                        let op = OpCode::try_from(op).unwrap();
                        let dest = self.alloc_virt_reg();

                        self.emit(op, &[dest.clone(), lhs, rhs]);

                        dest
                    }
                }
            }
            Expression::UnaryOperation(UnaryOperationExpression { op, expr }) => {
                let operand = self.build_expr_inner(*expr);
                let dest = self.alloc_virt_reg();
                let op = OpCode::try_from(op).unwrap();
                self.emit(op, &[dest.clone(), operand]);
                dest
            }
            Expression::Grouped(GroupedExpression(expr)) => self.build_expr_inner(*expr),
            Expression::Array(ArrayExpression { elements }) => {
                let array = self.alloc_virt_reg();
                self.emit(OpCode::NewArray, &[array.clone()]);
                for element in elements {
                    let item = self.build_expr_inner(element);
                    self.emit(OpCode::ArrayPush, &[array.clone(), item]);
                }
                array
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let dict = self.alloc_virt_reg();
                self.emit(OpCode::NewDictionary, &[dict.clone()]);
                for kv in elements {
                    let key = Operand::Immed(Primitive::String(kv.key.name.into()));
                    let value = self.build_expr_inner(*kv.value);
                    self.emit(OpCode::DictionaryPut, &[dict.clone(), key, value]);
                }
                dict
            }
            Expression::Index(IndexExpression { object, index }) => {
                let object = self.build_expr_inner(*object);
                let index = self.build_expr_inner(*index);
                let result = self.alloc_virt_reg();
                self.emit(OpCode::Index, &[result.clone(), object, index]);
                result
            }
            e => {
                unreachable!("expr {:?}", e);
            }
        }
    }

    fn alloc_virt_reg(&mut self) -> Operand {
        let vr = self.vr_allocator.alloc();
        Operand::VirtReg(vr)
    }

    fn create_literal_operand(&mut self, lit: LiteralExpression) -> Operand {
        match lit {
            LiteralExpression::Null => Operand::Immed(Primitive::Null),
            LiteralExpression::Boolean(b) => Operand::Immed(Primitive::Bool(b)),
            LiteralExpression::Integer(i) => Operand::Immed(Primitive::Integer(i)),
            LiteralExpression::Float(f) => Operand::Immed(Primitive::Float(f)),
            LiteralExpression::Char(c) => Operand::Immed(Primitive::Char(c)),
            LiteralExpression::String(s) => Operand::Immed(Primitive::String(s.into())),
        }
    }

    fn emit(&mut self, opcode: OpCode, operands: &[Operand]) {
        let _idx = self.instructions.len();

        let opcode = Instruction::new(
            opcode,
            operands.get(0).unwrap_or(&Operand::None).clone(),
            operands.get(1).unwrap_or(&Operand::None).clone(),
            operands.get(2).unwrap_or(&Operand::None).clone(),
        );

        self.instructions.push(opcode);
    }

    pub fn instructions(&self) -> Module {
        Module::with_instructions(self.instructions.clone())
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn test_irbuilder() {
        let inputs = vec![
            r#"a + b * c + 100 - e + f % g / 2 + a#,
            r#"user.Role in admins || user.Id == comment.UserId"#,
            r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#,
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let instructions = IRBuilder::build_expr(expr);

            println!("{}", instructions);
            println!("====",);
        }
    }
}
