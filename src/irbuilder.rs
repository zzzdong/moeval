use std::collections::BTreeMap;

use crate::ast::*;
use crate::instruction::*;
use crate::value::Value;

pub struct IRBuilder {
    module: Module,

    symbol_table: BTreeMap<String, ValueRef>,
}

impl IRBuilder {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            symbol_table: BTreeMap::new(),
        }
    }

    // pub fn generate(&self, ast: Program) -> String {
    //     let mut code = String::new();

    //     for statement in ast.items {
    //         code += &self.generate_statement(items);
    //     }

    //     code
    // }

    // fn generate_statement(&self, statement: Statement) -> String {
    //     match statement {
    //         Statement::Expression(expression) => self.generate_expression(expression),
    //         _ => unimplemented!("{:?}", statement),
    //     }
    // }

    pub fn build_expression(&mut self, expr: Expression) -> ValueRef {
        match expr {
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Identifier(identifier) => self.build_identifier(identifier),
            Expression::Binary(op, lhs, rhs) => self.build_binary(op, *lhs, *rhs),
            Expression::Member(member) => self.build_get_property(member),
            Expression::Call(call) => self.build_call(call),
            Expression::Assign(assign) => self.build_assign(assign),
            // Expression::Closure(closure) => self.build_closure(closure),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_get_property(&mut self, expr: MemberExpression) -> ValueRef {
        let MemberExpression { object, property } = expr;

        let object = self.build_expression(*object);

        self.module.get_property(object, &property)
    }

    fn build_set_property(&mut self, expr: MemberExpression, value: ValueRef) {
        let MemberExpression { object, property } = expr;

        let object = self.build_expression(*object);

        self.module.set_property(object, &property, value)
    }

    fn build_call(&mut self, expr: CallExpression) -> ValueRef {
        let CallExpression { func, args } = expr;

        let args: Vec<ValueRef> = args
            .into_iter()
            .map(|arg| self.build_expression(arg))
            .collect();

        match *func {
            Expression::Member(member) => {
                let MemberExpression { object, property } = member;

                let object = self.build_expression(*object);

                self.module.call_property(object, property, args)
            }
            _ => {
                let func = self.build_expression(*func);

                self.module.make_call(func, args)
            }
        }
    }

    fn build_assign(&mut self, expr: AssignExpression) -> ValueRef {
        let AssignExpression { object, value } = expr;

        match *object {
            Expression::Member(member) => {
                let value = self.build_expression(*value);
                self.build_set_property(member, value);
                value
            }
            _ => {
                let object = self.build_expression(*object);
                let value = self.build_expression(*value);

                self.module.assign(object, value);
                value
            }
        }
    }

    // fn build_closure(&mut self, expr: ClosureExpression) -> ValueRef {
    //     let ClosureExpression { params, body } = expr;

    //     let params = params
    //         .into_iter()
    //         .map(|param| self.build_expression(param.name))
    //         .collect();

    //     let body = self.build_expression(*body);

    //     self.module.make_closure(params, body)
    // }

    fn build_literal(&mut self, literal: LiteralExpression) -> ValueRef {
        let value = match literal {
            LiteralExpression::Boolean(b) => Value::Boolean(b),
            LiteralExpression::Integer(i) => Value::Integer(i),
            LiteralExpression::Float(f) => Value::Float(f),
            LiteralExpression::Char(c) => Value::Char(c),
            LiteralExpression::String(s) => Value::String(s),
        };

        self.module.make_constant(value)
    }

    fn build_identifier(&mut self, identifier: IdentifierExpression) -> ValueRef {
        *self
            .symbol_table
            .entry(identifier.0.to_string())
            .or_insert_with(|| self.module.load_external_variable(&identifier.0))
    }

    fn build_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> ValueRef {
        let lhs = self.build_expression(lhs);
        let rhs = self.build_expression(rhs);

        match op {
            BinOp::Add => self.module.binop(Opcode::Add, lhs, rhs),
            BinOp::Sub => self.module.binop(Opcode::Sub, lhs, rhs),
            BinOp::Mul => self.module.binop(Opcode::Mul, lhs, rhs),
            BinOp::Div => self.module.binop(Opcode::Div, lhs, rhs),
            BinOp::Mod => self.module.binop(Opcode::Mod, lhs, rhs),

            BinOp::Equal => self.module.binop(Opcode::Equal, lhs, rhs),
            BinOp::NotEqual => self.module.binop(Opcode::NotEqual, lhs, rhs),
            BinOp::Greater => self.module.binop(Opcode::Greater, lhs, rhs),
            BinOp::GreaterEqual => self.module.binop(Opcode::GreaterEqual, lhs, rhs),
            BinOp::Less => self.module.binop(Opcode::Less, lhs, rhs),
            BinOp::LessEqual => self.module.binop(Opcode::LessEqual, lhs, rhs),

            _ => unimplemented!("{:?}", op),
        }
    }

    pub fn debug_instructions(&self) {
        self.module.debug();
    }
}
