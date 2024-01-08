use std::collections::BTreeMap;

use crate::ast::*;
use crate::instruction::*;
use crate::value::Value;

pub struct IRBuilder {
    module: Module,
    symbol_table: SymbolTable,
}

impl IRBuilder {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn build(&mut self, ast: Program) {
        let main_block = self.module.create_block(Some("main"));

        let builder = self.module.switch_to_block(main_block);

        for item in ast.items {
            self.build_toplevel(item);
        }
    }

    fn build_toplevel(&mut self, toplevel: TopLevelItem) {
        match toplevel {
            TopLevelItem::Expression(expr) => {
                self.build_expression(expr);
            }
            TopLevelItem::Statement(stmt) => {
                self.build_statement(stmt);
            }
        }
    }

    fn build_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Expression(expression) => {
                self.build_expression(expression);
            }
            Statement::Return(ReturnStatement { value }) => {
                let value = value.map(|expr| self.build_expression(expr));
                self.module.ins().return_(value);
            }
            _ => unimplemented!("{:?}", statement),
        }
    }

    pub fn build_expression(&mut self, expr: Expression) -> ValueRef {
        match expr {
            Expression::Literal(literal) => self.build_literal(literal),
            Expression::Identifier(identifier) => self.build_identifier(identifier),
            Expression::Binary(op, lhs, rhs) => self.build_binary(op, *lhs, *rhs),
            Expression::Member(member) => self.build_get_property(member),
            Expression::Call(call) => self.build_call(call),
            Expression::Assign(assign) => self.build_assign(assign),
            Expression::Closure(closure) => self.build_closure(closure),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn build_get_property(&mut self, expr: MemberExpression) -> ValueRef {
        let MemberExpression { object, property } = expr;

        let object = self.build_expression(*object);

        self.module.ins().get_property(object, &property)
    }

    fn build_set_property(&mut self, expr: MemberExpression, value: ValueRef) {
        let MemberExpression { object, property } = expr;

        let object = self.build_expression(*object);

        self.module.ins().set_property(object, &property, value)
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

                self.module.ins().call_property(object, property, args)
            }
            _ => {
                let func = self.build_expression(*func);

                self.module.ins().make_call(func, args)
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

                self.module.ins().assign(object, value);
                value
            }
        }
    }

    fn build_closure(&mut self, expr: ClosureExpression) -> ValueRef {
        let ClosureExpression { params, body } = expr;

        let old_block = self.module.current_block();
        self.symbol_table.enter_scope();

        let closure_block = self.module.create_block(None::<&str>);
        let func = self.module.ins().make_function(closure_block);
        self.module.switch_to_block(closure_block);

        for (idx, param) in params.into_iter().enumerate() {
            let value = self.module.ins().load_argument(idx);
            self.symbol_table.define(param.name, value);
        }

        for stmt in body.into_iter() {
            self.build_statement(stmt);
        }

        self.module.switch_to_block(old_block.unwrap());
        self.symbol_table.leave_scope();

        func
    }

    fn build_literal(&mut self, literal: LiteralExpression) -> ValueRef {
        let value = match literal {
            LiteralExpression::Boolean(b) => Value::Boolean(b),
            LiteralExpression::Integer(i) => Value::Integer(i),
            LiteralExpression::Float(f) => Value::Float(f),
            LiteralExpression::Char(c) => Value::Char(c),
            LiteralExpression::String(s) => Value::String(s),
        };

        self.module.ins().make_constant(value)
    }

    fn build_identifier(&mut self, identifier: IdentifierExpression) -> ValueRef {
        match self.symbol_table.get(&identifier.0) {
            Some(value) => value,
            None => {
                let value = self
                    .module
                    .ins()
                    .load_external_variable(identifier.0.clone());
                self.symbol_table.define(identifier.0, value);
                value
            }
        }
    }

    fn build_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> ValueRef {
        let lhs = self.build_expression(lhs);
        let rhs = self.build_expression(rhs);

        match op {
            BinOp::Add => self.module.ins().binop(Opcode::Add, lhs, rhs),
            BinOp::Sub => self.module.ins().binop(Opcode::Sub, lhs, rhs),
            BinOp::Mul => self.module.ins().binop(Opcode::Mul, lhs, rhs),
            BinOp::Div => self.module.ins().binop(Opcode::Div, lhs, rhs),
            BinOp::Mod => self.module.ins().binop(Opcode::Mod, lhs, rhs),

            BinOp::Equal => self.module.ins().binop(Opcode::Equal, lhs, rhs),
            BinOp::NotEqual => self.module.ins().binop(Opcode::NotEqual, lhs, rhs),
            BinOp::Greater => self.module.ins().binop(Opcode::Greater, lhs, rhs),
            BinOp::GreaterEqual => self.module.ins().binop(Opcode::GreaterEqual, lhs, rhs),
            BinOp::Less => self.module.ins().binop(Opcode::Less, lhs, rhs),
            BinOp::LessEqual => self.module.ins().binop(Opcode::LessEqual, lhs, rhs),

            _ => unimplemented!("{:?}", op),
        }
    }

    pub fn debug_instructions(&self) {
        self.module.debug();
    }
}

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<BTreeMap<String, ValueRef>>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            scopes: vec![BTreeMap::new()],
        }
    }

    fn get(&self, name: &str) -> Option<ValueRef> {
        let last = self.scopes.last()?;
        last.get(name).copied()
    }

    fn define(&mut self, name: String, value: ValueRef) {
        let last = self.scopes.last_mut().unwrap();
        last.insert(name, value);
    }

    fn enter_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
    }
}

struct Block {}
