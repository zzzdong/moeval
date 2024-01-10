use std::collections::BTreeMap;

use crate::ast::*;
use crate::instruction::{Module, Opcode, ValueRef};
use crate::irbuilder::IRBuilder;
use crate::value::Value;

pub struct Codegen<'a> {
    builder: IRBuilder<'a>,
    symbol_table: SymbolTable,
}

impl<'a> Codegen<'a> {
    pub fn new(ir_builder: IRBuilder<'a>) -> Self {
        Self {
            builder: ir_builder,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn compile(&mut self, ast: Program) {
        let block = self.builder.create_block(Some("entry"));
        self.builder.set_entry_block(block);
        self.builder.switch_to_block(block);

        for item in ast.items {
            self.compile_toplevel(item);
        }
    }

    fn compile_toplevel(&mut self, toplevel: TopLevelItem) {
        match toplevel {
            TopLevelItem::Expression(expr) => {
                self.compile_expression(expr);
            }
            TopLevelItem::Statement(stmt) => {
                self.compile_statement(stmt);
            }
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression);
            }
            Statement::Let(let_stmt) => {
                self.compile_let_stmt(let_stmt);
            }
            Statement::Item(stmt) => {
                self.compile_item_stmt(stmt);
            }
            Statement::Return(ReturnStatement { value }) => {
                let value = value.map(|expr| self.compile_expression(expr));
                self.builder.return_(value);
            }

            Statement::If(if_stmt) => {
                self.compile_if_stmt(if_stmt);
            }

            _ => unimplemented!("{:?}", statement),
        }
    }

    pub fn compile_let_stmt(&mut self, let_stmt: LetStatement) {
        let LetStatement { name, ty, value } = let_stmt;

        let value = value.map(|expr| self.compile_expression(expr));
        let value = value.unwrap_or_else(|| self.builder.make_inst_value());
        self.symbol_table.define(name, value);
    }

    pub fn compile_item_stmt(&mut self, item: ItemStatement) {
        match item {
            ItemStatement::Fn(fn_item) => {
                self.compile_function(fn_item);
            }
            _ => unimplemented!("statement {:?}", item),
        }
    }

    pub fn compile_if_stmt(&mut self, if_stmt: IfStatement) {
        let IfStatement {
            condition,
            then_branch,
            else_branch,
        } = if_stmt;

        let then_blk = self.builder.create_block(None::<&str>);
        let else_blk = else_branch
            .as_ref()
            .map(|_| self.builder.create_block(None::<&str>));
        let next_blk = self.builder.create_block(None::<&str>);

        let cond = self.compile_expression(condition);
        self.builder.br_if(cond, then_blk, else_blk);

        self.builder.switch_to_block(then_blk);
        self.compile_block(then_branch);
        self.builder.cfg().set_block_after(next_blk, then_blk);

        else_branch.map(|block| {
            let else_blk = else_blk.unwrap();
            self.builder.switch_to_block(else_blk);
            self.compile_block(block);
            self.builder.cfg().set_block_after(next_blk, else_blk);
        });

        self.builder.switch_to_block(next_blk);
    }

    pub fn compile_block(&mut self, block: Vec<Statement>) {
        for statement in block {
            self.compile_statement(statement);
        }
    }

    pub fn compile_function(&mut self, fn_item: FunctionItem) -> ValueRef {
        let FunctionItem {
            name,
            params,
            return_ty,
            body,
        } = fn_item;

        self.symbol_table.enter_scope();
        let block = self.builder.create_block(Some(name.clone()));
        self.builder.switch_to_block(block);

        let func = self.builder.create_function(Some(name), block);

        for (idx, param) in params.into_iter().enumerate() {
            let value = self.builder.load_argument(idx);
            self.symbol_table.define(param.name, value);
        }

        for stmt in body.into_iter() {
            self.compile_statement(stmt);
        }

        self.symbol_table.exit_scope();

        let next_blk = self.builder.create_block(None::<&str>);
        self.builder.switch_to_block(block);

        func
    }

    pub fn compile_expression(&mut self, expr: Expression) -> ValueRef {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(identifier),
            Expression::Binary(op, lhs, rhs) => self.compile_binary(op, *lhs, *rhs),
            Expression::Member(member) => self.compile_get_property(member),
            Expression::Call(call) => self.compile_call(call),
            Expression::Assign(assign) => self.compile_assign(assign),
            Expression::Closure(closure) => self.compile_closure(closure),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn compile_get_property(&mut self, expr: MemberExpression) -> ValueRef {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.get_property(object, &property)
    }

    fn compile_set_property(&mut self, expr: MemberExpression, value: ValueRef) {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.set_property(object, &property, value)
    }

    fn compile_call(&mut self, expr: CallExpression) -> ValueRef {
        let CallExpression { func, args } = expr;

        let args: Vec<ValueRef> = args
            .into_iter()
            .map(|arg| self.compile_expression(arg))
            .collect();

        match *func {
            Expression::Member(member) => {
                let MemberExpression { object, property } = member;

                let object = self.compile_expression(*object);

                self.builder.call_property(object, property, args)
            }
            _ => {
                let func = self.compile_expression(*func);

                self.builder.make_call(func, args)
            }
        }
    }

    fn compile_assign(&mut self, expr: AssignExpression) -> ValueRef {
        let AssignExpression { object, value, op } = expr;

        let value = self.compile_expression(*value);

        match *object {
            Expression::Member(member) => {
                let old = self.compile_get_property(member.clone());
                let value = match op {
                    Some(BinOp::Add) => self.builder.binop(Opcode::Add, old, value),
                    Some(BinOp::Sub) => self.builder.binop(Opcode::Sub, old, value),
                    Some(BinOp::Mul) => self.builder.binop(Opcode::Mul, old, value),
                    Some(BinOp::Div) => self.builder.binop(Opcode::Div, old, value),
                    Some(BinOp::Mod) => self.builder.binop(Opcode::Mod, old, value),
                    None => old,
                    _ => unreachable!(),
                };

                self.compile_set_property(member, value);
                value
            }
            _ => {
                let object = self.compile_expression(*object);
                let value = match op {
                    Some(BinOp::Add) => self.builder.binop(Opcode::Add, object, value),
                    Some(BinOp::Sub) => self.builder.binop(Opcode::Sub, object, value),
                    Some(BinOp::Mul) => self.builder.binop(Opcode::Mul, object, value),
                    Some(BinOp::Div) => self.builder.binop(Opcode::Div, object, value),
                    Some(BinOp::Mod) => self.builder.binop(Opcode::Mod, object, value),
                    None => object,
                    _ => unreachable!(),
                };

                self.builder.assign(object, value);
                value
            }
        }
    }

    fn compile_closure(&mut self, expr: ClosureExpression) -> ValueRef {
        let ClosureExpression { params, body } = expr;

        self.symbol_table.enter_scope();
        let block = self.builder.create_block(None::<&str>);
        self.builder.switch_to_block(block);

        let func = self.builder.create_function(None::<&str>, block);

        for (idx, param) in params.into_iter().enumerate() {
            let value = self.builder.load_argument(idx);
            self.symbol_table.define(param.name, value);
        }

        for stmt in body.into_iter() {
            self.compile_statement(stmt);
        }

        self.symbol_table.exit_scope();

        let next_blk = self.builder.create_block(None::<&str>);
        self.builder.switch_to_block(block);

        func
    }

    fn compile_literal(&mut self, literal: LiteralExpression) -> ValueRef {
        let value = match literal {
            LiteralExpression::Boolean(b) => Value::Boolean(b),
            LiteralExpression::Integer(i) => Value::Integer(i),
            LiteralExpression::Float(f) => Value::Float(f),
            LiteralExpression::Char(c) => Value::Char(c),
            LiteralExpression::String(s) => Value::String(s),
        };

        self.builder.make_constant(value)
    }

    fn compile_identifier(&mut self, identifier: IdentifierExpression) -> ValueRef {
        match self.symbol_table.get(&identifier.0) {
            Some(value) => value,
            None => {
                let value = self.builder.load_external_variable(identifier.0.clone());
                self.symbol_table.define(identifier.0, value);
                value
            }
        }
    }

    fn compile_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> ValueRef {
        let lhs = self.compile_expression(lhs);
        let rhs = self.compile_expression(rhs);

        match op {
            BinOp::Add => self.builder.binop(Opcode::Add, lhs, rhs),
            BinOp::Sub => self.builder.binop(Opcode::Sub, lhs, rhs),
            BinOp::Mul => self.builder.binop(Opcode::Mul, lhs, rhs),
            BinOp::Div => self.builder.binop(Opcode::Div, lhs, rhs),
            BinOp::Mod => self.builder.binop(Opcode::Mod, lhs, rhs),

            BinOp::Equal => self.builder.binop(Opcode::Equal, lhs, rhs),
            BinOp::NotEqual => self.builder.binop(Opcode::NotEqual, lhs, rhs),
            BinOp::Greater => self.builder.binop(Opcode::Greater, lhs, rhs),
            BinOp::GreaterEqual => self.builder.binop(Opcode::GreaterEqual, lhs, rhs),
            BinOp::Less => self.builder.binop(Opcode::Less, lhs, rhs),
            BinOp::LessEqual => self.builder.binop(Opcode::LessEqual, lhs, rhs),

            _ => unimplemented!("{:?}", op),
        }
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

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}
