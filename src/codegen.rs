use std::collections::BTreeMap;
use std::sync::Arc;

use crate::ast::*;
use crate::instruction::{Function, Module, Opcode, ValueRef};
use crate::irbuilder::{FunctionBuilder, InstBuilder};
use crate::value::Value;

pub struct Codegen {
    symbol_table: SymbolTable,
    module: Module,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            module: Module::new(),
        }
    }

    pub fn compile(mut self, ast: Program) -> Module {
        let func = Function::new(None);
        let mut func_builder = FunctionBuilder::new(&mut self.module, func);

        let entry = func_builder.create_block(Some("module".to_string()));
        func_builder.data_flow_graph_mut().set_entry(entry);
        func_builder.data_flow_graph_mut().switch_to_block(entry);

        let mut func_compiler = FunctionCompiler::new(func_builder, &mut self.symbol_table);

        for toplevel in ast.items {
            func_compiler.compile_toplevel(toplevel);
        }

        let func = func_compiler.finalize();

        let Function { name, dfg } = func;

        self.module.dfg = dfg;

        self.module
    }
}

pub struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> FunctionCompiler<'a> {
    pub fn new(builder: FunctionBuilder<'a>, symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            builder,
            symbol_table,
        }
    }

    pub fn finalize(self) -> Function {
        self.builder.finalize()
    }

    fn symbol_table(&mut self) -> &mut SymbolTable {
        &mut self.symbol_table
    }

    fn builder(&mut self) -> &mut FunctionBuilder<'a> {
        &mut self.builder
    }

    fn compile(mut self, func_item: FunctionItem) -> ValueRef {
        let FunctionItem {
            name,
            params,
            return_ty,
            body,
        } = func_item;

        let entry = self.builder().create_block(Some(name));
        self.builder().data_flow_graph_mut().set_entry(entry);
        self.builder().switch_to_block(entry);

        for (idx, param) in params.into_iter().enumerate() {
            let value = self.builder().load_argument(idx);
            self.symbol_table().define(param.name, value);
        }

        for stmt in body.into_iter() {
            self.compile_statement(stmt);
        }

        let Self {
            builder,
            symbol_table,
        } = self;

        let func = builder.module.add_function(builder.func);

        ValueRef::Function(func)
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
                self.builder().return_(value);
            }

            Statement::If(if_stmt) => {
                self.compile_if_stmt(if_stmt);
            }

            _ => unimplemented!("{:?}", statement),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: LetStatement) {
        let LetStatement { name, ty, value } = let_stmt;

        match value {
            Some(expr) => {
                let value = self.compile_expression(expr);
                let dest = self.builder().make_inst_value();
                self.builder().assign(dest, value);
                self.symbol_table().define(name, dest);
            }
            None => {
                let dest = self.builder().make_inst_value();
                self.symbol_table().define(name, dest);
            }
        }
    }

    fn compile_item_stmt(&mut self, item: ItemStatement) {
        match item {
            ItemStatement::Fn(fn_item) => {
                self.compile_function(fn_item);
            }
            _ => unimplemented!("statement {:?}", item),
        }
    }

    fn compile_if_stmt(&mut self, if_stmt: IfStatement) {
        let IfStatement {
            condition,
            then_branch,
            else_branch,
        } = if_stmt;

        let then_blk = self.builder().create_block(None);
        let else_blk = else_branch
            .as_ref()
            .map(|_| self.builder().create_block(None));
        let next_blk = self.builder().create_block(None);

        let cond = self.compile_expression(condition);
        self.builder().br_if(cond, then_blk, else_blk);

        self.builder().switch_to_block(then_blk);
        self.compile_block(then_branch);
        self.builder().br(next_blk);

        else_branch.map(|block| {
            let else_blk = else_blk.unwrap();
            self.builder().switch_to_block(else_blk);
            self.compile_block(block);
            self.builder().br(next_blk);
        });

        self.builder().switch_to_block(next_blk);
    }

    fn compile_block(&mut self, block: Vec<Statement>) {
        for statement in block {
            self.compile_statement(statement);
        }
    }

    fn compile_function(&mut self, fn_item: FunctionItem) -> ValueRef {
        let mut symbols = self.symbol_table().new_scope();

        let func_name = if fn_item.name.is_empty() {
            None
        } else {
            Some(fn_item.name.clone())
        };

        let func = Function::new(func_name.clone());

        let builder = FunctionBuilder::new(&mut self.builder.module, func);

        let compiler = FunctionCompiler::new(builder, &mut symbols);

        let func = compiler.compile(fn_item);
        if let Some(name) = func_name {
            self.symbol_table().define(name, func);
        }

        func
    }

    fn compile_expression(&mut self, expr: Expression) -> ValueRef {
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

        self.builder().get_property(object, &property)
    }

    fn compile_set_property(&mut self, expr: MemberExpression, value: ValueRef) {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder().set_property(object, &property, value)
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

                self.builder().call_property(object, property, args)
            }
            _ => {
                let func = self.compile_expression(*func);

                self.builder().make_call(func, args)
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
                    Some(BinOp::Add) => self.builder().binop(Opcode::Add, old, value),
                    Some(BinOp::Sub) => self.builder().binop(Opcode::Sub, old, value),
                    Some(BinOp::Mul) => self.builder().binop(Opcode::Mul, old, value),
                    Some(BinOp::Div) => self.builder().binop(Opcode::Div, old, value),
                    Some(BinOp::Mod) => self.builder().binop(Opcode::Mod, old, value),
                    None => old,
                    _ => unreachable!(),
                };

                self.compile_set_property(member, value);
                value
            }
            _ => {
                let object = self.compile_expression(*object);
                let value = match op {
                    Some(BinOp::Add) => self.builder().binop(Opcode::Add, object, value),
                    Some(BinOp::Sub) => self.builder().binop(Opcode::Sub, object, value),
                    Some(BinOp::Mul) => self.builder().binop(Opcode::Mul, object, value),
                    Some(BinOp::Div) => self.builder().binop(Opcode::Div, object, value),
                    Some(BinOp::Mod) => self.builder().binop(Opcode::Mod, object, value),
                    None => object,
                    _ => unreachable!(),
                };

                self.builder().assign(object, value);
                value
            }
        }
    }

    fn compile_closure(&mut self, expr: ClosureExpression) -> ValueRef {
        let mut symbols = self.symbol_table().new_scope();

        let func = Function::new(None);

        let mut builder = FunctionBuilder::new(&mut self.builder.module, func);

        let mut compiler = FunctionCompiler::new(builder, &mut symbols);

        let fn_item = FunctionItem {
            name: String::new(),
            params: expr.params,
            return_ty: None,
            body: expr.body,
        };

        let func = compiler.compile(fn_item);

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

        self.builder().make_constant(value)
    }

    fn compile_identifier(&mut self, identifier: IdentifierExpression) -> ValueRef {
        match self.symbol_table().get(&identifier.0) {
            Some(value) => value,
            None => {
                let value = self.builder().load_external_variable(identifier.0.clone());
                self.symbol_table().define(identifier.0, value);
                value
            }
        }
    }

    fn compile_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> ValueRef {
        let lhs = self.compile_expression(lhs);
        let rhs = self.compile_expression(rhs);

        match op {
            BinOp::Add => self.builder().binop(Opcode::Add, lhs, rhs),
            BinOp::Sub => self.builder().binop(Opcode::Sub, lhs, rhs),
            BinOp::Mul => self.builder().binop(Opcode::Mul, lhs, rhs),
            BinOp::Div => self.builder().binop(Opcode::Div, lhs, rhs),
            BinOp::Mod => self.builder().binop(Opcode::Mod, lhs, rhs),

            BinOp::Equal => self.builder().binop(Opcode::Equal, lhs, rhs),
            BinOp::NotEqual => self.builder().binop(Opcode::NotEqual, lhs, rhs),
            BinOp::Greater => self.builder().binop(Opcode::Greater, lhs, rhs),
            BinOp::GreaterEqual => self.builder().binop(Opcode::GreaterEqual, lhs, rhs),
            BinOp::Less => self.builder().binop(Opcode::Less, lhs, rhs),
            BinOp::LessEqual => self.builder().binop(Opcode::LessEqual, lhs, rhs),

            _ => unimplemented!("{:?}", op),
        }
    }
}

#[derive(Debug, Clone)]
struct SymbolTable {
    parent: Option<Box<SymbolTable>>,
    symbols: BTreeMap<String, ValueRef>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            parent: None,
            symbols: BTreeMap::new(),
        }
    }

    fn is_top_level(&self) -> bool {
        self.parent.is_none()
    }

    fn get(&self, name: &str) -> Option<ValueRef> {
        if let Some(value) = self.symbols.get(name) {
            return Some(value.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.get(name);
        }
        None
    }

    fn define(&mut self, name: String, value: ValueRef) {
        self.symbols.insert(name, value);
    }

    fn new_scope(&self) -> SymbolTable {
        Self {
            parent: Some(Box::new(self.clone())),
            symbols: BTreeMap::new(),
        }
    }
}
