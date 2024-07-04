use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::vec;

use crate::ast::*;
use crate::ir::builder::{Builder, FuncParam, FuncSignature, Module};
use crate::ir::instruction::{Address, Opcode};
use crate::ir::types::*;

pub struct Codegen {
    module: Module,
}

impl Codegen {
    pub fn compile(ast: Program) -> Module {
        let mut module = Module::new();
        let mut context = module.make_context();
        let mut ir_builder = Builder::new(&mut context, &mut module);

        let mut func_compiler = FunctionCompiler::new(&mut ir_builder, SymbolTable::new());

        let entry = func_compiler.compile_function_id(None, vec![], ast.stmts);

        module.set_entry(entry);

        module
    }
}

enum PatternValue {
    Wildcard(Address),
    Variable(Address),
    Tuple {
        value: Address,
        fields: Vec<PatternValue>,
    },
}

pub struct FunctionCompiler<'short, 'long: 'short> {
    builder: &'short mut Builder<'long>,
    symbols: SymbolTable,
}

impl<'short, 'long: 'short> FunctionCompiler<'short, 'long> {
    pub fn new(builder: &'short mut Builder<'long>, symbols: SymbolTable) -> Self {
        Self { builder, symbols }
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
            Statement::For(for_stmt) => {
                self.compile_for_stmt(for_stmt);
            }

            _ => unimplemented!("{:?}", statement),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: LetStatement) {
        let LetStatement { name, ty, value } = let_stmt;

        match value {
            Some(expr) => {
                let value = self.compile_expression(expr);
                let dst = self.builder.create_alloc();
                self.builder.assign(dst, value);
                self.symbols.declare(&name, dst);
            }
            None => {
                let dst = self.builder.create_alloc();
                self.symbols.declare(&name, dst);
            }
        }
    }

    fn compile_item_stmt(&mut self, item: ItemStatement) {
        match item {
            ItemStatement::Fn(fn_item) => {
                self.compile_function_item(fn_item);
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

        let true_blk = self.builder.create_block(None);
        let false_blk = else_branch
            .as_ref()
            .map(|_| self.builder.create_block(None));
        let next_blk = self.builder.create_block(None);

        let cond = self.compile_expression(condition);
        self.builder
            .br_if(cond, true_blk, false_blk.unwrap_or(next_blk));

        self.builder.switch_to_block(true_blk);
        self.compile_block(then_branch);
        self.builder.br(next_blk);

        if let Some(block) = else_branch {
            let else_blk = false_blk.unwrap();
            self.builder.switch_to_block(else_blk);
            self.compile_block(block);
            self.builder.br(next_blk);
        }

        self.builder.switch_to_block(next_blk);
    }

    fn compile_pattern(&mut self, pat: Pattern, value: Address) {
        match pat {
            Pattern::Wildcard => {}
            Pattern::Identifier(ident) => {
                let dst = self.builder.create_alloc();
                self.builder.assign(dst, value);
                self.symbols.declare(&ident, dst);
            }

            Pattern::Tuple(pats) => {
                for (i, pat) in pats.iter().enumerate() {
                    let index = self.builder.make_constant(Primitive::Integer(i as i64));
                    let field = self.builder.index_get(value, index);
                    self.compile_pattern(pat.clone(), field);
                }
            }
            _ => {
                unimplemented!("Unsupport pattern");
            }
        }
    }

    fn compile_for_stmt(&mut self, for_stmt: ForStatement) {
        let ForStatement {
            pat,
            iterable,
            body,
        } = for_stmt;

        let next = self.builder.create_alloc();

        let loop_blk = self.builder.create_block(None);
        let after_blk = self.builder.create_block(None);

        let iterable = self.compile_expression(iterable);
        let iterable = self.builder.make_iterator(iterable);
        self.builder.br(loop_blk);

        self.builder.switch_to_block(loop_blk);

        self.builder.iterate_next(iterable, next, after_blk);

        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);

        self.compile_pattern(pat, next);

        for statement in body {
            self.compile_statement(statement);
        }
        self.symbols = old_symbols;

        self.builder.br(loop_blk);

        self.builder.switch_to_block(after_blk);
    }

    fn compile_block(&mut self, block: Vec<Statement>) {
        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);
        for statement in block {
            self.compile_statement(statement);
        }
        self.symbols = old_symbols;
    }

    fn compile_function_item(&mut self, fn_item: FunctionItem) -> Address {
        let FunctionItem {
            name,
            params,
            return_ty,
            body,
        } = fn_item;

        self.compile_function(Some(name), params, body)
    }

    fn compile_function(
        &mut self,
        name: Option<String>,
        params: Vec<crate::ast::FunctionParam>,
        body: Vec<Statement>,
    ) -> Address {
        let id = self.compile_function_id(name, params, body);
        Address::Function(id)
    }

    fn compile_function_id(
        &mut self,
        name: Option<String>,
        params: Vec<crate::ast::FunctionParam>,
        body: Vec<Statement>,
    ) -> FunctionId {
        let func_sig = FuncSignature::new(
            name.clone(),
            params
                .iter()
                .map(|p| FuncParam::new(p.name.clone()))
                .collect(),
        );
        let func_id = self.builder.module.declare_function(func_sig);
        if let Some(name) = &name {
            self.symbols.declare(name, Address::Function(func_id));
        }

        let symbols = self.symbols.new_scope();

        let context = {
            let mut context = self.builder.module.make_context();
            let mut builder = Builder::new(&mut context, &mut self.builder.module);

            let mut compiler = FunctionCompiler::new(&mut builder, symbols);

            let entry = compiler.builder.create_block(None);
            compiler.builder.set_entry_block(entry);
            compiler.builder.switch_to_block(entry);

            for (idx, param) in params.iter().enumerate() {
                let arg = compiler.builder.load_argument(idx);
                compiler.symbols.declare(param.name.as_str(), arg);
            }

            for stmt in body {
                compiler.compile_statement(stmt);
            }

            context
        };

        self.builder.module.define_function(func_id, context);

        func_id
    }

    fn compile_expression(&mut self, expr: Expression) -> Address {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(identifier),
            Expression::Binary(BinOp::Range, lhs, rhs) => self.compile_range(*lhs, *rhs),
            Expression::Binary(op, lhs, rhs) => self.compile_binary(op, *lhs, *rhs),
            Expression::Member(member) => self.compile_get_property(member),
            Expression::Call(call) => self.compile_call(call),
            Expression::Assign(assign) => self.compile_assign(assign),
            Expression::Closure(closure) => self.compile_closure(closure),
            Expression::Array(array) => self.compile_array(array),
            Expression::Index(index) => self.compile_index(index),
            Expression::Map(map) => self.compile_map(map),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn compile_get_property(&mut self, expr: MemberExpression) -> Address {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.get_property(object, &property)
    }

    fn compile_set_property(&mut self, expr: MemberExpression, value: Address) {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.set_property(object, &property, value)
    }

    fn compile_call(&mut self, expr: CallExpression) -> Address {
        let CallExpression { func, args } = expr;

        let args: Vec<Address> = args
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

    fn compile_assign(&mut self, expr: AssignExpression) -> Address {
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
            Expression::Index(expr) => {
                let IndexExpression { object, index } = expr;
                let object = self.compile_expression(*object);
                let index = self.compile_expression(*index);

                self.builder.index_set(object, index, value);
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

    fn compile_closure(&mut self, expr: ClosureExpression) -> Address {
        let ClosureExpression { params, body } = expr;

        self.compile_function(None, params, body)
    }

    fn compile_array(&mut self, expr: ArrayExpression) -> Address {
        let ArrayExpression(elements) = expr;
        let array = self.builder.new_array(Some(elements.len()));

        for element in elements {
            let elem = self.compile_expression(element);
            self.builder.array_push(array, elem);
        }

        array
    }

    fn compile_map(&mut self, expr: MapExpression) -> Address {
        let MapExpression(elements) = expr;
        let map = self.builder.new_map();

        for (key, value) in elements {
            let key = self.compile_literal(key.as_literal().unwrap());
            let elem = self.compile_expression(value);
            self.builder.index_set(map, key, elem);
        }

        map
    }

    fn compile_index(&mut self, expr: IndexExpression) -> Address {
        let IndexExpression { object, index } = expr;

        let object = self.compile_expression(*object);
        let index = self.compile_expression(*index);
        self.builder.index_get(object, index)
    }

    fn compile_literal(&mut self, literal: LiteralExpression) -> Address {
        let value = match literal {
            LiteralExpression::Boolean(b) => Primitive::Boolean(b),
            LiteralExpression::Integer(i) => Primitive::Integer(i),
            LiteralExpression::Float(f) => Primitive::Float(f),
            LiteralExpression::Char(c) => Primitive::Char(c),
            LiteralExpression::String(s) => Primitive::String(s),
        };

        self.builder.make_constant(value)
    }

    fn compile_identifier(&mut self, identifier: IdentifierExpression) -> Address {
        match self.symbols.get(&identifier.0) {
            Some(value) => value,
            None => {
                let value = self.builder.load_external_variable(identifier.0.clone());
                self.symbols.declare(&identifier.0, value);
                value
            }
        }
    }

    fn compile_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> Address {
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

            BinOp::LogicAnd => self.builder.binop(Opcode::And, lhs, rhs),
            BinOp::LogicOr => self.builder.binop(Opcode::Or, lhs, rhs),

            _ => unimplemented!("{:?}", op),
        }
    }

    fn compile_range(&mut self, lhs: Expression, rhs: Expression) -> Address {
        let begin = self.compile_expression(lhs);
        let end = self.compile_expression(rhs);
        self.builder.range(begin, end)
    }
}

// #[derive(Debug, Clone)]
// struct SymbolTable {
//     parent: Option<Box<SymbolTable>>,
//     symbols: BTreeMap<String, ValueRef>,
// }

// impl SymbolTable {
//     fn new() -> Self {
//         SymbolTable {
//             parent: None,
//             symbols: BTreeMap::new(),
//         }
//     }

//     fn is_top_level(&self) -> bool {
//         self.parent.is_none()
//     }

//     fn get(&self, name: &str) -> Option<ValueRef> {
//         if let Some(value) = self.symbols.get(name) {
//             return Some(value.clone());
//         }
//         if let Some(parent) = &self.parent {
//             return parent.get(name);
//         }
//         None
//     }

//     fn declare(&mut self, name: impl Into<String>, value: ValueRef) {
//         self.symbols.insert(name.into(), value);
//     }

//     fn new_scope(&self) -> SymbolTable {
//         Self {
//             parent: Some(Box::new(self.clone())),
//             symbols: BTreeMap::new(),
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct SymbolNode {
    parent: Option<SymbolTable>,
    symbols: BTreeMap<String, Address>,
}

#[derive(Debug, Clone)]
pub struct SymbolTable(Rc<RefCell<SymbolNode>>);

impl SymbolTable {
    fn new() -> Self {
        SymbolTable(Rc::new(RefCell::new(SymbolNode {
            parent: None,
            symbols: BTreeMap::new(),
        })))
    }

    fn get(&self, name: &str) -> Option<Address> {
        if let Some(value) = self.0.borrow().symbols.get(name) {
            return Some(*value);
        }
        if let Some(parent) = &self.0.borrow().parent {
            return parent.get(name);
        }
        None
    }

    fn declare(&mut self, name: impl Into<String>, value: Address) {
        self.0.borrow_mut().symbols.insert(name.into(), value);
    }

    fn new_scope(&self) -> SymbolTable {
        SymbolTable(Rc::new(RefCell::new(SymbolNode {
            parent: Some(self.clone()),
            symbols: BTreeMap::new(),
        })))
    }
}
