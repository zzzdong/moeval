use crate::ast::*;
use crate::ir::*;
use crate::vm::Primitive;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Semantics(String),
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Parse(error) => write!(f, "Parse error: {}", error),
            CompileError::Semantics(message) => write!(f, "Semantics error: {}", message),
        }
    }
}

impl std::error::Error for CompileError {}

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, input: &str) -> Result<Module, CompileError> {
        // 解析输入
        let ast = parse_file(input)?;
        // println!("{:#?}", ast);
        // // 语义分析
        // let ast = crate::semantics::analyze(ast)?;

        let mut inst = Inst::new();

        let builder: &mut dyn InstBuilder = &mut ModuleBuilder::new(&mut inst);

        let entry = builder.create_block("__entry".into());
        builder.switch_to_block(entry);

        let mut inst_compiler = InstCompiler::new(builder, SymbolTable::new());

        inst_compiler.compile_statements(ast.stmts);

        let mut pass_manager = InstPassManager::new();

        pass_manager.add_pass(SimplifyPass::new());

        let inst = pass_manager.run(inst);

        log::trace!("{}", inst);

        let module = inst.into_module(None);

        Ok(module)
    }
}

struct LoopContext {
    pub(crate) break_point: BlockId,
    pub(crate) continue_point: BlockId,
}

impl LoopContext {
    pub(crate) fn new(break_point: BlockId, continue_point: BlockId) -> Self {
        Self {
            break_point,
            continue_point,
        }
    }
}

pub struct InstCompiler<'a> {
    builder: &'a mut dyn InstBuilder,
    symbols: SymbolTable,
    loop_contexts: Vec<LoopContext>,
}

impl<'a> InstCompiler<'a> {
    pub fn new(builder: &'a mut dyn InstBuilder, symbols: SymbolTable) -> Self {
        Self {
            builder,
            symbols,
            loop_contexts: Vec::new(),
        }
    }

    fn compile_statements(&mut self, stmts: Vec<Statement>) {
        let entry = self.create_block("main");
        self.builder.switch_to_block(entry);
        self.builder.set_entry(entry);

        for stmt in stmts {
            self.compile_statement(stmt);
        }

        // FIXME: This is a hack to make block not empty.
        self.builder.make_halt();
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
            Statement::Loop(loop_stmt) => {
                self.compile_loop_stmt(loop_stmt);
            }
            Statement::For(for_stmt) => {
                self.compile_for_stmt(for_stmt);
            }
            Statement::Break => {
                self.compile_break_stmt();
            }
            Statement::Continue => {
                self.compile_continue_stmt();
            }

            _ => unimplemented!("{:?}", statement),
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: LetStatement) {
        let LetStatement { name, ty: _, value } = let_stmt;

        let dst = self.builder.create_alloc();

        if let Some(value) = value {
            let value = self.compile_expression(value);
            self.builder.assign(dst, value);
        }

        self.symbols.declare(&name, Symbol::new(dst));
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

        let merge_blk = self.create_block("if_merge");
        let then_blk = self.create_block("if_then");
        let else_blk = else_branch.as_ref().map(|_| self.create_block("if_else"));

        let cond = self.compile_expression(condition);
        self.builder
            .br_if(cond, then_blk, else_blk.unwrap_or(merge_blk));

        self.builder.switch_to_block(then_blk);
        self.compile_block(then_branch);
        self.builder.br(merge_blk);

        if let Some(block) = else_branch {
            let else_blk = else_blk.unwrap();
            self.builder.switch_to_block(else_blk);
            self.compile_block(block);
            self.builder.br(merge_blk);
        }

        self.builder.switch_to_block(merge_blk);
    }

    fn compile_pattern(&mut self, pat: Pattern, value: Operand) {
        match pat {
            Pattern::Wildcard => {}
            Pattern::Identifier(ident) => {
                let dst = self.builder.create_alloc();
                self.builder.assign(dst, value);
                self.symbols.declare(&ident, Symbol::new(dst));
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

        let loop_init = self.create_block("loop_init");
        let loop_header = self.create_block("loop_header");
        let loop_body = self.create_block("iterate");
        let after_blk = self.create_block(None);

        self.enter_loop_context(after_blk, loop_header);

        self.builder.br(loop_init);

        // loop init, create iterator
        self.builder.switch_to_block(loop_init);
        let iterable = self.compile_expression(iterable);
        let iterable = self.builder.make_iterator(iterable);
        self.builder.br(loop_header);

        // loop header, check if iterator has next
        self.builder.switch_to_block(loop_header);
        let has_next = self.builder.iterator_has_next(iterable);
        self.builder.br_if(has_next, loop_body, after_blk);

        // loop body, get next value
        self.builder.switch_to_block(loop_body);
        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);
        let next = self.builder.iterate_next(iterable);
        self.compile_pattern(pat, next);

        for statement in body {
            self.compile_statement(statement);
        }
        self.symbols = old_symbols;

        self.builder.br(loop_header);

        // done loop
        self.level_loop_context();
        self.builder.switch_to_block(after_blk);
    }

    fn compile_loop_stmt(&mut self, loop_stmt: LoopStatement) {
        let LoopStatement { body } = loop_stmt;

        let loop_body = self.create_block("loop_body");
        let after_blk = self.create_block(None);

        self.enter_loop_context(after_blk, loop_body);

        self.builder.br(loop_body);
        self.builder.switch_to_block(loop_body);
        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);

        for statement in body {
            self.compile_statement(statement);
        }

        self.symbols = old_symbols;

        self.builder.br(loop_body);

        // done loop
        self.level_loop_context();
        self.builder.switch_to_block(after_blk);
    }

    fn compile_break_stmt(&mut self) {
        self.builder.br(self.loop_context().break_point);
    }

    fn compile_continue_stmt(&mut self) {
        self.builder.br(self.loop_context().continue_point);
    }

    fn compile_block(&mut self, block: Vec<Statement>) {
        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);
        for statement in block {
            self.compile_statement(statement);
        }
        self.symbols = old_symbols;
    }

    fn compile_function_item(&mut self, fn_item: FunctionItem) -> Operand {
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
        params: Vec<FunctionParam>,
        body: Vec<Statement>,
    ) -> Operand {
        let curr = self.builder.current_block();

        let func_sig = FuncSignature::new(
            name.clone(),
            params
                .iter()
                .map(|p| FuncParam::new(p.name.clone()))
                .collect(),
        );
        let func_id = self.builder.module_mut().declare_function(func_sig.clone());

        let mut func = Function::new(func_id, func_sig);

        let symbols = self.symbols.new_scope();

        let mut func_builder = FunctionBuilder::new(self.builder.module_mut(), &mut func);

        let mut func_compiler = InstCompiler::new(&mut func_builder, symbols);

        let entry = func_compiler.create_block(name);
        func_compiler.builder.set_entry(entry);
        func_compiler.builder.switch_to_block(entry);

        for (idx, param) in params.iter().enumerate() {
            let arg = func_compiler.builder.load_argument(idx);
            func_compiler
                .symbols
                .declare(param.name.as_str(), Symbol::new(arg));
        }

        for stmt in body {
            func_compiler.compile_statement(stmt);
        }

        // append return instruction
        func_compiler.builder.return_(None);

        self.builder.module_mut().define_function(func_id, func);

        self.builder.switch_to_block(curr);

        Operand::Function(func_id)
    }

    fn compile_expression(&mut self, expr: Expression) -> Operand {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifier(identifier) => self.compile_identifier(identifier),
            Expression::Prefix(op, expr) => self.compile_unary(op, *expr),
            Expression::Binary(BinOp::Range, lhs, rhs) => self.compile_range(*lhs, *rhs, false),
            Expression::Binary(BinOp::RangeInclusive, lhs, rhs) => {
                self.compile_range(*lhs, *rhs, true)
            }
            Expression::Binary(op, lhs, rhs) => self.compile_binary(op, *lhs, *rhs),
            Expression::Member(member) => self.compile_get_property(member),
            Expression::Call(call) => self.compile_call(call),
            Expression::Assign(assign) => self.compile_assign(assign),
            Expression::Closure(closure) => self.compile_closure(closure),
            Expression::Array(array) => self.compile_array(array),
            Expression::Index(index) => self.compile_index(index),
            Expression::Map(map) => self.compile_map(map),
            Expression::Slice(slice) => self.compile_slice(slice),
            Expression::Await(expr) => self.compile_await(*expr),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn compile_get_property(&mut self, expr: MemberExpression) -> Operand {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.get_property(object, &property)
    }

    fn compile_set_property(&mut self, expr: MemberExpression, value: Operand) {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.set_property(object, &property, value)
    }

    fn compile_call(&mut self, expr: CallExpression) -> Operand {
        let CallExpression { func, args } = expr;

        let args: Vec<Operand> = args
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

    fn compile_assign(&mut self, expr: AssignExpression) -> Operand {
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
                    None => value,
                    _ => unreachable!(),
                };

                self.builder.assign(object, value);
                value
            }
        }
    }

    fn compile_closure(&mut self, expr: ClosureExpression) -> Operand {
        let ClosureExpression { params, body } = expr;

        self.compile_function(None, params, body)
    }

    fn compile_array(&mut self, expr: ArrayExpression) -> Operand {
        let ArrayExpression(elements) = expr;
        let array = self.builder.new_array(Some(elements.len()));

        for element in elements {
            let elem = self.compile_expression(element);
            self.builder.array_push(array, elem);
        }

        array
    }

    fn compile_map(&mut self, expr: MapExpression) -> Operand {
        let MapExpression(elements) = expr;
        let map = self.builder.new_map();

        for (key, value) in elements {
            let key = self.compile_literal(key.as_literal().unwrap());
            let elem = self.compile_expression(value);
            self.builder.index_set(map, key, elem);
        }

        map
    }

    fn compile_await(&mut self, expr: Expression) -> Operand {
        let promise = self.compile_expression(expr);

        self.builder.await_promise(promise)
    }

    fn compile_index(&mut self, expr: IndexExpression) -> Operand {
        let IndexExpression { object, index } = expr;

        let object = self.compile_expression(*object);

        match *index {
            Expression::Binary(BinOp::Range, begin, end) => {
                let begin = self.compile_expression(*begin);
                let end = self.compile_expression(*end);
                self.builder.slice(object, Opcode::Range { begin, end })
            }
            Expression::Binary(BinOp::RangeInclusive, begin, end) => {
                let begin = self.compile_expression(*begin);
                let end = self.compile_expression(*end);
                self.builder
                    .slice(object, Opcode::RangeInclusive { begin, end })
            }

            index => {
                let index = self.compile_expression(index);
                self.builder.index_get(object, index)
            }
        }
    }

    fn compile_slice(&mut self, expr: SliceExpression) -> Operand {
        let SliceExpression {
            object,
            range,
            begin,
            end,
        } = expr;

        let object = self.compile_expression(*object);
        let begin = begin.map(|expr| self.compile_expression(*expr));
        let end = end.map(|expr| self.compile_expression(*expr));

        let op = match (range, begin, end) {
            (BinOp::Range, Some(begin), Some(end)) => Opcode::Range { begin, end },
            (BinOp::Range, Some(begin), None) => Opcode::RangeFrom { begin },
            (BinOp::Range, None, Some(end)) => Opcode::RangeTo { end },
            (BinOp::RangeInclusive, Some(begin), Some(end)) => {
                Opcode::RangeInclusive { begin, end }
            }
            (BinOp::RangeInclusive, None, Some(end)) => Opcode::RangeToInclusive { end },
            (BinOp::Range, None, None) => Opcode::RangeFull,
            _ => unreachable!(),
        };

        self.builder.slice(object, op)
    }

    fn compile_literal(&mut self, literal: LiteralExpression) -> Operand {
        let value = match literal {
            LiteralExpression::Boolean(b) => Primitive::Boolean(b),
            LiteralExpression::Integer(i) => Primitive::Integer(i),
            LiteralExpression::Float(f) => Primitive::Float(f),
            LiteralExpression::Char(c) => Primitive::Char(c),
            LiteralExpression::String(s) => Primitive::String(s),
        };

        self.builder.make_constant(value)
    }

    fn compile_identifier(&mut self, identifier: IdentifierExpression) -> Operand {
        match self.symbols.get(&identifier.0) {
            Some(value) => value.as_variable(),
            None => {
                // find func first
                match self
                    .builder
                    .module()
                    .functions
                    .iter()
                    .find(|func| func.signature.name == Name::from(identifier.0.clone()))
                {
                    Some(func) => Operand::Function(func.id),
                    None => {
                        let value = self.builder.load_external_variable(identifier.0.clone());
                        self.symbols.declare(&identifier.0, Symbol::new(value));
                        value
                    }
                }
            }
        }
    }

    fn compile_unary(&mut self, op: PrefixOp, expr: Expression) -> Operand {
        let rhs = self.compile_expression(expr);

        match op {
            PrefixOp::Not => self.builder.unaryop(Opcode::Not, rhs),
            PrefixOp::Neg => self.builder.unaryop(Opcode::Neg, rhs),
        }
    }

    fn compile_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> Operand {
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

    fn compile_range(&mut self, lhs: Expression, rhs: Expression, bounded: bool) -> Operand {
        let begin = self.compile_expression(lhs);
        let end = self.compile_expression(rhs);
        self.builder.range(begin, end, bounded)
    }

    fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        self.builder.create_block(label.into())
    }

    fn loop_context(&self) -> &LoopContext {
        self.loop_contexts.last().expect("not in loop context")
    }

    fn enter_loop_context(&mut self, break_point: BlockId, continue_point: BlockId) {
        self.loop_contexts
            .push(LoopContext::new(break_point, continue_point));
    }

    fn level_loop_context(&mut self) {
        self.loop_contexts.pop().expect("not in loop context");
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Symbol(Operand);

impl Symbol {
    pub fn new(value: Operand) -> Symbol {
        Symbol(value)
    }

    pub fn as_variable(&self) -> Operand {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct SymbolNode {
    parent: Option<SymbolTable>,
    symbols: BTreeMap<String, Symbol>,
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

    fn get(&self, name: &str) -> Option<Symbol> {
        if let Some(value) = self.0.borrow().symbols.get(name) {
            return Some(*value);
        }
        if let Some(parent) = &self.0.borrow().parent {
            return parent.get(name);
        }
        None
    }

    fn declare(&mut self, name: impl Into<String>, value: Symbol) {
        self.0.borrow_mut().symbols.insert(name.into(), value);
    }

    fn new_scope(&self) -> SymbolTable {
        SymbolTable(Rc::new(RefCell::new(SymbolNode {
            parent: Some(self.clone()),
            symbols: BTreeMap::new(),
        })))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_control_flow() {
        let inputs = [
            "if a { b; } else { c; }",
            "if a { b; } else if c { d; } else { e; }",
            "if a { b; } else if c { d; } else if e { f; } else { g; }",
            "for i in 0..10 { if i % 2 == 0 {continue;} if i % 3 == 0 {break;} }",
        ];

        let compiler = Compiler::new();
        for input in inputs.iter() {
            let module = compiler.compile(input).unwrap();
            println!("============");
            println!("{module}")
        }
    }

    #[test]
    fn test_compiler() {
        let inputs = [
            "1 + 2 * 3 - 4 - a * b / c == 0;",
            "a.b.c.d(1, 1) + b(c, d);",
            "a.c = b.c(1,1);",
            "all(Tweets, |x|{return x.Len <= 240;});",
            r#"
let cc = 100;
fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let k = fib(10);
let a = 1;
let b = 2;
if a > 0 {
    a -= 1;
} else {
    a += 1;
}

let c = b + a;
c += cc;

k + cc;
"#,
        ];

        let compiler = Compiler::new();
        for input in inputs.iter() {
            // println!("{}", input);
            let module = compiler.compile(input).unwrap();
            println!("============");
            println!("{module}")
        }
    }
}
