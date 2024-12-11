mod ast;
mod parser;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ir::*;
use ast::*;
use parser::ParseError;

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
        let ast = parser::parse_file(input)?;
        // println!("{:#?}", ast);
        // // 语义分析
        // let ast = crate::semantics::analyze(ast)?;

        let mut module = Module::new();

        let builder: &mut dyn InstBuilder = &mut ModuleBuilder::new(&mut module);

        let mut inst_compiler = InstCompiler::new(builder, SymbolTable::new());

        inst_compiler.compile_statements(ast.stmts);

        Ok(module)
    }
}

struct State {
    pub(crate) break_point: Option<BlockId>,
    pub(crate) continue_point: Option<BlockId>,
}

impl State {
    pub(crate) fn new() -> Self {
        Self {
            break_point: None,
            continue_point: None,
        }
    }
}

pub struct InstCompiler<'a> {
    builder: &'a mut dyn InstBuilder,
    symbols: SymbolTable,
    state: State,
}

impl<'a> InstCompiler<'a> {
    pub fn new(builder: &'a mut dyn InstBuilder, symbols: SymbolTable) -> Self {
        Self {
            builder,
            symbols,
            state: State::new(),
        }
    }

    fn compile_statements(&mut self, stmts: Vec<Statement>) {
        let entry = self.create_block("main");
        self.builder.switch_to_block(entry);
        self.builder.control_flow_graph_mut().set_entry(entry);

        for stmt in stmts {
            self.compile_statement(stmt);
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

        self.symbols.declare(&name, Symbol::new_variable(dst));
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

        let curr_blk = self.builder.current_block();
        let next_blk = self.create_block(None);

        let true_blk = self.create_block(None);

        let false_blk = else_branch.as_ref().map(|_| {
            let blk = self.create_block(None);
            blk
        });

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

    fn compile_pattern(&mut self, pat: Pattern, value: Variable) {
        match pat {
            Pattern::Wildcard => {}
            Pattern::Identifier(ident) => {
                let dst = self.builder.create_alloc();
                self.builder.assign(dst, value);
                self.symbols.declare(&ident, Symbol::new_variable(dst));
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

        // after loop
        self.builder.switch_to_block(after_blk);
    }

    fn compile_break_stmt(&mut self) {
        if let Some(break_point) = self.state.break_point.take() {
            self.builder.br(break_point);
        }
    }

    fn compile_continue_stmt(&mut self) {
        if let Some(continue_point) = self.state.continue_point.take() {
            self.builder.br(continue_point);
        }
    }

    fn compile_block(&mut self, block: Vec<Statement>) {
        let new_symbols = self.symbols.new_scope();
        let old_symbols = std::mem::replace(&mut self.symbols, new_symbols);
        for statement in block {
            self.compile_statement(statement);
        }
        self.symbols = old_symbols;
    }

    fn compile_function_item(&mut self, fn_item: FunctionItem) -> FunctionId {
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
        params: Vec<ast::FunctionParam>,
        body: Vec<Statement>,
    ) -> FunctionId {
        let func_sig = FuncSignature::new(
            name.clone(),
            params
                .iter()
                .map(|p| FuncParam::new(p.name.clone()))
                .collect(),
        );
        let func_id = self.builder.module_mut().declare_function(func_sig);
        if let Some(name) = &name {
            self.symbols.declare(name, Symbol::new_function(func_id));
        }

        let symbols = self.symbols.new_scope();

        let mut func_compiler = InstCompiler::new(self.builder, symbols);

        for (idx, param) in params.iter().enumerate() {
            let arg = func_compiler.builder.load_argument(idx);
            func_compiler
                .symbols
                .declare(param.name.as_str(), Symbol::new_variable(arg));
        }

        for stmt in body {
            func_compiler.compile_statement(stmt);
        }

        let func_control_flow_graph = func_compiler.builder.take_control_flow_graph();

        self.builder
            .module_mut()
            .define_function(func_id, func_control_flow_graph);

        func_id
    }

    fn compile_expression(&mut self, expr: Expression) -> Variable {
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
            // Expression::Closure(closure) => self.compile_closure(closure),
            Expression::Array(array) => self.compile_array(array),
            Expression::Index(index) => self.compile_index(index),
            Expression::Map(map) => self.compile_map(map),
            Expression::Slice(slice) => self.compile_slice(slice),
            Expression::Await(expr) => self.compile_await(*expr),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn compile_get_property(&mut self, expr: MemberExpression) -> Variable {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.get_property(object, &property)
    }

    fn compile_set_property(&mut self, expr: MemberExpression, value: Variable) {
        let MemberExpression { object, property } = expr;

        let object = self.compile_expression(*object);

        self.builder.set_property(object, &property, value)
    }

    fn compile_call(&mut self, expr: CallExpression) -> Variable {
        let CallExpression { func, args } = expr;

        let args: Vec<Variable> = args
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

    fn compile_assign(&mut self, expr: AssignExpression) -> Variable {
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

    fn compile_closure(&mut self, expr: ClosureExpression) -> FunctionId {
        let ClosureExpression { params, body } = expr;

        self.compile_function(None, params, body)
    }

    fn compile_array(&mut self, expr: ArrayExpression) -> Variable {
        let ArrayExpression(elements) = expr;
        let array = self.builder.new_array(Some(elements.len()));

        for element in elements {
            let elem = self.compile_expression(element);
            self.builder.array_push(array, elem);
        }

        array
    }

    fn compile_map(&mut self, expr: MapExpression) -> Variable {
        let MapExpression(elements) = expr;
        let map = self.builder.new_map();

        for (key, value) in elements {
            let key = self.compile_literal(key.as_literal().unwrap());
            let elem = self.compile_expression(value);
            self.builder.index_set(map, key, elem);
        }

        map
    }

    fn compile_await(&mut self, expr: Expression) -> Variable {
        unimplemented!();

        // let promise = self.compile_expression(expr);

        // self.builder.await_promise(promise)
    }

    fn compile_index(&mut self, expr: IndexExpression) -> Variable {
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

    fn compile_slice(&mut self, expr: SliceExpression) -> Variable {
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

    fn compile_literal(&mut self, literal: LiteralExpression) -> Variable {
        let value = match literal {
            LiteralExpression::Boolean(b) => Primitive::Boolean(b),
            LiteralExpression::Integer(i) => Primitive::Integer(i),
            LiteralExpression::Float(f) => Primitive::Float(f),
            LiteralExpression::Char(c) => Primitive::Char(c),
            LiteralExpression::String(s) => Primitive::String(s),
        };

        self.builder.make_constant(value)
    }

    fn compile_identifier(&mut self, identifier: IdentifierExpression) -> Variable {
        match self.symbols.get(&identifier.0) {
            Some(value) => value.as_variable(),
            None => {
                let value = self.builder.load_external_variable(identifier.0.clone());
                self.symbols
                    .declare(&identifier.0, Symbol::new_variable(value));
                value
            }
        }
    }

    fn compile_unary(&mut self, op: PrefixOp, expr: Expression) -> Variable {
        let rhs = self.compile_expression(expr);

        match op {
            PrefixOp::Not => self.builder.unaryop(Opcode::Not, rhs),
            PrefixOp::Neg => self.builder.unaryop(Opcode::Neg, rhs),
        }
    }

    fn compile_binary(&mut self, op: BinOp, lhs: Expression, rhs: Expression) -> Variable {
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

    fn compile_range(&mut self, lhs: Expression, rhs: Expression, bounded: bool) -> Variable {
        let begin = self.compile_expression(lhs);
        let end = self.compile_expression(rhs);
        self.builder.range(begin, end, bounded)
    }

    fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        self.builder.create_block(label.into())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Symbol {
    Variable(Variable),
    Function(FunctionId),
}

impl Symbol {
    pub fn new_variable(value: Variable) -> Symbol {
        Symbol::Variable(value)
    }

    pub fn new_function(value: FunctionId) -> Symbol {
        Symbol::Function(value)
    }

    pub fn as_variable(&self) -> Variable {
        match self {
            Symbol::Variable(value) => *value,
            _ => panic!("not a variable"),
        }
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
