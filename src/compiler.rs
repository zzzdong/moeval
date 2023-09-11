use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    fmt,
};

use crate::{ast::*, value::Value};
use crate::{opcode::OpCode, vm::Register};

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

#[derive(Debug, Clone)]
enum Operand {
    Immed(Value),
    Register(usize),
    Stack(usize),
    Var(usize), // for ir pass
    None,
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Immed(Value) => write!(f, "{}", Value),
            Operand::Register(i) => write!(f, "r{}", i),
            Operand::Var(i) => write!(f, "v{}", i),
            Operand::Stack(i) => write!(f, "sp+{}", i),
            Operand::None => write!(f, ""),
        }
    }
}

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
    opcodes: Vec<IROpCode>,
}

impl IRCompiler {
    fn new() -> Self {
        let mut stack = IRStack::new();
        stack.push();
        IRCompiler {
            stack,
            var: 0,
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
                    self.emit(
                        OpCode::LoadEnv,
                        &[dest.clone(), Operand::Immed(Value::String(name))],
                    );

                    dest
                }
            },
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Member => {
                        let lhs = self.compile_expr(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            // load member
                            let dest = self.create_var();
                            self.emit(
                                OpCode::LoadMember,
                                &[dest.clone(), lhs, Operand::Immed(Value::String(name))],
                            );
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

                        self.emit(op, &[dest.clone(), lhs, rhs]);

                        dest
                    }
                }
            }
            Expression::UnaryOperation(UnaryOperationExpression::Negation(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.create_var();
                self.emit(OpCode::Negate, &[dest.clone(), operand]);
                dest
            }
            Expression::UnaryOperation(UnaryOperationExpression::Not(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.create_var();
                self.emit(OpCode::Not, &[dest.clone(), operand]);
                dest
            }
            Expression::Grouped(GroupedExpression(expr)) => self.compile_expr(*expr),
            Expression::Array(ArrayExpression { elements }) => {
                let array = self.create_var();
                self.emit(OpCode::NewArray, &[array.clone()]);
                for element in elements {
                    let item = self.compile_expr(element);
                    self.emit(OpCode::ArrayPush, &[array.clone(), item]);
                }
                array
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let dict = self.create_var();
                self.emit(OpCode::NewDictionary, &[dict.clone()]);
                for kv in elements {
                    let key = Operand::Immed(Value::String(kv.key.name));
                    let value = self.compile_expr(*kv.value);
                    self.emit(OpCode::DictionaryPut, &[dict.clone(), key, value]);
                }
                dict
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
            LiteralExpression::Null => Operand::Immed(Value::Null),
            LiteralExpression::Boolean(b) => Operand::Immed(Value::Bool(b)),
            LiteralExpression::Integer(i) => Operand::Immed(Value::Integer(i)),
            LiteralExpression::Float(f) => Operand::Immed(Value::Float(f)),
            LiteralExpression::Char(c) => Operand::Immed(Value::Char(c)),
            LiteralExpression::String(s) => Operand::Immed(Value::String(s)),
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

    fn ir_code(&self) -> Vec<IROpCode> {
        self.opcodes.clone()
    }
}

struct IRRewriter {
    regalloc: RegisterAllocation,
    opcodes: Vec<IROpCode>,
    symbol_map: HashMap<usize, LiveInterval>,
}

impl IRRewriter {
    fn new(num_registers: usize) -> IRRewriter {
        IRRewriter {
            regalloc: RegisterAllocation::new(num_registers),
            opcodes: Vec::new(),
            symbol_map: HashMap::new(),
        }
    }


    fn rewrite(&mut self, input: Vec<IROpCode>) -> Vec<IROpCode> {
        self.opcodes.reserve(input.len());

        let line_scan = self.regalloc.line_scan(&input);
        for interval in line_scan {
            self.symbol_map.insert(interval.symbol.var, interval);
        }

        for IROpCode {
            index,
            opcode,
            operand0,
            operand1,
            operand2,
        } in input
        {
            let operands = &[
                self.rewrite_operand(operand0),
                self.rewrite_operand(operand1),
                self.rewrite_operand(operand2),
            ];
            self.emit(opcode, operands);
        }

        self.opcodes.clone()
    }

    fn rewrite_operand(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Var(var) => {
                let symbol = self.symbol_map.get(&var).unwrap().symbol;
                if symbol.location != usize::MAX {
                    // self.emit(OpCode::Push, &[Operand::Register(symbol.register)]);
                    Operand::Stack(symbol.location)
                } else {
                    Operand::Register(symbol.register)
                }
            }
            _ => operand,
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

    
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Symbol {
    var: usize,
    register: usize,
    location: usize,
}
impl Symbol {
    fn new(var: usize) -> Symbol {
        Symbol {
            var,
            register: usize::MAX,
            location: usize::MAX,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LiveInterval {
    symbol: Symbol,
    start: usize,
    end: usize,
}

#[derive(Debug)]
struct RegisterPool {
    registers: Vec<bool>,
}

impl RegisterPool {
    fn new(num_registers: usize) -> Self {
        let mut registers = Vec::with_capacity(num_registers);
        registers.resize(num_registers, false);

        RegisterPool { registers }
    }

    fn num_registers(&self) -> usize {
        self.registers.len()
    }

    fn is_empty(&self) -> bool {
        self.registers.iter().all(|r| *r == true)
    }

    fn alloc_register(&self) -> usize {
        match self.registers.iter().position(|r| *r == false) {
            Some(idx) => idx,
            None => panic!("Register pool is empty"),
        }
    }

    fn free_register(&mut self, idx: usize) {
        self.registers[idx] = true;
    }
}

#[derive(Debug)]
struct RegisterAllocation {
    live_intervals: HashMap<usize, LiveInterval>,
    active_intervals: Vec<LiveInterval>,
    pool: RegisterPool,
    curr_stack_location: usize,
}

impl RegisterAllocation {
    pub fn new(num_registers: usize) -> Self {
        Self {
            live_intervals: HashMap::new(),
            active_intervals: Vec::new(),
            pool: RegisterPool::new(num_registers),
            curr_stack_location: 0,
        }
    }

    pub fn line_scan(&mut self, ir_code: &[IROpCode]) -> Vec<LiveInterval> {
        self.build_live_interval(ir_code);

        let mut live_intervals = self.get_live_intervals();

        println!("live_intervals --> {:?}", live_intervals);

        for (i, interval) in live_intervals.iter_mut().enumerate() {
            println!("Interval {}: {:?}", i, interval);
            self.expire_old_intervals(interval);
            if self.pool.is_empty() {
                println!("");
                self.spill_at_interval(interval);
            } else {
                interval.symbol.register = self.pool.alloc_register();
                println!(
                    "ACTION: ALLOCATE REGISTER{} TO INTERVAL({:?})\n",
                    interval.symbol.register, interval
                );
                self.active_intervals.push(interval.clone());
                self.active_intervals.sort_by_key(|interval| interval.end);
            }
        }

        live_intervals
    }

    fn build_live_interval(&mut self, ir_code: &[IROpCode]) {
        for (idx, ir) in ir_code.iter().enumerate() {
            if let Operand::Var(var) = ir.operand0 {
                self.update_live_interval(var, idx);
            }
            if let Operand::Var(var) = ir.operand1 {
                self.update_live_interval(var, idx);
            }
            if let Operand::Var(var) = ir.operand2 {
                self.update_live_interval(var, idx);
            }
        }
    }

    fn update_live_interval(&mut self, var: usize, idx: usize) {
        match self.live_intervals.entry(var) {
            Entry::Vacant(entry) => {
                entry.insert(LiveInterval {
                    symbol: Symbol::new(var),
                    start: idx,
                    end: idx,
                });
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().end = idx;
            }
        }
    }

    fn get_live_intervals(&self) -> Vec<LiveInterval> {
        let mut interals: Vec<LiveInterval> = self.live_intervals.values().cloned().collect();
        interals.sort_by_key(|interval| interval.start);
        interals
    }

    fn expire_old_intervals(&mut self, interval: &LiveInterval) {
        let mut active_intervals = self.active_intervals.clone();
        active_intervals.sort_by_key(|interval| interval.end);

        let mut to_remove = Vec::new();

        for (idx, active) in active_intervals.iter_mut().enumerate() {
            if active.end >= interval.start {
                // for i in to_remove {
                //     active_intervals.remove(i);
                // }
                // self.active_intervals = active_intervals;
                // return;
                break;
            }
            to_remove.push(active.symbol);
            self.pool.free_register(active.symbol.register);
        }

        for var in to_remove {
            self.active_intervals.remove(
                self.active_intervals
                    .iter()
                    .position(|interval| interval.symbol == var)
                    .unwrap(),
            );
        }
    }

    fn spill_at_interval(&mut self, interval: &mut LiveInterval) {
        let location = self.new_stack_location();
        let spill = self.active_intervals.last_mut().unwrap();

        if spill.end > interval.end {
            println!(
                "ACTION: ALLOCATE REGISTER{} TO INTERVAL({:?})\n",
                spill.symbol.register, interval
            );
            interval.symbol.register = spill.symbol.register;
            spill.symbol.location = location;
            self.active_intervals.pop();
            self.active_intervals.push(interval.clone());
        } else {
            println!("ACTION: SPILL INTERVAL {:?}", interval);
            interval.symbol.location = location;
        }
    }

    fn new_stack_location(&mut self) -> usize {
        let old = self.curr_stack_location;
        self.curr_stack_location += 1;
        old
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
            r#"user.Role in admins || user.Id == comment.UserId"#,
            r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#,
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let mut compiler = IRCompiler::new();
            compiler.compile_expr(expr);

            compiler.print_code();
            println!("====",);

            let mut rewriter = IRRewriter::new(4);

            let ir = rewriter.rewrite(compiler.ir_code());

            for code in ir {
                println!("{};", code);
            }
        }
    }
}
