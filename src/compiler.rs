use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    fmt,
};

use log::debug;

use crate::{ast::*, value::Value};
use crate::{opcode::OpCode, vm::Register};

#[derive(Debug, Clone)]
struct IRCode {
    index: usize,
    opcode: OpCode,
    operand0: Operand,
    operand1: Operand,
    operand2: Operand,
}

impl fmt::Display for IRCode {
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

pub struct IRBuilder {
    var: usize,
    codes: Vec<IRCode>,
    symbol_table: HashMap<String, Symbol>,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            var: 0,
            codes: Vec::new(),
            symbol_table: HashMap::new(),
        }
    }

    fn compile_expr(&mut self, expr: Expression) -> Operand {
        println!("compiling expr: `{}`", expr);
        match expr {
            Expression::Literal(lit) => self.create_literal_operand(lit),
            Expression::Identifier(IdentifierExpression { name }) => match self.symbol_table.get(&name) {
                Some(symbol) => Operand::Var(symbol.var),
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
        let idx = self.codes.len();

        let opcode = IRCode {
            index: idx,
            opcode,
            operand0: operands.get(0).unwrap_or(&Operand::None).clone(),
            operand1: operands.get(1).unwrap_or(&Operand::None).clone(),
            operand2: operands.get(2).unwrap_or(&Operand::None).clone(),
        };

        self.codes.push(opcode);
    }

    fn print_code(&self) {
        for code in &self.codes {
            println!("{};", code);
        }
    }

    fn ir_code(&self) -> Vec<IRCode> {
        self.codes.clone()
    }
}

struct VRegRewriter {
    regalloc: RegisterAllocation,
    opcodes: Vec<IRCode>,
    symbol_map: HashMap<usize, LiveInterval>,
    stack_bottom: usize,
    reg_cache: Vec<usize>,
}

impl VRegRewriter {
    fn new(num_registers: usize) -> VRegRewriter {
        VRegRewriter {
            regalloc: RegisterAllocation::new(num_registers),
            opcodes: Vec::new(),
            symbol_map: HashMap::new(),
            stack_bottom: 0,
            reg_cache: Vec::new(),
        }
    }

    fn rewrite(&mut self, input: Vec<IRCode>) -> Vec<IRCode> {
        self.opcodes.reserve(input.len());

        let line_scan = self.regalloc.line_scan(&input);

        println!("Line scan: {:?}", line_scan);

        for interval in line_scan {
            self.symbol_map.insert(interval.symbol.var, interval);
        }

        for IRCode {
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

            // restore
            self.reg_cache.reverse();
            for reg in self.reg_cache.clone() {
                self.emit(OpCode::Pop, &[Operand::Register(reg)]);
            }
        }

        self.opcodes.clone()
    }

    fn rewrite_operand(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Var(var) => {
                let symbol = self.symbol_map.get(&var).unwrap().symbol;
                if symbol.location != usize::MAX && symbol.register != usize::MAX {
                    self.emit(OpCode::Push, &[Operand::Register(symbol.register)]);
                    Operand::Stack(symbol.location)
                } else if symbol.location != usize::MAX {
                    // spill, borrow a reg
                    let reg = self.reg_cache.len();
                    self.emit(OpCode::Push, &[Operand::Register(reg)]);
                    self.reg_cache.push(reg);
                    Operand::Register(reg)
                } else {
                    Operand::Register(symbol.register)
                }
            }
            _ => operand,
        }
    }

    fn emit(&mut self, opcode: OpCode, operands: &[Operand]) {
        let idx = self.opcodes.len();

        let opcode = IRCode {
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
        let mut registers = Vec::new();
        registers.resize(num_registers, false);

        RegisterPool { registers }
    }

    fn num_registers(&self) -> usize {
        self.registers.len()
    }

    fn is_empty(&self) -> bool {
        self.registers.iter().all(|r| *r == true)
    }

    fn alloc_register(&mut self) -> usize {
        match self.registers.iter().position(|r| *r == false) {
            Some(idx) => {
                self.registers[idx] = true;
                println!("alloc R{}", idx);
                idx
            }
            None => panic!("Register pool is empty"),
        }
    }

    fn free_register(&mut self, idx: usize) {
        println!("free R{}", idx);
        self.registers[idx] = false;
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

    pub fn line_scan(&mut self, ir_code: &[IRCode]) -> Vec<LiveInterval> {
        self.build_live_interval(ir_code);

        let mut live_intervals = self.get_live_intervals();

        println!("live_intervals --> {:?}", live_intervals);

        for (i, interval) in live_intervals.iter_mut().enumerate() {
            // println!("Interval {}: {:?}", i, interval);
            self.expire_old_intervals(interval);
            if self.pool.is_empty() {
                self.spill_at_interval(interval);
            } else {
                interval.symbol.register = self.pool.alloc_register();
                // println!(
                //     "ACTION: ALLOCATE REGISTER{} TO INTERVAL({:?})\n",
                //     interval.symbol.register, interval
                // );
                self.active_intervals.sort_by_key(|interval| interval.end);
                self.active_intervals.push(interval.clone());
            }
        }

        live_intervals
    }

    fn build_live_interval(&mut self, ir_code: &[IRCode]) {
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
        self.active_intervals.sort_by_key(|interval| interval.end);

        let mut to_remove = Vec::new();

        for (idx, active) in self.active_intervals.iter().enumerate() {
            if active.end >= interval.start {
                break;
            }
            to_remove.push(active.symbol);
            self.pool.free_register(active.symbol.register);
        }

        self.active_intervals.retain(|interval| !to_remove.contains(&interval.symbol));
    }

    fn spill_at_interval(&mut self, interval: &mut LiveInterval) {
        if self.active_intervals.is_empty() {
            return
        }

        let stack_location = self.new_stack_location();
        let spill = self.active_intervals.last_mut().unwrap();

        if spill.end > interval.end {
            // println!(
            //     "ACTION: ALLOCATE REGISTER{} TO INTERVAL({:?})\n",
            //     spill.symbol.register, interval
            // );
            interval.symbol.register = spill.symbol.register;
            spill.symbol.location = stack_location;

            println!("SPILL R{} -> sp+{}", spill.symbol.register, stack_location);

            self.active_intervals.pop();
            self.active_intervals.sort_by_key(|interval| interval.end);
            self.active_intervals.push(interval.clone());
        } else {
            // println!("ACTION: SPILL INTERVAL {:?}", interval);
            println!("ASSIGN symbol{:?} to sp+{}", interval.symbol, stack_location);
            interval.symbol.location = stack_location;
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
            r#"a + b * c + 100 - e + f ^ g / 2 || a == 3"#,
            r#"user.Role in admins || user.Id == comment.UserId"#,
            r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#,
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let mut compiler = IRBuilder::new();
            compiler.compile_expr(expr);

            compiler.print_code();
            println!("====",);

            let mut rewriter = VRegRewriter::new(4);

            let ir = rewriter.rewrite(compiler.ir_code());

            for code in ir {
                println!("{};", code);
            }
        }
    }
}
