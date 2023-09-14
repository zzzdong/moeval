use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap, VecDeque},
    fmt,
};

use log::debug;

use crate::{ast::*, value::Value};
use crate::{opcode::OpCode, vm::Register};

#[derive(Debug, Clone)]
struct Instruction {
    index: usize,
    opcode: OpCode,
    operand0: Operand,
    operand1: Operand,
    operand2: Operand,
}

impl Instruction {
    fn new(opcode: OpCode, operand0: Operand, operand1: Operand, operand2: Operand) -> Self {
        Instruction {
            index: 0,
            opcode,
            operand0,
            operand1,
            operand2,
        }
    }

    fn single(opcode: OpCode, operand0: Operand) -> Self {
        Instruction {
            index: 0,
            opcode,
            operand0,
            operand1: Operand::None,
            operand2: Operand::None,
        }
    }

    fn two(opcode: OpCode, operand0: Operand, operand1: Operand) -> Self {
        Instruction {
            index: 0,
            opcode,
            operand0,
            operand1,
            operand2: Operand::None,
        }
    }
}

impl fmt::Display for Instruction {
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
pub struct VirtReg(usize);

impl fmt::Display for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

pub struct Instructions(Vec<Instruction>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    fn push(&mut self, inst: Instruction) {
        self.0.push(inst)
    }

    fn extend(&mut self, insts: impl IntoIterator<Item = Instruction>) {
        self.0.extend(insts)
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in &self.0 {
            writeln!(f, "{};", i)?;
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
struct VirtRegAllocator(usize);

impl VirtRegAllocator {
    pub fn new() -> Self {
        VirtRegAllocator(0)
    }

    pub fn alloc(&mut self) -> VirtReg {
        let var = self.0;
        self.0 += 1;
        VirtReg(var)
    }
}

#[derive(Debug, Clone)]
pub struct IRBuilder {
    vr_allocator: VirtRegAllocator,
    instructions: Vec<Instruction>,
    symbol_table: HashMap<String, Symbol>,
}

impl IRBuilder {
    fn new() -> Self {
        IRBuilder {
            vr_allocator: VirtRegAllocator::new(),
            instructions: Vec::new(),
            symbol_table: HashMap::new(),
        }
    }

    fn compile_expr(&mut self, expr: Expression) -> Operand {
        println!("compiling expr: `{}`", expr);
        match expr {
            Expression::Literal(lit) => self.create_literal_operand(lit),
            Expression::Identifier(IdentifierExpression { name }) => {
                match self.symbol_table.get(&name) {
                    Some(symbol) => Operand::Var(symbol.var),
                    None => {
                        let dest = self.alloc_virt_reg();
                        self.emit(
                            OpCode::LoadEnv,
                            &[dest.clone(), Operand::Immed(Value::String(name))],
                        );

                        dest
                    }
                }
            }
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                match op {
                    BinaryOperation::Member => {
                        let lhs = self.compile_expr(*left);
                        if let Expression::Identifier(IdentifierExpression { name }) = *right {
                            // load member
                            let dest = self.alloc_virt_reg();
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
                        let dest = self.alloc_virt_reg();

                        self.emit(op, &[dest.clone(), lhs, rhs]);

                        dest
                    }
                }
            }
            Expression::UnaryOperation(UnaryOperationExpression::Negation(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.alloc_virt_reg();
                self.emit(OpCode::Negate, &[dest.clone(), operand]);
                dest
            }
            Expression::UnaryOperation(UnaryOperationExpression::Not(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.alloc_virt_reg();
                self.emit(OpCode::Not, &[dest.clone(), operand]);
                dest
            }
            Expression::Grouped(GroupedExpression(expr)) => self.compile_expr(*expr),
            Expression::Array(ArrayExpression { elements }) => {
                let array = self.alloc_virt_reg();
                self.emit(OpCode::NewArray, &[array.clone()]);
                for element in elements {
                    let item = self.compile_expr(element);
                    self.emit(OpCode::ArrayPush, &[array.clone(), item]);
                }
                array
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let dict = self.alloc_virt_reg();
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

    fn alloc_virt_reg(&mut self) -> Operand {
        let vr = self.vr_allocator.alloc();
        Operand::Var(vr.0)
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
        let idx = self.instructions.len();

        let opcode = Instruction {
            index: idx,
            opcode,
            operand0: operands.get(0).unwrap_or(&Operand::None).clone(),
            operand1: operands.get(1).unwrap_or(&Operand::None).clone(),
            operand2: operands.get(2).unwrap_or(&Operand::None).clone(),
        };

        self.instructions.push(opcode);
    }

    fn instructions(&self) -> Instructions {
        Instructions(self.instructions.clone())
    }
}

struct VirtRegRewriter {
    regalloc: RegisterAllocator,
}

impl VirtRegRewriter {
    fn new(num_registers: usize) -> VirtRegRewriter {
        VirtRegRewriter {
            regalloc: RegisterAllocator::new(num_registers),
        }
    }

    fn rewrite(&mut self, input: Instructions) -> Instructions {
        let mut output = Instructions::new();

        let virt_reg_map = self.regalloc.line_scan(&input);
        println!("VirtRegMap: {:?}", &virt_reg_map);

        for inst in input.0 {
            output.extend(virt_reg_map.rewrite(inst));
        }

        output
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
struct VirtRegMap {
    intervals: Vec<LiveInterval>,
    virt_reg_map: BTreeMap<usize, LiveInterval>,
}

impl VirtRegMap {
    fn new(intervals: Vec<LiveInterval>) -> Self {
        let mut reg_map = BTreeMap::new();
        for interval in &intervals {
            reg_map.insert(interval.symbol.var, interval.clone());
        }
        Self {
            intervals,
            virt_reg_map: reg_map,
        }
    }

    fn rewrite(&self, inst: Instruction) -> Vec<Instruction> {
        let mut insts = Vec::new();

        let Instruction {
            index,
            opcode,
            operand0,
            operand1,
            operand2,
        } = inst;

        let operand1 = match operand1 {
            Operand::Var(var) => {
                let interval = self
                    .virt_reg_map
                    .get(&var)
                    .expect("var not found in virt_reg_map");
                if interval.symbol.location != usize::MAX {
                    Operand::Stack(interval.symbol.location)
                } else {
                    Operand::Register(interval.symbol.register)
                }
            }
            _ => operand1,
        };

        let operand2 = match operand2 {
            Operand::Var(var) => {
                let interval = self
                    .virt_reg_map
                    .get(&var)
                    .expect("var not found in virt_reg_map");
                if interval.symbol.location != usize::MAX {
                    Operand::Stack(interval.symbol.location)
                } else {
                    Operand::Register(interval.symbol.register)
                }
            }
            _ => operand2,
        };

        // if dest should spill, need store into stack
        if let Operand::Var(var) = operand0 {
            let interval = self
                .virt_reg_map
                .get(&var)
                .expect("var not found in virt_reg_map");
            if interval.symbol.location != usize::MAX {
                insts.push(Instruction::single(OpCode::Push, Operand::Register(0)));
                insts.push(Instruction::new(
                    opcode,
                    Operand::Register(0),
                    operand1,
                    operand2,
                ));
                insts.push(Instruction::two(
                    OpCode::Store,
                    Operand::Stack(interval.symbol.location),
                    Operand::Register(0),
                ));
                insts.push(Instruction::single(OpCode::Pop, Operand::Register(0)));
            } else {
                insts.push(Instruction::new(
                    opcode,
                    Operand::Register(interval.symbol.register),
                    operand1,
                    operand2,
                ));
            }
        }

        insts
    }
}

#[derive(Debug)]
struct RegisterAllocator {
    live_intervals: HashMap<usize, LiveInterval>,
    active_intervals: Vec<LiveInterval>,
    pool: RegisterPool,
    curr_stack_location: usize,
}

impl RegisterAllocator {
    pub fn new(num_registers: usize) -> Self {
        Self {
            live_intervals: HashMap::new(),
            active_intervals: Vec::new(),
            pool: RegisterPool::new(num_registers),
            curr_stack_location: 0,
        }
    }

    pub fn line_scan(&mut self, instructions: &Instructions) -> VirtRegMap {
        self.build_live_interval(instructions);

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

        VirtRegMap::new(live_intervals)
    }

    fn build_live_interval(&mut self, instructions: &Instructions) {
        for (idx, ir) in instructions.0.iter().enumerate() {
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

        self.active_intervals
            .retain(|interval| !to_remove.contains(&interval.symbol));
    }

    fn spill_at_interval(&mut self, interval: &mut LiveInterval) {
        if self.active_intervals.is_empty() {
            return;
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
            println!(
                "ASSIGN symbol{:?} to sp+{}",
                interval.symbol, stack_location
            );
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
            r#"a + b * c + 100 - e + f % g / 2 || a == 3"#,
            r#"user.Role in admins || user.Id == comment.UserId"#,
            r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#,
        ];

        for input in inputs {
            let expr = Parser::parse(input).unwrap();

            println!("expr {:?}", expr);

            let mut compiler = IRBuilder::new();
            compiler.compile_expr(expr);

            println!("{}", compiler.instructions());
            println!("====",);

            let mut rewriter = VirtRegRewriter::new(2);

            let output = rewriter.rewrite(compiler.instructions());

            println!("{}", output);
        }
    }
}
