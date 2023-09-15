use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::{collections::BTreeMap, fmt};

use crate::ast::*;
use crate::opcode::{OpCode, Operand};
use crate::value::Value;
use crate::vm::{Register, StackOffset, VirtReg};

#[derive(Debug, Clone)]
struct VirtRegAllocator {
    count: usize,
}

impl VirtRegAllocator {
    pub fn new() -> Self {
        VirtRegAllocator { count: 0 }
    }

    pub fn alloc(&mut self) -> VirtReg {
        let var = self.count;
        self.count += 1;
        VirtReg(var)
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
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
        write!(f, "{:?} {}", self.opcode, self.operand0)?;
        if self.operand1 != Operand::None {
            write!(f, " {}", self.operand1)?;
        }
        if self.operand2 != Operand::None {
            write!(f, " {}", self.operand2)?;
        }

        Ok(())
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
pub struct IRBuilder {
    vr_allocator: VirtRegAllocator,
    symbol_table: HashMap<String, VirtReg>,
    instructions: Vec<Instruction>,
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
        match expr {
            Expression::Literal(lit) => self.create_literal_operand(lit),
            Expression::Identifier(IdentifierExpression { name }) => {
                match self.symbol_table.get(&name) {
                    Some(vreg) => Operand::VirtReg(*vreg),
                    None => {
                        let dest = self.alloc_virt_reg();
                        self.emit(
                            OpCode::LoadEnv,
                            &[dest.clone(), Operand::Immed(Value::String(name.clone()))],
                        );

                        if let Operand::VirtReg(vreg) = dest {
                            self.symbol_table.insert(name, vreg);
                        }

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
            Expression::PrefixOperation(PrefixOperationExpression::Negation(expr)) => {
                let operand = self.compile_expr(*expr);
                let dest = self.alloc_virt_reg();
                self.emit(OpCode::Negate, &[dest.clone(), operand]);
                dest
            }
            Expression::PrefixOperation(PrefixOperationExpression::Not(expr)) => {
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
        Operand::VirtReg(vr)
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
    fn new(registers: &[Register]) -> VirtRegRewriter {
        VirtRegRewriter {
            regalloc: RegisterAllocator::new(registers),
        }
    }

    fn rewrite(&mut self, input: Instructions) -> Instructions {
        let mut output = Instructions::new();

        let virt_reg_map = self.regalloc.line_scan(&input);
        for inst in input.0 {
            output.extend(virt_reg_map.rewrite(inst));
        }

        output
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Address {
    None,
    Register(Register),
    StackOffset(StackOffset),
}

#[derive(Debug, Clone, Copy, Hash)]
struct LiveInterval {
    vreg: VirtReg,
    address: Address,
    start: usize,
    end: usize,
}

#[derive(Debug)]
struct RegisterPool {
    registers: HashMap<Register, bool>,
}

impl RegisterPool {
    fn new(registers: &[Register]) -> Self {
        let mut registers_map = HashMap::new();
        for r in registers {
            registers_map.insert(*r, true);
        }

        RegisterPool {
            registers: registers_map,
        }
    }

    fn num_registers(&self) -> usize {
        self.registers.len()
    }

    fn is_empty(&self) -> bool {
        self.registers.iter().all(|(_reg, usable)| *usable == false)
    }

    fn alloc_register(&mut self) -> Register {
        match self
            .registers
            .iter_mut()
            .find(|(_reg, usable)| **usable == true)
        {
            Some((reg, usable)) => {
                *usable = false;
                *reg
            }
            None => panic!("Register pool is empty"),
        }
    }

    fn free_register(&mut self, reg: Register) {
        *self.registers.get_mut(&reg).unwrap() = true;
    }
}

#[derive(Debug)]
struct VirtRegMap {
    intervals: Vec<LiveInterval>,
    virt_reg_map: HashMap<VirtReg, LiveInterval>,
}

impl VirtRegMap {
    fn new(intervals: Vec<LiveInterval>) -> Self {
        let mut reg_map = HashMap::new();
        for interval in &intervals {
            reg_map.insert(interval.vreg, interval.clone());
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
            Operand::VirtReg(var) => {
                let interval = self
                    .virt_reg_map
                    .get(&var)
                    .expect("var not found in virt_reg_map");

                match interval.address {
                    Address::None => unreachable!(),
                    Address::Register(reg) => Operand::Register(reg),
                    Address::StackOffset(off) => Operand::Stack(off),
                }
            }
            _ => operand1,
        };

        let operand2 = match operand2 {
            Operand::VirtReg(var) => {
                let interval = self
                    .virt_reg_map
                    .get(&var)
                    .expect("var not found in virt_reg_map");
                match interval.address {
                    Address::None => unreachable!(),
                    Address::Register(reg) => Operand::Register(reg),
                    Address::StackOffset(off) => Operand::Stack(off),
                }
            }
            _ => operand2,
        };

        // if dest should spill, need store into stack
        if let Operand::VirtReg(var) = operand0 {
            let interval = self
                .virt_reg_map
                .get(&var)
                .expect("var not found in virt_reg_map");
            match interval.address {
                Address::StackOffset(off) => {
                    let tmp_register = Register::R0;

                    insts.push(Instruction::single(
                        OpCode::Push,
                        Operand::Register(tmp_register),
                    ));
                    insts.push(Instruction::new(
                        opcode,
                        Operand::Register(tmp_register),
                        operand1,
                        operand2,
                    ));
                    insts.push(Instruction::two(
                        OpCode::Store,
                        Operand::Stack(off),
                        Operand::Register(tmp_register),
                    ));
                    insts.push(Instruction::single(
                        OpCode::Pop,
                        Operand::Register(tmp_register),
                    ));
                }
                Address::Register(reg) => {
                    insts.push(Instruction::new(
                        opcode,
                        Operand::Register(reg),
                        operand1,
                        operand2,
                    ));
                }
                _ => {
                    unreachable!()
                }
            }
        }

        insts
    }
}

#[derive(Debug)]
struct RegisterAllocator {
    live_intervals: HashMap<VirtReg, LiveInterval>,
    active_intervals: Vec<LiveInterval>,
    pool: RegisterPool,
    curr_stack_location: usize,
}

impl RegisterAllocator {
    pub fn new(registers: &[Register]) -> Self {
        Self {
            live_intervals: HashMap::new(),
            active_intervals: Vec::new(),
            pool: RegisterPool::new(registers),
            curr_stack_location: 0,
        }
    }

    pub fn line_scan(&mut self, instructions: &Instructions) -> VirtRegMap {
        self.build_live_interval(instructions);

        let mut live_intervals = self.get_live_intervals();

        for (i, interval) in live_intervals.iter_mut().enumerate() {
            self.expire_old_intervals(interval);
            if self.pool.is_empty() {
                self.spill_at_interval(interval);
            } else {
                interval.address = Address::Register(self.pool.alloc_register());
                self.active_intervals.sort_by_key(|interval| interval.end);
                self.active_intervals.push(interval.clone());
            }
        }

        VirtRegMap::new(live_intervals)
    }

    fn build_live_interval(&mut self, instructions: &Instructions) {
        for (idx, ir) in instructions.0.iter().enumerate() {
            if let Operand::VirtReg(var) = ir.operand0 {
                self.update_live_interval(var, idx);
            }
            if let Operand::VirtReg(var) = ir.operand1 {
                self.update_live_interval(var, idx);
            }
            if let Operand::VirtReg(var) = ir.operand2 {
                self.update_live_interval(var, idx);
            }
        }
    }

    fn update_live_interval(&mut self, vreg: VirtReg, idx: usize) {
        match self.live_intervals.entry(vreg) {
            Entry::Vacant(entry) => {
                entry.insert(LiveInterval {
                    vreg,
                    address: Address::None,
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
            to_remove.push(active.address);
            if let Address::Register(reg) = active.address {
                self.pool.free_register(reg);
            } else {
                panic!("must expire a Resigter")
            }
        }

        self.active_intervals
            .retain(|interval| !to_remove.contains(&interval.address));
    }

    fn spill_at_interval(&mut self, interval: &mut LiveInterval) {
        if self.active_intervals.is_empty() {
            return;
        }

        let stack_location = self.new_stack_location();
        let spill = self.active_intervals.last_mut().unwrap();

        if spill.end > interval.end {
            interval.address = spill.address;
            spill.address = stack_location;

            self.active_intervals.pop();
            self.active_intervals.sort_by_key(|interval| interval.end);
            self.active_intervals.push(interval.clone());
        } else {
            interval.address = stack_location;
        }
    }

    fn new_stack_location(&mut self) -> Address {
        let old = self.curr_stack_location;
        self.curr_stack_location += 1;
        Address::StackOffset(StackOffset::new(old))
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn test_irbuilder() {
        let inputs = vec![
            r#"a + b * c + 100 - e + f % g / 2 + a#,
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

            let mut rewriter = VirtRegRewriter::new(&[Register::R0, Register::R1]);

            let output = rewriter.rewrite(compiler.instructions());

            println!("{}", output);
        }
    }
}
