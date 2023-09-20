use std::collections::{hash_map::Entry, HashMap};

use crate::{
    instruction::{Instruction, Module, OpCode, Operand},
    instruction::{Register, StackOffset, VirtReg},
    value::Primitive,
};

use super::ModuleRewriter;

pub struct VirtRegRewriter {
    regalloc: RegisterAllocator,
}

impl VirtRegRewriter {
    pub fn new(registers: &[Register]) -> VirtRegRewriter {
        VirtRegRewriter {
            regalloc: RegisterAllocator::new(registers),
        }
    }

    pub fn rewrite(&mut self, input: Module) -> Module {
        let mut output = Module::new();

        let virt_reg_map = self.regalloc.line_scan(&input);

        // alloc stack space
        let stack_offset = virt_reg_map.max_stack_offset();
        if let Some(stack_offset) = stack_offset {
            output.push(Instruction::single(
                OpCode::StackAlloc,
                Operand::Immed(Primitive::Integer(stack_offset as i64)),
            ));
        }

        // rewrite virtual register
        for inst in input.into_iter() {
            output.extend(virt_reg_map.rewrite(inst));
        }

        output
    }
}

impl ModuleRewriter for VirtRegRewriter {
    fn rewrite(&mut self, module: Module) -> Module {
        VirtRegRewriter::rewrite(self, module)
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
        self.registers.iter().all(|(_reg, usable)| !(*usable))
    }

    fn alloc_register(&mut self) -> Register {
        match self.registers.iter_mut().find(|(_reg, usable)| **usable) {
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
            reg_map.insert(interval.vreg, *interval);
        }
        Self {
            intervals,
            virt_reg_map: reg_map,
        }
    }

    fn max_stack_offset(&self) -> Option<usize> {
        let mut stack_offset = None;
        for interval in &self.intervals {
            if let Address::StackOffset(offset) = interval.address {
                match stack_offset {
                    Some(off) if offset.as_usize() > off => {
                        stack_offset = Some(offset.as_usize());
                    }
                    None => {
                        stack_offset = Some(offset.as_usize());
                    }
                    _ => {}
                }
            }
        }

        stack_offset
    }

    fn rewrite(&self, inst: Instruction) -> Vec<Instruction> {
        let mut insts = Vec::new();

        let Instruction {
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
        match operand0 {
            Operand::VirtReg(var) => {
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
            _ => {
                let inst = Instruction::new(opcode, operand0, operand1, operand2);
                insts.push(inst)
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

    pub fn line_scan(&mut self, instructions: &Module) -> VirtRegMap {
        self.build_live_interval(instructions);

        let mut live_intervals = self.get_live_intervals();

        for (_i, interval) in live_intervals.iter_mut().enumerate() {
            self.expire_old_intervals(interval);
            if self.pool.is_empty() {
                self.spill_at_interval(interval);
            } else {
                interval.address = Address::Register(self.pool.alloc_register());
                self.active_intervals.sort_by_key(|interval| interval.end);
                self.active_intervals.push(*interval);
            }
        }

        VirtRegMap::new(live_intervals)
    }

    fn build_live_interval(&mut self, instructions: &Module) {
        for (idx, ir) in instructions.iter().enumerate() {
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

        for (_idx, active) in self.active_intervals.iter().enumerate() {
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
            self.active_intervals.push(*interval);
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
