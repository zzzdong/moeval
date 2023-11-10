use std::collections::{HashMap, BTreeMap};
use std::collections::hash_map::Entry;
use std::fmt;

use crate::irbuilder::{IRModule, InstructionData, ValueData, ValueRef};
use crate::opcode::Opcode;
use crate::Value;

pub struct CodeGen {}

impl CodeGen {
    pub fn new() -> CodeGen {
        CodeGen {}
    }

    pub fn gen(&mut self, ir: IRModule) -> CodeUnit {
        let mut unit = CodeUnit::new();
    }

    fn gen_ir_code(&mut self, ir: IRModule) -> CodeUnit {
        let mut unit = CodeUnit::new();

        for inst in ir.instructions.iter() {
            match inst.data {
                InstructionData::Binary { op, lhs, rhs } => {
                    let operand1 = self.gen_ir_operand(&ir, lhs);
                    let operand2 = self.gen_ir_operand(&ir, rhs);
                    let dest = self.gen_ir_operand(&ir, inst.result.unwrap());
                    unit.add_inst(VmCode::Triple {
                        opcode: op,
                        dest,
                        operand1,
                        operand2,
                    });
                }
                InstructionData::Unary { op, operand } => {
                    let dest = self.gen_ir_operand(&ir, inst.result.unwrap());
                    let operand = self.gen_ir_operand(&ir, operand);
                    unit.add_inst(VmCode::Double {
                        opcode: op,
                        dest,
                        operand,
                    });
                }
                InstructionData::NewArray { size } => {
                    let dest = self.gen_ir_operand(&ir, inst.result.unwrap());
                    let operand = self.gen_ir_operand(&ir, size);
                    unit.add_inst(VmCode::Double {
                        opcode: Opcode::NewArray,
                        dest,
                        operand,
                    });
                }
                InstructionData::ArrayPush { array, element } => {
                    let dest = self.gen_ir_operand(&ir, array);
                    let operand = self.gen_ir_operand(&ir, element);
                    unit.add_inst(VmCode::Double {
                        opcode: Opcode::ArrayPush,
                        dest,
                        operand,
                    });
                }
                InstructionData::NewDictionary => {
                    let operand = self.gen_ir_operand(&ir, inst.result.unwrap());
                    unit.add_inst(VmCode::Single {
                        opcode: Opcode::NewDictionary,
                        dest: operand,
                    });
                }
                InstructionData::DictionaryPut { object, key, value } => {
                    let dest = self.gen_ir_operand(&ir, object);
                    let operand1 = self.gen_ir_operand(&ir, key);
                    let operand2 = self.gen_ir_operand(&ir, value);
                    unit.add_inst(VmCode::Triple {
                        opcode: Opcode::DictionaryPut,
                        dest,
                        operand1,
                        operand2,
                    });
                }
                InstructionData::LoadEnv { name } => {
                    let dest = self.gen_ir_operand(&ir, inst.result.unwrap());
                    let operand = self.gen_ir_operand(&ir, name);
                    unit.add_inst(VmCode::Double { opcode: Opcode::LoadEnv, dest, operand })
                }
                InstructionData::Call { func, args } => {
                    unimplemented!()
                }
                InstructionData::Return { value } => {
                    unimplemented!()
                }
            }
        }

        unit
    }

    fn gen_ir_operand(&self, ir: &IRModule, value: ValueRef) -> Operand {
        match ir.value_data(&value) {
            ValueData::Constant(v) => Operand::Immed(v.clone()),
            ValueData::Inst => Operand::VirtReg(VirtReg(value.0)),
        }
    }
}

enum Operand {
    Heap(usize),
    Stack(usize),
    Register(Register),
    Immed(Value),
    VirtReg(VirtReg),
}

pub enum VmCode {
    Single {
        opcode: Opcode,
        dest: Operand,
    },
    Double {
        opcode: Opcode,
        dest: Operand,
        operand: Operand,
    },
    Triple {
        opcode: Opcode,
        dest: Operand,
        operand1: Operand,
        operand2: Operand,
    },
}

pub struct CodeUnit {
    pub codes: Vec<VmCode>,
    pub consts: Vec<Value>,
}

impl CodeUnit {
    pub fn new() -> CodeUnit {
        CodeUnit {
            codes: Vec::new(),
            consts: Vec::new(),
        }
    }

    pub fn add_inst(&mut self, code: VmCode) {
        self.codes.push(code);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VirtReg(pub usize);

impl fmt::Display for VirtReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    Rbp = 13,
    Rsp = 14,
    Rpc = 15,
}

impl Register {
    pub fn registers() -> [Register; 8] {
        [
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
        ]
    }

    pub fn as_usize(&self) -> usize {
        *self as usize
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Register::R0 => write!(f, "R0"),
            Register::R1 => write!(f, "R1"),
            Register::R2 => write!(f, "R2"),
            Register::R3 => write!(f, "R3"),
            Register::R4 => write!(f, "R4"),
            Register::R5 => write!(f, "R5"),
            Register::R6 => write!(f, "R6"),
            Register::R7 => write!(f, "R7"),
            Register::R8 => write!(f, "R8"),
            Register::R9 => write!(f, "R9"),
            Register::R10 => write!(f, "R10"),
            Register::R11 => write!(f, "R11"),
            Register::R12 => write!(f, "R12"),
            Register::Rbp => write!(f, "Rbp"),
            Register::Rsp => write!(f, "Rsp"),
            Register::Rpc => write!(f, "Rpc"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackSlot {
    pub offset: usize,
    pub size: usize,
}

impl StackSlot {
    pub fn new(offset: usize, size: usize) -> StackSlot {
        StackSlot { offset, size }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl fmt::Display for StackSlot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rbp+{}", self.offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Address {
    None,
    Register(Register),
    Stack(StackSlot),
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
struct StackFrame {
    allocated_size: usize,
}

impl StackFrame {
    fn new() -> Self {
        StackFrame { allocated_size: 0 }
    }

    fn alloc(&mut self, size: usize) -> StackSlot {
        let offset = self.allocated_size;
        self.allocated_size += size;
        StackSlot::new(offset, size)
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

    fn allocated_stack_size(&self) -> usize {
        let mut stack_size = 0;
        for interval in &self.intervals {
            if let Address::Stack(stack_slot) = interval.address {
                stack_size += stack_slot.size;
            }
        }

        stack_size
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
                    Address::Stack(off) => Operand::Stack(off),
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
                    Address::Stack(off) => Operand::Stack(off),
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
                    Address::Stack(off) => {
                        let tmp_register = Register::R0;

                        insts.push(Instruction::single(
                            Opcode::Push,
                            Operand::Register(tmp_register),
                        ));
                        insts.push(Instruction::new(
                            opcode,
                            Operand::Register(tmp_register),
                            operand1,
                            operand2,
                        ));
                        insts.push(Instruction::two(
                            Opcode::Store,
                            Operand::Stack(off),
                            Operand::Register(tmp_register),
                        ));
                        insts.push(Instruction::single(
                            Opcode::Pop,
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
    live_intervals: BTreeMap<VirtReg, LiveInterval>,
    active_intervals: Vec<LiveInterval>,
    registers: RegisterPool,
    stack: StackFrame,
}

impl RegisterAllocator {
    pub fn new(registers: &[Register]) -> Self {
        Self {
            live_intervals: BTreeMap::new(),
            active_intervals: Vec::new(),
            registers: RegisterPool::new(registers),
            stack: StackFrame::new(),
        }
    }

    pub fn line_scan(&mut self, ir: &CodeUnit) -> VirtRegMap {
        self.build_live_interval(ir);

        let mut live_intervals = self.get_live_intervals();

        for (_i, interval) in live_intervals.iter_mut().enumerate() {
            self.expire_old_intervals(interval);
            if self.registers.is_empty() {
                self.spill_at_interval(interval);
            } else {
                interval.address = Address::Register(self.registers.alloc_register());
                self.active_intervals.sort_by_key(|interval| interval.end);
                self.active_intervals.push(*interval);
            }
        }

        VirtRegMap::new(live_intervals)
    }

    fn build_live_interval(&mut self, ir: &CodeUnit) {
        for (idx, ir) in ir.instructions.iter().enumerate() {
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
                self.registers.free_register(reg);
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

        let stack_slot = self.stack.alloc(1);
        let spill = self.active_intervals.last_mut().unwrap();

        if spill.end > interval.end {
            interval.address = spill.address;
            spill.address = Address::Stack(stack_slot);

            self.active_intervals.pop();
            self.active_intervals.sort_by_key(|interval| interval.end);
            self.active_intervals.push(*interval);
        } else {
            interval.address = Address::Stack(stack_slot);
        }
    }
}
