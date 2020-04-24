// For expanded pseudo-op structures
use crate::ir::ir4_validate_ambiguous_tokens;
use crate::ir::ir4_validate_ambiguous_tokens::{Operands, ImmOrLabel, UnsignedImmOrLabel, Checked};
use crate::error::MemoryError;
use lc3_isa;
use lc3_isa::{Word, SignedWord};
use lc3_isa::{Addr, Instruction};
use std::collections::HashMap;
use std::iter::repeat;
use itertools::Itertools;

pub type SymbolTable<'input> = HashMap<&'input str, Addr>;
pub type File<'input> = Vec<ir4_validate_ambiguous_tokens::Object<'input>>;

pub struct Object<'input> {
    orig: Addr,
    ops_or_values: Vec<(Option<Label<'input>>, OpOrValue<'input>)>,
}

#[derive(Clone)]
pub enum OpOrValue<'input> {
    Operation(ir4_validate_ambiguous_tokens::Operation<'input>),
    Value(Word),
}

pub struct CompleteObject<'input> {
    pub orig: Addr,
    pub insns_or_values: Vec<InsnOrValueWithSrc>,
    pub symbol_table: SymbolTable<'input>,
}

impl<'input> CompleteObject<'input> {
    pub fn get_source(&self, address: Addr) -> Option<Vec<String>> {
        if address < self.orig {
            return None;
        }
        let offset = (address - self.orig) as usize;
        let insn_or_value = self.insns_or_values.get(offset);
        if let Some(InsnOrValueWithSrc { src_lines, .. }) = insn_or_value {
            Some(src_lines.clone())
        } else {
            None
        }
    }

    pub fn get_label_addr(&self, label: &str) -> Option<&Addr> {
        self.symbol_table.get(label)
    }
}

pub struct InsnOrValueWithSrc {
    pub src_lines: Vec<String>,
    pub insn_or_value: InsnOrValue,
}

pub enum InsnOrValue {
    Instruction(Instruction),
    Value(Word),
}

pub type Label<'input> = &'input str;

pub fn expand_pseudo_ops(object: ir4_validate_ambiguous_tokens::Object) -> Object {
    let ir4_validate_ambiguous_tokens::Object { origin, content, .. } = object;
    
    let orig = origin.unwrap();

    let mut ops_or_values = Vec::new();
    for operation in content.operations {
        let label = operation.label.clone().map(Checked::unwrap);
        let mut values = Vec::new();
        match operation.operands {
            Operands::Blkw { size, .. } => {
                let num_values = size.unwrap() as usize;
                let block = repeat((None, OpOrValue::Value(0))).take(num_values);
                values.extend(block);
            },
            Operands::Stringz { string } => {
                for c in string.unwrap().chars() {
                    values.push((None, OpOrValue::Value(c as Word)));
                }
                values.push((None, OpOrValue::Value(0))); // null-terminate
            },
            Operands::End => { /* ignore */ },
            _ => {
                values.push((None, OpOrValue::Operation(operation)));
            },
        };
        let first = values.get_mut(0);
        if let Some(first_value) = first { // TODO: how to handle other case?
            first_value.0 = label;
        }
        ops_or_values.extend(values);
    }

    Object { orig, ops_or_values }
}

pub fn build_symbol_table<'input>(object: &Object<'input>) -> Result<HashMap<&'input str, Addr>, MemoryError> {
    let mut symbol_table = HashMap::new();
    let mut current_location = object.orig;
    for op_or_value in object.ops_or_values.iter() {
        if let Some(label) = op_or_value.0 {
            let other_location = symbol_table.insert(label.clone(), current_location);
            if let Some(_) = other_location {
                return Err(MemoryError("Duplicate label at different location.".to_string()))
            }
        }
        current_location += 1;
    };
    Ok(symbol_table)
}

pub fn validate_placement(objects: &Vec<Object>) -> Result<(), MemoryError> {
    let starts_and_ends = objects.iter().map(get_start_and_end);
    for ((_, prev_end), (next_start, _)) in starts_and_ends.tuple_windows() {
        if prev_end > next_start {
            return Err(MemoryError("Objects overlap.".to_string()));
        }
    }
    Ok(())
}

fn get_start_and_end(object: &Object) -> (Addr, Addr) {
    let start = object.orig;
    let end = start + object.ops_or_values.len() as Addr;
    (start, end)
}

pub fn construct_instructions<'input>(object: Object, symbol_table: HashMap<&'input str, Addr>) -> CompleteObject<'input> {
    let orig = object.orig;
    let mut current_location = object.orig;
    let mut insns_or_values = Vec::new();
    for op_or_value in object.ops_or_values {
        let (insn_or_value, src_lines) = match op_or_value.1 {
            OpOrValue::Operation(ir4_validate_ambiguous_tokens::Operation { operands: Operands::Fill { value }, src_lines, .. }) => {
                let value = match value.unwrap() {
                    UnsignedImmOrLabel::Imm(immediate) => immediate.unwrap(),
                    UnsignedImmOrLabel::Label(label) => {
                        let label = label.unwrap();
                        symbol_table.get(label).unwrap().clone()
                    },
                };
                (InsnOrValue::Value(value), src_lines)
            },
            OpOrValue::Operation(instruction_cst) => {
                let nzp = instruction_cst.nzp.unwrap();
                let src_lines = instruction_cst.src_lines;
                let insn = match instruction_cst.operands {
                    Operands::Add { dr, sr1, sr2_or_imm5 } => match sr2_or_imm5.unwrap() {
                        ir4_validate_ambiguous_tokens::Sr2OrImm5::Imm5(immediate) => Instruction::new_add_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        ir4_validate_ambiguous_tokens::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_add_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    },
                    Operands::And { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.unwrap() {
                        ir4_validate_ambiguous_tokens::Sr2OrImm5::Imm5(immediate) => Instruction::new_and_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        ir4_validate_ambiguous_tokens::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_and_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    },
                    
                    Operands::Ld { dr, pc_offset9 } => Instruction::new_ld(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                    Operands::Ldi { dr, pc_offset9 } => Instruction::new_ldi(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                    Operands::Ldr { dr, base, offset6 } => Instruction::new_ldr(dr.unwrap(), base.unwrap(), offset6.unwrap()),
                    Operands::Lea { dr, pc_offset9 } => Instruction::new_lea(dr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),

                    Operands::St { sr, pc_offset9 } => Instruction::new_st(sr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                    Operands::Sti { sr, pc_offset9 } => Instruction::new_sti(sr.unwrap(), compute_offset(pc_offset9, current_location, &symbol_table)),
                    Operands::Str { sr, base, offset6 } => Instruction::new_str(sr.unwrap(), base.unwrap(), offset6.unwrap()),
                    
                    Operands::Not { dr, sr } => Instruction::new_not(dr.unwrap(), sr.unwrap()),

                    Operands::Br { pc_offset9, .. } => {
                        let nzp = nzp.unwrap();
                        Instruction::new_br(nzp.n, nzp.z, nzp.p, compute_offset(pc_offset9, current_location, &symbol_table))
                    }

                    Operands::Jmp { base } => Instruction::new_jmp(base.unwrap()),
                    Operands::Jsr { pc_offset11 } => Instruction::new_jsr(compute_offset(pc_offset11, current_location, &symbol_table)),
                    Operands::Jsrr { base } => Instruction::new_jsrr(base.unwrap()),
                    
                    Operands::Ret => Instruction::new_ret(),
                    Operands::Rti => Instruction::new_rti(),
                    
                    Operands::Trap { trap_vec } => Instruction::new_trap(trap_vec.unwrap()),
                    Operands::Getc  => Instruction::new_trap(0x20),
                    Operands::Out   => Instruction::new_trap(0x21),
                    Operands::Puts  => Instruction::new_trap(0x22),
                    Operands::In    => Instruction::new_trap(0x23),
                    Operands::Putsp => Instruction::new_trap(0x24),
                    Operands::Halt  => Instruction::new_trap(0x25),

                    _ => unreachable!() // TODO: restructure enum to avoid this
                };
                (InsnOrValue::Instruction(insn), src_lines)
            }
            OpOrValue::Value(value) => (InsnOrValue::Value(value), vec![])
        };
        insns_or_values.push(InsnOrValueWithSrc {
            insn_or_value,
            src_lines
        });
        current_location += 1;
    }

    CompleteObject { orig, insns_or_values, symbol_table }
}

fn compute_offset(pc_offset: ir4_validate_ambiguous_tokens::Checked<ImmOrLabel>, location: Addr, symbol_table: &HashMap<&str, Addr>) -> SignedWord {
    match pc_offset.unwrap() {
        ImmOrLabel::Label(label) => {
            let label = label.unwrap();
            let label_location = symbol_table.get(label).unwrap().clone();
            let label_location = label_location as i64;
            let offset_base = (location + 1) as i64;
            (label_location - offset_base) as SignedWord
        }
        ImmOrLabel::Imm(immediate) => immediate.value.unwrap()
    }
}