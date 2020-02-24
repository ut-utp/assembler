// For expanded pseudo-op structures
use crate::cst;
use crate::cst::Operands;
use crate::error::MemoryError;
use lc3_isa;
use lc3_isa::{Word, SignedWord};
use lc3_isa::{Addr, Instruction};
use std::collections::HashMap;
use std::iter::repeat;
use itertools::Itertools;

pub type File<'input> = Vec<cst::Object<'input>>;

pub struct Object<'input> {
    orig: Addr,
    ops_or_values: Vec<OpOrValue<'input>>,
}

#[derive(Clone)]
pub enum OpOrValue<'input> {
    Operation(cst::Operation<'input>),
    Value(Word),
}

pub struct CompleteObject {
    pub orig: Addr,
    pub insns_or_values: Vec<InsnOrValue>,
}

pub enum InsnOrValue {
    Instruction(Instruction),
    Value(Word),
}

pub type Label<'input> = &'input str;

pub fn expand_pseudo_ops(object: cst::Object) -> Object {
    let cst::Object { origin, content, .. } = object;
    
    let orig = origin.unwrap();

    let mut ops_or_values = Vec::new();
    for operation in content.operations {
        match operation.operands {
            Operands::Blkw { size, .. } => {
                let num_values = size.unwrap() as usize;
                let values = repeat(OpOrValue::Value(0)).take(num_values);
                ops_or_values.extend(values);
            },
            Operands::Stringz { string } => {
                for c in string.src.chars() { // TODO: handle escape chars (\\, \", others?)
                    ops_or_values.push(OpOrValue::Value(c as Word));
                }
            },
            Operands::Fill { value } => {
                ops_or_values.push(OpOrValue::Value(value.unwrap()));
            },
            Operands::End => { /* ignore */ },
            _ => {
                ops_or_values.push(OpOrValue::Operation(operation));
            },
        };
    }
    
    Object { orig, ops_or_values }
}

pub fn build_symbol_table<'input>(object: &Object<'input>) -> Result<HashMap<&'input str, Addr>, MemoryError> {
    let mut symbol_table = HashMap::new();
    let mut current_location = object.orig;
    for op_or_value in object.ops_or_values.iter() {
        if let OpOrValue::Operation(cst::Operation { label: Some(label), .. }) = op_or_value {
            let other_location = symbol_table.insert(label.clone().unwrap(), current_location);
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

pub fn construct_instructions(object: Object, symbol_table: HashMap<&str, Addr>) -> CompleteObject {
    let orig = object.orig;
    let mut current_location = object.orig;
    let mut insns_or_values = Vec::new();
    for op_or_value in object.ops_or_values {
        let insn_or_value = match op_or_value {
            OpOrValue::Operation(instruction_cst) => {
                let insn = match instruction_cst.operands {
                    Operands::Add { dr, sr1, sr2_or_imm5 } => match sr2_or_imm5.unwrap() {
                        cst::Sr2OrImm5::Imm5(immediate) => Instruction::new_add_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        cst::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_add_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    },
                    Operands::And { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.unwrap() {
                        cst::Sr2OrImm5::Imm5(immediate) => Instruction::new_and_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        cst::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_and_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    },
                    
                    Operands::Ld { dr, label } => Instruction::new_ld(dr.unwrap(), compute_offset(label, current_location, &symbol_table)),
                    Operands::Ldi { dr, label } => Instruction::new_ldi(dr.unwrap(), compute_offset(label, current_location, &symbol_table)),
                    Operands::Ldr { dr, base, offset6 } => Instruction::new_ldr(dr.unwrap(), base.unwrap(), offset6.unwrap()),
                    Operands::Lea { dr, label } => Instruction::new_lea(dr.unwrap(), compute_offset(label, current_location, &symbol_table)),

                    Operands::St { sr, label } => Instruction::new_st(sr.unwrap(), compute_offset(label, current_location, &symbol_table)),
                    Operands::Sti { sr, label } => Instruction::new_sti(sr.unwrap(), compute_offset(label, current_location, &symbol_table)),
                    Operands::Str { sr, base, offset6 } => Instruction::new_str(sr.unwrap(), base.unwrap(), offset6.unwrap()),
                    
                    Operands::Not { dr, sr } => Instruction::new_not(dr.unwrap(), sr.unwrap()),

                    Operands::Br { nzp, label, .. } => {
                        let nzp = nzp.unwrap();
                        Instruction::new_br(nzp.n, nzp.z, nzp.p, compute_offset(label, current_location, &symbol_table))
                    }

                    Operands::Jmp { base } => Instruction::new_jmp(base.unwrap()),
                    Operands::Jsr { label } => Instruction::new_jsr(compute_offset(label, current_location, &symbol_table)),
                    Operands::Jsrr { base } => Instruction::new_jsrr(base.unwrap()),
                    
                    Operands::Ret => Instruction::new_ret(),
                    Operands::Rti => Instruction::new_rti(),
                    
                    Operands::Trap { trap_vec } => Instruction::new_trap(trap_vec.unwrap()),
                    Operands::Getc  => Instruction::new_trap(0x0020),
                    Operands::Out   => Instruction::new_trap(0x0021),
                    Operands::Puts  => Instruction::new_trap(0x0022),
                    Operands::In    => Instruction::new_trap(0x0023),
                    Operands::Putsp => Instruction::new_trap(0x0024),
                    Operands::Halt  => Instruction::new_trap(0x0025),

                    _ => unreachable!() // TODO: restructure enum to avoid this
                };
                InsnOrValue::Instruction(insn)
            }
            OpOrValue::Value(value) => InsnOrValue::Value(value)
        };
        insns_or_values.push(insn_or_value);
        current_location += 1;
    }

    CompleteObject { orig, insns_or_values }
}

fn compute_offset(label: cst::Checked<&str>, location: Addr, symbol_table: &HashMap<&str, Addr>) -> SignedWord {
    let label_location = symbol_table.get(label.unwrap()).unwrap().clone() as u32;
    let offset_base = (location + 1) as u32;
    (label_location - offset_base) as SignedWord
}