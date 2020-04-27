use crate::ir::{ir4_parse_ambiguous_tokens, ir5_expand_pseudo_ops};
use lc3_isa::{ADDR_SPACE_SIZE_IN_WORDS, Addr, Instruction, Word, SignedWord};

use lc3_isa::util::MemoryDump;
use crate::analysis::symbol_table::{SymbolTable, build_symbol_table};
use std::collections::HashMap;
use crate::ir::ir4_parse_ambiguous_tokens::{UnsignedImmOrLabel, Operands, ImmOrLabel};
use crate::ir::ir5_expand_pseudo_ops::expand_pseudo_ops;
use crate::analysis::memory_placement::validate_placement;

pub struct QueryableObject<'input> {
    segments: Vec<CompleteObject<'input>>
}

impl<'input> QueryableObject<'input> {
    pub fn get_source(&self, address: Addr) -> Option<Vec<String>> {
        self.segments.iter()
            .map(|o| o.get_source(address))
            .find(Option::is_some)
            .flatten()
    }
}

pub fn assemble<'input, O>(objects: O, background: Option<MemoryDump>) -> MemoryDump
    where O: IntoIterator<Item=ir4_parse_ambiguous_tokens::Object<'input>>
{
    let complete_objects = assemble_to_queryable_objects(objects);
    assemble_queryable_objects(complete_objects, background)
}

pub fn assemble_to_queryable_objects<'input, O>(objects: O) -> QueryableObject<'input>
    where O: IntoIterator<Item=ir4_parse_ambiguous_tokens::Object<'input>>
{
    let expanded_objects = expand_pseudo_ops(objects);
    validate_placement(&expanded_objects).unwrap();
    let segments = expanded_objects.into_iter()
        .map(|o| {
            let symbol_table = build_symbol_table(&o).unwrap();
            construct_instructions(o, symbol_table)
        })
        .collect();
    QueryableObject { segments }
}


pub fn assemble_queryable_objects(queryable_object: QueryableObject, background: Option<MemoryDump>) -> MemoryDump {
    let mut memory = background.unwrap_or(MemoryDump([0x0000; ADDR_SPACE_SIZE_IN_WORDS]));
    for complete_object in queryable_object.segments {
        let mut i = complete_object.orig as usize;
        for insn_or_value_with_src in complete_object.insns_or_values {
            let InsnOrValueWithSrc { insn_or_value, .. } = insn_or_value_with_src;
            memory[i] = match insn_or_value {
                InsnOrValue::Instruction(insn) => insn.into(),
                InsnOrValue::Value(value) => value,
            };
            i += 1;
        }
    }

    memory
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

pub fn construct_instructions<'input>(object: ir5_expand_pseudo_ops::Object, symbol_table: HashMap<&'input str, Addr>) -> CompleteObject<'input> {
    let orig = object.origin.unwrap();
    let mut current_location = orig;
    let mut insns_or_values = Vec::new();
    for operation in object.content.operations {
        let (insn_or_value, src_lines) = match operation.1 {
            OpOrValue::Operation(ir4_parse_ambiguous_tokens::Operation { operands: Operands::Fill { value }, src_lines, .. }) => {
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
                        ir4_parse_ambiguous_tokens::Sr2OrImm5::Imm5(immediate) => Instruction::new_add_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        ir4_parse_ambiguous_tokens::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_add_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
                    },
                    Operands::And { dr, sr1, sr2_or_imm5, } => match sr2_or_imm5.unwrap() {
                        ir4_parse_ambiguous_tokens::Sr2OrImm5::Imm5(immediate) => Instruction::new_and_imm(dr.unwrap(), sr1.unwrap(), immediate.unwrap()),
                        ir4_parse_ambiguous_tokens::Sr2OrImm5::Sr2(src_reg)    => Instruction::new_and_reg(dr.unwrap(), sr1.unwrap(), src_reg.unwrap()),
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

fn compute_offset(pc_offset: ir4_parse_ambiguous_tokens::Checked<ImmOrLabel>, location: Addr, symbol_table: &HashMap<&str, Addr>) -> SignedWord {
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
