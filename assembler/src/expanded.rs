// For expanded pseudo-op structures
use crate::cst;
use crate::cst::Operands;
use crate::error::MemoryError;
use lc3_isa;
use lc3_isa::Word;
use lc3_isa::{Addr, Instruction};
use std::collections::HashMap;

pub type File<'input> = Vec<cst::Object<'input>>;

pub struct Object<'input> {
    orig: Addr,
    memory_locations: Vec<MemoryLocation<'input>>,
}

pub enum MemoryLocation<'input> {
    Instruction(cst::Operation<'input>),
    Value(Word),
}

pub struct LabelValues<'input> {
    label: &'input str,
    mem: Word,
}

pub struct OffsetValues {
    mem: Word,
    offset: i16,
}

pub enum IsaInstructions {
    Instruction(Instruction),
    Value(Word),
}

pub type Label<'input> = &'input str;

fn assembler_pass_one(objects: Vec<cst::Object>) -> Result<Vec<OffsetValues>, MemoryError> {
    let mut orig_vals = Vec::<Object>::new();

    for object in objects {
        let mut expansion = Vec::new();
        let mut orig_val = Object {
            orig: 0,
            memory_locations: vec![],
        };
        for i in object.content.operations {
            match i.operands {
                Operands::Orig { origin } => {
                    orig_val = Object {
                        orig: origin.value.unwrap(),
                        memory_locations: vec![],
                    };
                }
                Operands::Blkw { size, .. } => {
                    let mut count = 0;
                    let val = size.value.unwrap();
                    while count < val {
                        expansion.push(MemoryLocation::Value(0));
                        count += 1;
                    }
                }
                Operands::Stringz { string } => {
                    //let str_length = (string.end - string.start) as u16;
                    for c in string.src.chars() {
                        expansion.push(MemoryLocation::Value(c as u16));
                    }
                }
                Operands::Fill { value } => {
                    expansion.push(MemoryLocation::Value(value.value.unwrap() as u16));
                }
                Operands::End {} => {
                    expansion.push(MemoryLocation::Instruction(i));
                }
                _ => {
                    expansion.push(MemoryLocation::Instruction(i));
                }
            };
        }

        orig_val.memory_locations = expansion;
        orig_vals.push(orig_val);
    }

    return assembler_pass_two(orig_vals);
}

fn assembler_pass_two(object_vec: Vec<Object>) -> Result<Vec<OffsetValues>, MemoryError> {
    let mut offset_reliant = Vec::<LabelValues>::new();
    let mut labels = HashMap::<&str, Word>::new();
    // let mut orig_flag = 0;
    for instruction_csts in object_vec {
        let origin = instruction_csts.orig;
        let mut memory = origin;

        for memory_locations in instruction_csts.memory_locations {
            match memory_locations {
                MemoryLocation::Instruction(instruction_cst) => {
                    match instruction_cst.operands {
                        Operands::Br { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::Jsr { label } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::Ld { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::Ldi { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::Lea { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::St { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }
                        Operands::Sti { label, .. } => {
                            memory += 1;
                            let offset_val = LabelValues {
                                label: label.value.unwrap(),
                                mem: memory,
                            };
                            offset_reliant.push(offset_val);
                        }

                        Operands::End {} => { break; }
                        _ => {
                            memory += 1;
                            if let Some(label) = instruction_cst.label {
                                labels.insert(label.value.unwrap(), memory);
                            };
                            memory += 1;
                        }
                    }
                }
                MemoryLocation::Value(_) => {
                    memory += 1;
                }
            };
        }
    }
    let mut offset_values = Vec::<OffsetValues>::new();

    for offset_instruction_csts in offset_reliant {
        let offset = (offset_instruction_csts.mem
            - labels.get(offset_instruction_csts.label).unwrap()) as i16;
        let structure = OffsetValues {
            mem: offset_instruction_csts.mem,
            offset: offset,
        };
        offset_values.push(structure); // holds the memory location to insert the offset at...
    }

    return Ok(offset_values);
}

fn assembly_pass_three(
    object_vec: Vec<Object>,
    offsets: Vec<OffsetValues>,
) -> Vec<IsaInstructions> {
    let mut isa_instruction_csts = Vec::<IsaInstructions>::new();
    let mut offset_counter = 0;
    for instruction_csts in object_vec {
        //let origin = Instruction_csts.orig;
        //let mut memory = origin;
        for memory_locations in instruction_csts.memory_locations {
            match memory_locations {
                MemoryLocation::Instruction(instruction_cst) => {
                    //memory += 1;
                    match instruction_cst.operands {
                        Operands::Add {
                            dr,
                            sr1,
                            sr2_or_imm5,
                        } => match sr2_or_imm5.unwrap() {
                            cst::Sr2OrImm5::Imm5(immediate) => {
                                isa_instruction_csts.push(IsaInstructions::Instruction(
                                    Instruction::new_add_imm(
                                        dr.value.unwrap(),
                                        sr1.value.unwrap(),
                                        immediate.value.unwrap(),
                                    ),
                                ));
                            }
                            cst::Sr2OrImm5::Sr2(src_reg) => {
                                isa_instruction_csts.push(IsaInstructions::Instruction(
                                    Instruction::new_add_reg(
                                        dr.value.unwrap(),
                                        sr1.value.unwrap(),
                                        src_reg.value.unwrap(),
                                    ),
                                ));
                            }
                        },
                        Operands::And {
                            dr,
                            sr1,
                            sr2_or_imm5,
                        } => match sr2_or_imm5.unwrap() {
                            cst::Sr2OrImm5::Imm5(immediate) => {
                                isa_instruction_csts.push(IsaInstructions::Instruction(
                                    Instruction::new_and_imm(
                                        dr.value.unwrap(),
                                        sr1.value.unwrap(),
                                        immediate.value.unwrap(),
                                    ),
                                ));
                            }
                            cst::Sr2OrImm5::Sr2(src_reg) => {
                                isa_instruction_csts.push(IsaInstructions::Instruction(
                                    Instruction::new_and_reg(
                                        dr.value.unwrap(),
                                        sr1.value.unwrap(),
                                        src_reg.value.unwrap(),
                                    ),
                                ));
                            }
                        },
                        Operands::Ld { dr, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_ld(dr.value.unwrap(), my_offset),
                            ));
                        }
                        Operands::Ldi { dr, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_ldi(dr.value.unwrap(), my_offset),
                            ));
                        }
                        Operands::Ldr { dr, base, offset6 } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_ldr(
                                    dr.value.unwrap(),
                                    base.value.unwrap(),
                                    offset6.value.unwrap(),
                                ),
                            ));
                        }
                        Operands::Lea { dr, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_lea(dr.value.unwrap(), my_offset),
                            ));
                        }

                        Operands::St { sr, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_st(sr.value.unwrap(), my_offset),
                            ));
                        }

                        Operands::Sti { sr, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_sti(sr.value.unwrap(), my_offset),
                            ));
                        }

                        Operands::Str { sr, base, offset6 } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_str(
                                    sr.value.unwrap(),
                                    base.value.unwrap(),
                                    offset6.value.unwrap(),
                                ),
                            ));
                        }

                        Operands::Not { dr, sr } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_not(dr.value.unwrap(), sr.value.unwrap()),
                            ));
                        }

                        Operands::Br { nzp, .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            let condition_codes = nzp.unwrap();
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_br(
                                    condition_codes.n,
                                    condition_codes.z,
                                    condition_codes.p,
                                    my_offset,
                                ),
                            ));
                        }

                        Operands::Jmp { base } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_jmp(base.value.unwrap()),
                            ));
                        }

                        Operands::Jsr { .. } => {
                            let my_offset = offsets[offset_counter].offset;
                            offset_counter += 1;
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_jsr(my_offset),
                            ));
                        }

                        Operands::Jsrr { base } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_jsrr(base.value.unwrap()),
                            ));
                        }

                        Operands::Ret {} => {
                            isa_instruction_csts
                                .push(IsaInstructions::Instruction(Instruction::new_ret()));
                        }

                        Operands::Rti {} => {
                            isa_instruction_csts
                                .push(IsaInstructions::Instruction(Instruction::new_rti()));
                        }
                        Operands::Trap { trap_vec } => {
                            isa_instruction_csts.push(IsaInstructions::Instruction(
                                Instruction::new_trap(trap_vec.value.unwrap()),
                            ))
                        }
                        _ => {}
                    }
                }
                MemoryLocation::Value(value) => {
                    isa_instruction_csts.push(IsaInstructions::Value(value));
                }
            }
        }
    }

    return isa_instruction_csts;
}
