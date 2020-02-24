use crate::expanded::{expand_pseudo_ops, build_symbol_table, validate_placement, construct_instructions, CompleteObject, InsnOrValue};
use crate::cst;
use lc3_isa::ADDR_SPACE_SIZE_IN_WORDS;

use lc3_isa::util::MemoryDump;

pub fn assemble<'input, O>(objects: O) -> MemoryDump
    where O: IntoIterator<Item=cst::Object<'input>>
{
    let expanded_objects = objects.into_iter().map(expand_pseudo_ops).collect();
    validate_placement(&expanded_objects).unwrap();
    let complete_objects: Vec<CompleteObject> = expanded_objects.into_iter()
        .map(|o| {
            let symbol_table = build_symbol_table(&o).unwrap();
            construct_instructions(o, symbol_table)
        })
        .collect();

    let mut memory = [0x0000; ADDR_SPACE_SIZE_IN_WORDS];
    for complete_object in complete_objects {
        let mut i = complete_object.orig as usize;
        for insn_or_value in complete_object.insns_or_values {
            memory[i] = match insn_or_value {
                InsnOrValue::Instruction(insn) => insn.into(),
                InsnOrValue::Value(value) => value,
            };
            i += 1;
        }
    }

    MemoryDump(memory)
}