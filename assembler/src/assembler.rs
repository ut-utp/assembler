use crate::expanded::{expand_pseudo_ops, build_symbol_table, validate_placement, construct_instructions, CompleteObject, InsnOrValue, InsnOrValueWithSrc};
use crate::cst;
use lc3_isa::{ADDR_SPACE_SIZE_IN_WORDS, Addr};

use lc3_isa::util::MemoryDump;

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
    where O: IntoIterator<Item=cst::Object<'input>>
{
    let complete_objects = assemble_to_queryable_objects(objects);
    assemble_queryable_objects(complete_objects, background)
}


pub fn assemble_to_queryable_objects<'input, O>(objects: O) -> QueryableObject<'input>
    where O: IntoIterator<Item=cst::Object<'input>>
{
    let expanded_objects = objects.into_iter().map(expand_pseudo_ops).collect();
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
