use crate::expanded::{CompleteObject, expand_pseudo_ops};
use crate::cst;
use lc3_isa::Word;

type MemoryDump = Vec<Word>;

fn assemble(cst: cst::File) -> MemoryDump {
    let expanded_objects = cst.objects.into_iter().map(expand_pseudo_ops); 
    MemoryDump::new()
}