// For expanded pseudo-op structures

use crate::cst::Operation;
use lc3_isa::{Word, Addr};

pub type File<'input> = Vec<Object<'input>>;

pub struct Object<'input> {
    orig: Addr,
    memory_locations: Vec<MemoryLocation<'input>>,
}

pub enum MemoryLocation<'input> {
    Instruction(Instruction<'input>),
    Value(Word),
}

pub type Label<'input> = &'input str;

pub type Instruction<'input> = Operation<'input>;
