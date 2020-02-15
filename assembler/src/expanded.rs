// For expanded pseudo-op structures
use crate::cst;
use lc3_isa::{Addr, SignedWord};
use crate::cst::{Operation, Operands, Immediate};
use lc3_isa::{Word};
use crate::lexer::Opcode;

pub type File<'input> = Vec<cst::Object<'input>>;

pub struct Object_expanded<'input> {
    orig: Addr,
    memory_locations: Vec<MemoryLocation<'input>>,
}

pub enum MemoryLocation<'input> {
    Instruction(Instruction<'input>),
    Value(Word),
}

pub type Label<'input> = &'input str;

pub type Instruction<'input> = Operation<'input>;


fn assembler_pass_one(object: cst::Object) -> Object_expanded{
    let mut expansion = Vec::<MemoryLocation>::new();
    let mut orig_val = Object_expanded {
        orig: 0,
        memory_locations: vec![],
    };
    // let mut end_val = Object_expanded {
    //     orig: 0,
    //     memory_locations: vec![],
    // };
    for i in object.operations {
        
        match i.operands {

            Operands::Orig{origin } => {
                 orig_val = Object_expanded {
                    orig: origin.value.unwrap(),
                    memory_locations: vec![],
                };        
            },
            Operands::Blkw {size_src, size} => {
                let mut count = 0; 
                let val = size.value.unwrap();
                while count <  val {
                    expansion.push(MemoryLocation::Value(0));
                }
            },
            Operands::Stringz {string} => {
                let str_length = (string.end - string.start) as u16;
                for c in string.src.chars() {
                    expansion.push(MemoryLocation::Value(c as u16));
                }
            },
            Operands::Fill { value } => {
                expansion.push(MemoryLocation::Value(value.value.unwrap() as u16));
            },
            Operands::End {} => {
                expansion.push(MemoryLocation::Instruction(i));
            },
            _ => { expansion.push(MemoryLocation::Instruction(i)); }
        }

    }
     orig_val.memory_locations = expansion;

    return orig_val;


}