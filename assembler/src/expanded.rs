// For expanded pseudo-op structures
use crate::cst;
use lc3_isa::{Addr, SignedWord};
use crate::cst::{Operation, Operands, Immediate};
use lc3_isa::{Word};
use crate::lexer::Opcode;
use crate::error::{MemoryError, ParseError};

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


fn assembler_pass_one(object: cst::Object) -> Result<bool, MemoryError> {
    let mut expansion = Vec::<>::new();
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

            Operands::Orig{ origin } => {
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
            _ => { 
                //labels.push(i.label.unwrap().value);
                expansion.push(MemoryLocation::Instruction(i)); 
            
            }
        };
        

    };
     orig_val.memory_locations = expansion;

    return assembler_pass_two(orig_val);
  


}


fn assembler_pass_two(instructions: Object_expanded) -> Result<bool, MemoryError> {
    let origin = instructions.orig;
    let mut memory = origin;
    let mut labels = Vec::<Word>::new();
    // let mut origin_placement = 0;
    let mut orig_flag = 0;
    for memory_locations in instructions.memory_locations{
        match memory_locations {
            MemoryLocation::Instruction(instruction) => {
                match instruction.operands {
                    Operands::End {} => {
                        memory += 1; 
                        break; 
                    },
                    _ => {
                        if instruction.label.unwrap().value.unwrap() != "" {
                            labels.push(memory);

                        };
                        memory += 1; 
                        }
                }
            },
            MemoryLocation::Value(value) => {
                memory += 1; 
            },
             _ => {
                // otherwise lone orig value
                orig_flag = 1; 
             }
        };
    }




    if orig_flag == 1{
        Err(MemoryError("Lone Origin value".to_string()))
    } else {
        Ok(memory < 2^16-1)
    }
}




// fn assembler_pass_three(instructions: Object_expanded, object: cst::Object) {
//     let check = assembler_pass_two(instructions);
//     if check.unwrap() {
//         let mut locations = Vec::<cst::Checked<Word>>::new();
//         for i in object.operations{
//             locations.push(i.label.unwrap().value.unwrap());
//         };

//     } else {



//     }



// }