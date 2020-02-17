// For expanded pseudo-op structures
use crate::cst;
use lc3_isa::{Addr, SignedWord};
use crate::cst::{Operation, Operands, Immediate};
use lc3_isa::{Word};
use crate::lexer::Opcode;
use crate::error::{MemoryError, ParseError};
use std::collections::HashMap;


pub type File<'input> = Vec<cst::Object<'input>>;


pub struct Object_expanded<'input> {
    orig: Addr,
    memory_locations: Vec<MemoryLocation<'input>>,
}

pub enum MemoryLocation<'input> {
    Instruction(Instruction<'input>),
    Value(Word),
}

pub struct Label_values<'input> {
   label: &'input str,
    mem: Word,
}

pub struct Offset_values {
    mem: Word,
    offset: i16,
}

pub type Label<'input> = &'input str;

pub type Instruction<'input> = Operation<'input>;


fn assembler_pass_one(object: cst::Object) -> Result<Vec<Offset_values>, MemoryError> {
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


fn assembler_pass_two(instructions: Object_expanded) -> Result<Vec<Offset_values>, MemoryError> {
    let origin = instructions.orig;
    let mut memory = origin;
   // let mut labels = HashMap::<&str, Word>::new();
    let mut offset_reliant = Vec::<Label_values>::new();
    // let mut origin_placement = 0;
    let mut orig_flag = 0;
    let mut labels = HashMap::<&str, Word>::new();
    for memory_locations in instructions.memory_locations{
        match memory_locations {
            MemoryLocation::Instruction(instruction) => {
                match instruction.operands {
                    Operands::Br {nzp, nzp_src, label} => {
                        memory += 1;

                        let offset_val = Label_values {
                            label: label.value.unwrap(),
                            mem: memory,
                        };
                        offset_reliant.push(offset_val);
                    },
                    // Operands::Ret => {
                    //     memory += 1;


                    // },
                    // Operands::Rti => {
                    //     memory += 1;

                    // },
                    Operands::Jsr { label }  => {
                        memory += 1;
                        let offset_val = Label_values {
                            label: label.value.unwrap(),
                            mem: memory,
                        };
                        offset_reliant.push(offset_val);
                    },
                    Operands::End {} => {
                        memory += 1; 
                        break; 
                    },
                    _ => {
                        memory += 1;
                        if let Some(label) = instruction.label {
                            labels.insert(
                                label.value.unwrap(),
                                memory,
                            );
                            
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
    };
    let mut offset_values = Vec::<Offset_values>::new();
    
    for offset_instructions in offset_reliant {
        let offset = (offset_instructions.mem - labels.get(offset_instructions.label).unwrap()) as i16; 
        let structure = Offset_values{
            mem: offset_instructions.mem,
            offset: offset,
        };
        offset_values.push(structure);
    };
    

    if orig_flag == 1{
        Err(MemoryError("Lone Origin value".to_string()))
    } else {
        return Ok(offset_values);
        //Ok(memory < 2^16-1)

    }
}

// fn assembler_pass_three(instructions: Object_expanded, labels: HashMap<&str, Word>) {
//     let mut offsets = Vec::<Offset_values>::new();
//     let mut pc = instructions.orig;

//     for memory_locations in instructions.memory_locations{ 
//         match memory_locations {
//             MemoryLocation::Instruction(instruction) => {
//                 match instruction.operands {
//                     Operands::Br {nzp, nzp_src, label} => {
//                         pc += 1;
                        
//                         let pc_value = labels.get(label.value.unwrap());
//                         let offset = (pc - pc_value.unwrap()) as i16; 
//                         let offset_val = Offset_values {
//                             instruction: instruction,
//                             offset: offset,
//                         };
                      
//                     },
//                     Operands::Ret => {
//                         pc += 1;
//                     },
//                     Operands::Rti => {
//                         pc += 1;
                    
//                     },
//                     _ => {
//                         pc += 1;
//                     }
                    
//                 }
//             },
//             MemoryLocation::Value(value) => {
        
//             },
           
//         };
//     };



// }

