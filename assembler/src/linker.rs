use std::collections::HashMap;
use std::num::{ParseIntError, TryFromIntError};
use chumsky::chain::Chain;
use chumsky::Parser;
use lc3_isa::util::MemoryDump;
use lc3_isa::{Addr, ADDR_SPACE_SIZE_IN_WORDS, Word};
use crate::assembler::{assemble_instruction, SymbolTable, Object, ObjectWord, AssemblyResult};

struct LinkedObject {
    origin: Addr,
    words: Vec<Word>,
}

fn layer_object(image: &mut [Word; ADDR_SPACE_SIZE_IN_WORDS], object: LinkedObject) {
    let LinkedObject { origin, words } = object;
    let mut addr = origin as usize;
    for word in words {
        image[addr] = word;
        addr += 1;
    }
}

fn link_object(symbol_table: &SymbolTable, object: Object) -> Result<LinkedObject, TryFromIntError> {
    let mut words = Vec::new();
    let Object { origin, words: object_words, .. } = object;
    let mut location_counter = origin;
    for object_word in object_words {
        match object_word {
            ObjectWord::Value(word) => {
                words.push(word);
                location_counter += 1;
            },
            ObjectWord::UnlinkedInstruction(instruction) =>
                match assemble_instruction(&symbol_table, &location_counter, instruction)? {
                    AssemblyResult::SingleObjectWord(word) => match word {
                        ObjectWord::Value(word) => {
                            words.push(word);
                            location_counter += 1;
                        }
                        ObjectWord::UnlinkedInstruction(_) => panic!("Failed to link an instruction")
                    }
                    AssemblyResult::MultipleObjectWords(ows) => {
                        let mut ws = ows.into_iter()
                            .map(|ow| match ow {
                                ObjectWord::Value(word) => word,
                                ObjectWord::UnlinkedInstruction(_) => panic!("Unexpected unlinked instruction")
                            })
                            .collect::<Vec<_>>();
                        location_counter += ws.len() as u16;
                        words.extend(ws);
                    }
                }
        }
    }
    Ok(LinkedObject { origin, words })
}

pub fn link(objects: impl IntoIterator<Item=Object>, overlay_on_os: bool) -> Result<MemoryDump, TryFromIntError> {
    let objects = objects.into_iter().collect::<Vec<_>>();

    let mut symbol_table = HashMap::new();
    for object in objects.iter() {
        for (label, addr) in object.symbol_table.iter() {
            symbol_table.insert(label.clone(), *addr);
        }
    }

    let mut image =
        if overlay_on_os {
            let mut os = lc3_os::OS_IMAGE.clone().0;
            os[lc3_isa::USER_PROGRAM_START_ADDR as usize] =
                objects.get(0)
                    .expect("Found no objects in file; could not find origin.")
                    .origin; // TODO: fail gracefully
            os
        } else {
            [0; ADDR_SPACE_SIZE_IN_WORDS]
        };
    for object in objects {
        let linked_object = link_object(&symbol_table, object)?;
        layer_object(&mut image, linked_object);
    }

    Ok(image.into())
}