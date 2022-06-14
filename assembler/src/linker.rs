use std::collections::HashMap;
use std::num::{ParseIntError, TryFromIntError};
use chumsky::chain::Chain;
use chumsky::Parser;
use lc3_isa::util::MemoryDump;
use lc3_isa::{Addr, ADDR_SPACE_SIZE_IN_WORDS, Word};
use crate::assembler::{assemble_instruction, SymbolTable, Object, ObjectWord, AssemblyResult, Region};

struct LinkedRegion {
    origin: Addr,
    words: Vec<Word>,
}

fn layer_region(image: &mut [Word; ADDR_SPACE_SIZE_IN_WORDS], object: LinkedRegion) {
    let LinkedRegion { origin, words } = object;
    let mut addr = origin as usize;
    for word in words {
        image[addr] = word;
        addr += 1;
    }
}

fn link_region(symbol_table: &SymbolTable, region: Region) -> Result<LinkedRegion, TryFromIntError> {
    let mut words = Vec::new();
    let Region { origin, words: region_words, .. } = region;
    let mut location_counter = origin;
    for region_word in region_words {
        match region_word {
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
    Ok(LinkedRegion { origin, words })
}

pub fn link(objects: impl IntoIterator<Item=Object>, overlay_on_os: bool) -> Result<MemoryDump, crate::analysis::SingleError> {
    let objects = objects.into_iter().collect::<Vec<_>>();

    let mut symbol_table = HashMap::new();
    for object in objects.iter() {
        for (label, addr) in object.symbol_table.iter() {
            symbol_table.insert(label.clone(), *addr);
        }
    }

    let mut image =
        if overlay_on_os {
            let first_object = objects.get(0).ok_or(crate::analysis::SingleError::Link)?;
            let first_region = first_object.regions.get(0).ok_or(crate::analysis::SingleError::Link)?;

            let mut os = lc3_os::OS_IMAGE.clone().0;
            os[lc3_os::USER_PROG_START_ADDR as usize] = first_region.origin;

            os
        } else {
            [0; ADDR_SPACE_SIZE_IN_WORDS]
        };
    for object in objects {
        for region in object.regions {
            let linked_region = link_region(&symbol_table, region).map_err(|_| crate::analysis::SingleError::Link)?;
            layer_region(&mut image, linked_region);
        }
    }

    Ok(image.into())
}