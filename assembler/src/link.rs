use std::collections::HashMap;
use std::num::{ParseIntError, TryFromIntError};
use chumsky::chain::Chain;
use chumsky::Parser;
use lc3_isa::util::MemoryDump;
use lc3_isa::{Addr, ADDR_SPACE_SIZE_IN_WORDS, Word};
use crate::assemble::{assemble_instruction, AssemblyResult, Object, ObjectWord, Region, SymbolTable};
use crate::error::SingleError;

pub struct LinkedRegion {
    pub(crate) origin: Addr,
    pub(crate) words: Vec<Word>,
}

fn link_region(symbol_table: &SymbolTable, region: Region) -> Result<LinkedRegion, SingleError> {
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
                match assemble_instruction(&symbol_table, &location_counter, instruction).map_err(|_| SingleError::Link)? {
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

pub(crate) fn link_regions(symbol_table: &SymbolTable, regions: Vec<Region>) -> Result<Vec<LinkedRegion>, SingleError> {
    regions.into_iter()
        .map(|region| link_region(symbol_table, region))
        .collect()
}

pub fn link(objects: impl IntoIterator<Item=Object>) -> Result<Vec<LinkedRegion>, SingleError> {
    let objects = objects.into_iter().collect::<Vec<_>>();

    let mut global_symbol_table = HashMap::new();
    for object in objects.iter() {
        for (label, addr) in object.symbol_table.iter() {
            global_symbol_table.insert(label.clone(), *addr);
        }
    }

    let linked_regions =
        objects.into_iter()
            .map(|object| link_regions(&mut global_symbol_table, object.regions))
            .collect::<Result<Vec<Vec<LinkedRegion>>, SingleError>>()?
            .into_iter()
            .flatten()
            .collect();

    Ok(linked_regions)
}
