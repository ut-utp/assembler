use std::collections::HashMap;
use lc3_isa::{Addr, Word};
use crate::assemble::{assemble_instruction, AssemblyResult, Object, ObjectWord, Region, SymbolTable};
use crate::error::SingleError;

pub struct Block {
    pub(crate) origin: Addr,
    pub(crate) words: Vec<Word>,
}

fn link_region(symbol_table: &SymbolTable, region: Region) -> Result<Block, SingleError> {
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
                        ObjectWord::UnlinkedInstruction(_) => { return Err(SingleError::Link); }
                    }
                    AssemblyResult::MultipleObjectWords(ows) => {
                        let ws = ows.into_iter()
                            .map(|ow| match ow {
                                ObjectWord::Value(word) => Ok(word),
                                ObjectWord::UnlinkedInstruction(_) => Err(SingleError::Link),
                            })
                            .collect::<Result<Vec<_>, SingleError>>()?;
                        location_counter += ws.len() as u16;
                        words.extend(ws);
                    }
                }
        }
    }
    Ok(Block { origin, words })
}

pub(crate) fn link_regions(symbol_table: &SymbolTable, regions: Vec<Region>) -> Result<Vec<Block>, SingleError> {
    regions.into_iter()
        .map(|region| link_region(symbol_table, region))
        .collect()
}

pub fn link(objects: impl IntoIterator<Item=Object>) -> Result<Vec<Block>, SingleError> {
    let objects = objects.into_iter().collect::<Vec<_>>();

    let mut global_symbol_table = HashMap::new();
    for object in objects.iter() {
        for (label, addr) in object.symbol_table.iter() {
            global_symbol_table.insert(label.clone(), *addr);
        }
    }

    let blocks =
        objects.into_iter()
            .map(|object| link_regions(&mut global_symbol_table, object.regions))
            .collect::<Result<Vec<Vec<Block>>, SingleError>>()?
            .into_iter()
            .flatten()
            .collect();

    Ok(blocks)
}
