//! Functions and data structures for linking [`Object`](crate::assemble::Object)s
//! produced by [initial assembly](crate::assemble::assemble).
//!
//! Linking is the process of assembling instructions in an object which
//! refer to labels in other objects. When writing an LC-3 program, this
//! allows referencing code which *other* programmers have assembled and
//! distributed as objects.
//!
//! This module assumes that all objects share a global namespace for labels.
//! **Linking two or more objects which each define the same label will result in undefined behavior.**

use std::collections::HashMap;
use lc3_isa::{Addr, Word};
use crate::assemble::{assemble_instruction, AssemblyResult, Object, ObjectWord, ObjectBlock, SymbolTable};
use crate::error::SingleError;

/// A block of LC-3 words and a target starting memory address.
///
/// `origin` is the intended address of the first word in `words` when loading the block.
pub struct Block {
    pub origin: Addr,
    pub words: Vec<Word>,
}

fn link_object_block(symbol_table: &SymbolTable, block: ObjectBlock) -> Result<Block, SingleError> {
    let mut words = Vec::new();
    let ObjectBlock { origin, words: object_words, .. } = block;
    let mut location_counter = origin;
    for object_word in object_words {
        match object_word {
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

pub(crate) fn link_object_blocks(symbol_table: &SymbolTable, blocks: Vec<ObjectBlock>) -> Result<Vec<Block>, SingleError> {
    blocks.into_iter()
        .map(|block| link_object_block(symbol_table, block))
        .collect()
}


/// Links a set of [`Object`](crate::assemble::Object)s to finish
/// assembling them into a set of loadable memory blocks.
///
/// See the [module-level documentation](crate::link) for details on the linking process.
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
            .map(|object| link_object_blocks(&mut global_symbol_table, object.blocks))
            .collect::<Result<Vec<Vec<Block>>, SingleError>>()?
            .into_iter()
            .flatten()
            .collect();

    Ok(blocks)
}
