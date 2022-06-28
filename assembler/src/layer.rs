use lc3_isa::util::MemoryDump;
use lc3_isa::{ADDR_SPACE_SIZE_IN_WORDS, Word};
use crate::error::SingleError;
use crate::link::Block;

fn layer_block(image: &mut [Word; ADDR_SPACE_SIZE_IN_WORDS], block: Block) {
    let Block { origin, words } = block;
    let mut addr = origin as usize;
    for word in words {
        image[addr] = word;
        addr += 1;
    }
}

pub fn layer(blocks: impl IntoIterator<Item=Block>, layer_onto_os: bool) -> Result<MemoryDump, SingleError> {
    let blocks = blocks.into_iter().collect::<Vec<_>>();

    let mut image =
        if layer_onto_os {
            let first_block = blocks.get(0).ok_or(SingleError::Layer)?;

            let mut os = lc3_os::OS_IMAGE.clone().0;
            os[lc3_os::USER_PROG_START_ADDR as usize] = first_block.origin;

            os
        } else {
            [0; ADDR_SPACE_SIZE_IN_WORDS]
        };

    for block in blocks {
        layer_block(&mut image, block);
    }

    Ok(image.into())
}
