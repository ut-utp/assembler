//! Functions for combining blocks of LC-3 memory into an executable image.

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

/// Combine the given blocks of memory into an executable image by placing them in memory in the given order.
///
/// Creating the memory image starts with all memory initialized to `0x0000`,
/// or if `layer_onto_os` is `true`, memory is initialized with the
/// [UTP LC-3 OS](https://github.com/ut-utp/core/tree/master/os). The
/// OS must be included for the UTP emulator to successfully load and execute the program.
///
/// After the image is initialized, each given [`Block`](crate::link::Block) is
/// inserted into memory at its target address, in the given order.
/// No regard is given to addresses that have already been initialized;
/// each block will be layered on top of the image and overwrite its target addresses.
/// If two words in different blocks occupy the same memory location,
/// that location will contain the the second block's word in the end.
pub fn layer(blocks: impl IntoIterator<Item=Block>, layer_onto_os: bool) -> Result<MemoryDump, SingleError> {
    let blocks = blocks.into_iter().collect::<Vec<_>>();

    let mut image =
        if layer_onto_os {
            let first_block = blocks.get(0).ok_or(SingleError::Layer)?;

            let mut os = lc3_os::OS_IMAGE.clone().0;
            os[lc3_os::USER_PROG_START_ADDR_SETTING_ADDR as usize] = first_block.origin;

            os
        } else {
            [0; ADDR_SPACE_SIZE_IN_WORDS]
        };

    for block in blocks {
        layer_block(&mut image, block);
    }

    Ok(image.into())
}
