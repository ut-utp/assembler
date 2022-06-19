use lc3_isa::util::MemoryDump;
use lc3_isa::{ADDR_SPACE_SIZE_IN_WORDS, Word};
use crate::error::SingleError;
use crate::link::LinkedRegion;

fn layer_region(image: &mut [Word; ADDR_SPACE_SIZE_IN_WORDS], region: LinkedRegion) {
    let LinkedRegion { origin, words } = region;
    let mut addr = origin as usize;
    for word in words {
        image[addr] = word;
        addr += 1;
    }
}

pub fn layer(regions: impl IntoIterator<Item=LinkedRegion>, layer_onto_os: bool) -> Result<MemoryDump, SingleError> {
    let regions = regions.into_iter().collect::<Vec<_>>();

    let mut image =
        if layer_onto_os {
            let first_region = regions.get(0).ok_or(SingleError::Layer)?;

            let mut os = lc3_os::OS_IMAGE.clone().0;
            os[lc3_os::USER_PROG_START_ADDR as usize] = first_region.origin;

            os
        } else {
            [0; ADDR_SPACE_SIZE_IN_WORDS]
        };

    for region in regions {
        layer_region(&mut image, region);
    }

    Ok(image.into())
}
