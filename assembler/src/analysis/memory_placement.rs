use itertools::Itertools;
use lc3_isa::Addr;

use crate::ir::ir5_expand_pseudo_ops;
use crate::error::MemoryError;

pub fn validate_placement(objects: &Vec<ir5_expand_pseudo_ops::Object>) -> Result<(), MemoryError> {
    let starts_and_ends = objects.iter().map(get_start_and_end);
    for ((_, prev_end), (next_start, _)) in starts_and_ends.tuple_windows() {
        if prev_end > next_start {
            return Err(MemoryError("Objects overlap.".to_string()));
        }
    }
    Ok(())
}

fn get_start_and_end(object: &ir5_expand_pseudo_ops::Object) -> (Addr, Addr) {
    let start = object.orig;
    let end = start + object.ops_or_values.len() as Addr;
    (start, end)
}
