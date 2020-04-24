use std::collections::HashMap;
use lc3_isa::Addr;
use crate::ir::ir5_expand_pseudo_ops;
use crate::error::MemoryError;

pub type SymbolTable<'input> = HashMap<&'input str, Addr>;

pub fn build_symbol_table<'input>(object: &ir5_expand_pseudo_ops::Object<'input>) -> Result<HashMap<&'input str, Addr>, MemoryError> {
    let mut symbol_table = HashMap::new();
    let mut current_location = object.orig;
    for op_or_value in object.ops_or_values.iter() {
        if let Some(label) = op_or_value.0 {
            let other_location = symbol_table.insert(label.clone(), current_location);
            if let Some(_) = other_location {
                return Err(MemoryError("Duplicate label at different location.".to_string()))
            }
        }
        current_location += 1;
    };
    Ok(symbol_table)
}
