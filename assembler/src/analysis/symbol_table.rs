use std::collections::HashMap;
use lc3_isa::Addr;
use crate::ir::ir5_expand_pseudo_ops;
use crate::lexer::Span;
use crate::error::ParseError;

pub type SymbolTable<'input> = HashMap<&'input str, Addr>;

#[derive(Debug)]
pub enum SymbolTableError {
    InvalidOrigin {
        parse_error: ParseError,
    },
    UnknownPseudoOpLength {
        parse_error: ParseError,
    },
    DuplicateLabel {
        ranges: (Span, Span),
        label_text: String
    }
}

pub fn build_symbol_table<'input>(object: &ir5_expand_pseudo_ops::Object<'input>) -> Result<HashMap<&'input str, Addr>, Vec<SymbolTableError>> {
    let mut symbol_table = HashMap::new();
    let mut errors = Vec::new();
    match &object.origin.value {
        Err(parse_error) => {
            errors.push(SymbolTableError::InvalidOrigin { parse_error: parse_error.clone() });
        },
        Ok(origin) => {
            let mut current_location = *origin;
            for operation in object.content.operations.iter() {
                if let Some(label) = &operation.label {
                    let span = label.src.span;
                    if let Ok(label_text) = label.value {
                        let other_value = symbol_table.insert(label_text, (current_location, span));
                        if let Some((other_location, other_span)) = other_value {
                            errors.push(SymbolTableError::DuplicateLabel { // TODO: handle triplicate+ labels in one error
                                ranges: (other_span, span),
                                label_text: label_text.to_string()
                            });
                        }
                    }
                }
                match operation.num_memory_locations_occupied() {
                    Ok(num_locations) => {
                        current_location += num_locations as Addr;
                    },
                    Err(error) => {
                        errors.push(SymbolTableError::UnknownPseudoOpLength {
                            parse_error: error.clone()
                        });
                    }
                };
            };
        }
    };
    if errors.is_empty() {
        let symbol_table = symbol_table.iter()
            .map(|(label, (addr, span))| (*label, *addr))
            .collect();
        Ok(symbol_table)
    } else {
        Err(errors)
    }
}
