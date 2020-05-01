use itertools::Itertools;
use lc3_isa::Addr;

use crate::ir::ir5_expand_pseudo_ops;
use crate::error::ParseError;
use crate::ir::ir4_parse_ambiguous_tokens::Checked;
use annotate_snippets::snippet::SourceAnnotation;

#[derive(Debug, Clone)]
pub enum MemoryPlacementError {
    InvalidOrigin {
        parse_error: ParseError,
    },
    UnknownPseudoOpLength {
        parse_error: ParseError,
    },
    ObjectsOverlap
}

impl MemoryPlacementError {

    pub fn message(&self) -> String {
        use MemoryPlacementError::*;
        match self {
            InvalidOrigin { .. } => "could not validate memory placement due to error parsing .ORIG",
            UnknownPseudoOpLength { .. } => "could not validate memory placement due to error parsing pseudo-op",
            ObjectsOverlap => "two objects (.ORIG/.END blocks) would occupy same memory locations",
        }.to_string()
    }

    pub fn annotations(&self) -> Vec<SourceAnnotation> {
        vec![]
    }

    pub fn should_show(&self) -> bool {
        use MemoryPlacementError::*;

        match self {
            InvalidOrigin { .. }
            | UnknownPseudoOpLength { .. } => false,
            ObjectsOverlap => true,
        }
    }

}


pub fn validate_placement(objects: &Vec<ir5_expand_pseudo_ops::Object>) -> Vec<MemoryPlacementError> {
    let starts_and_ends = objects.iter()
        .map(get_start_and_end)
        .collect::<Vec<_>>();
    let mut errors = Vec::new();
    for start_and_end in &starts_and_ends {
        if let Err(error) = start_and_end {
            errors.push(error.clone());
        }
    }
    if !errors.is_empty() {
        return errors;
    }
    let start_end_pairs = starts_and_ends.into_iter()
        .map(|start_and_end| start_and_end.unwrap())
        .sorted_by_key(|(start, end)| *start)
        .tuple_windows();
    for ((_, prev_end), (next_start, _)) in start_end_pairs {
        if prev_end > next_start {
            errors.push(MemoryPlacementError::ObjectsOverlap);
        }
    }
    errors
}

/// Returns the first memory location the object occupies and the first memory location after the object.
/// The object occupies all locations between the 'start' inclusive and 'end' exclusive.
fn get_start_and_end(object: &ir5_expand_pseudo_ops::Object) -> Result<(Addr, Addr), MemoryPlacementError> {
    match &object.origin.value {
        Err(error) => {
            Err(MemoryPlacementError::InvalidOrigin {
                parse_error: error.clone()
            })
        },
        Ok(origin) => {
            let start = *origin;
            let mut end = start;
            for operation in &object.content.operations {
                match operation.num_memory_locations_occupied() {
                    Ok(num_locations) => {
                        end += num_locations as Addr;
                    },
                    Err(error) => {
                        return Err(MemoryPlacementError::UnknownPseudoOpLength {
                            parse_error: error.clone()
                        });
                    }
                }
            }
            Ok((start, end))
        },
    }
}
