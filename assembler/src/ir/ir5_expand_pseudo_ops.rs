use crate::ir::ir4_validate_ambiguous_tokens;
use lc3_isa::{Word, Addr};
use crate::ir::ir4_validate_ambiguous_tokens::Checked;
use std::iter::repeat;

pub type Label<'input> = &'input str;

pub struct Object<'input> {
    pub(crate) orig: Addr,
    pub(crate) ops_or_values: Vec<(Option<Label<'input>>, OpOrValue<'input>)>,
}

#[derive(Clone)]
pub enum OpOrValue<'input> {
    Operation(ir4_validate_ambiguous_tokens::Operation<'input>),
    Value(Word),
}

pub fn expand_pseudo_ops(object: ir4_validate_ambiguous_tokens::Object) -> Object {
    let ir4_validate_ambiguous_tokens::Object { origin, content, .. } = object;

    let orig = origin.unwrap();

    let mut ops_or_values = Vec::new();
    for operation in content.operations {
        let label = operation.label.clone().map(Checked::unwrap);
        let mut values = Vec::new();
        use ir4_validate_ambiguous_tokens::Operands;
        match operation.operands {
            Operands::Blkw { size, .. } => {
                let num_values = size.unwrap() as usize;
                let block = repeat((None, OpOrValue::Value(0))).take(num_values);
                values.extend(block);
            },
            Operands::Stringz { string } => {
                for c in string.unwrap().chars() {
                    values.push((None, OpOrValue::Value(c as Word)));
                }
                values.push((None, OpOrValue::Value(0))); // null-terminate
            },
            Operands::End => { /* ignore */ },
            _ => {
                values.push((None, OpOrValue::Operation(operation)));
            },
        };
        let first = values.get_mut(0);
        if let Some(first_value) = first { // TODO: how to handle other case?
            first_value.0 = label;
        }
        ops_or_values.extend(values);
    }

    Object { orig, ops_or_values }
}
