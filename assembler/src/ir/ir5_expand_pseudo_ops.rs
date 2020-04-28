use lc3_isa::{Word, Addr};
use std::iter::repeat;

use crate::ir::{ir4_parse_ambiguous_tokens, ir2_parse_line_syntax};
use crate::ir::ir4_parse_ambiguous_tokens::Checked;
use crate::lexer::Token;
use crate::error::ParseError;

pub type Label<'input> = ir4_parse_ambiguous_tokens::Label<'input>;
pub type Immediate<'input, Addr> = Checked<'input, Addr>;

pub struct File<'input> {
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<ir2_parse_line_syntax::Line<'input>>,
}

pub struct Object<'input> {
    pub origin_src: Operation<'input>,
    pub origin: Immediate<'input, Addr>,
    pub content: ObjectContent<'input>,
}

pub struct ObjectContent<'input> {
    pub operations: Vec<Operation<'input>>,
    pub empty_lines: Vec<ir2_parse_line_syntax::Line<'input>>,
    pub hanging_labels: Vec<ir2_parse_line_syntax::Line<'input>>,
    pub invalid_lines: Vec<ir2_parse_line_syntax::Line<'input>>,
}

pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub operator: Token<'input>,
    pub nzp: Result<Option<ConditionCodes>, ParseError>,
    pub operands: Operands<'input>,

    pub src_lines: Vec<String>,
    pub separators: Vec<Separator<'input>>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,

    // Option::Some only if this Operation is a pseudo-op.
    // Result::Ok only if the operands for the pseudo-op are Ok.
    pub expanded: Option<Result<Vec<Word>, ParseError>>,
}

impl<'input> Operation<'input> {

    pub fn num_memory_locations_occupied(&self) -> Result<usize, ParseError> {
        match &self.expanded {
            None => Ok(1),
            Some(Ok(values)) => Ok(values.len()),
            Some(Err(error)) => Err(error.clone())
        }
    }

}

pub type Operands<'input> = ir4_parse_ambiguous_tokens::Operands<'input>;
pub type ConditionCodes = ir4_parse_ambiguous_tokens::ConditionCodes;
pub type Separator<'input> = ir4_parse_ambiguous_tokens::Separator<'input>;

pub fn expand_pseudo_ops(file: ir4_parse_ambiguous_tokens::File) -> File {
    let ir4_parse_ambiguous_tokens::File { objects, ignored } = file;
    let objects = objects.into_iter()
        .map(expand_object_pseudo_ops)
        .collect();
    File { objects, ignored }
}

pub fn expand_object_pseudo_ops(object: ir4_parse_ambiguous_tokens::Object) -> Object {
    let ir4_parse_ambiguous_tokens::Object { origin_src, origin, content, } = object;
    let origin_src = expand_operation(origin_src);
    let content = expand_object_content(content);
    Object { origin_src, origin, content }
}

fn expand_object_content(object_content: ir4_parse_ambiguous_tokens::ObjectContent) -> ObjectContent {
    let ir4_parse_ambiguous_tokens::ObjectContent { operations, empty_lines, hanging_labels, invalid_lines, } = object_content;
    let operations = operations.into_iter()
        .map(expand_operation)
        .collect();
    ObjectContent { operations, empty_lines, hanging_labels, invalid_lines, }
}

// TODO: make symbol table calculate addresses without this IR,
// so we can use it here to calculate .FILLs with a label operand.
fn expand_operation(operation: ir4_parse_ambiguous_tokens::Operation) -> Operation {
    use ir4_parse_ambiguous_tokens::Operands;

    let ir4_parse_ambiguous_tokens::Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines } = operation;

    let expanded = match &operands {
        Operands::Blkw { size, .. } => {
            match &size.value {
                Ok(size) => {
                    let num_values = *size as usize;
                    let block = repeat(0).take(num_values).collect();
                    Some(Ok(block))
                },
                Err(err) => {
                    Some(Err(err.clone()))
                }
            }
        },
        Operands::Stringz { string } => {
            match &string.value {
                Ok(string) => {
                    let mut values = Vec::new();
                    for c in string.chars() {
                        values.push(c as Word);
                    }
                    values.push(0); // null-terminate
                    Some(Ok(values))
                },
                Err(err) => {
                    Some(Err(err.clone()))
                }
            }
        },
        Operands::Orig { .. } | Operands::End => { Some(Ok(vec![])) }, // Expand, but to no values
        _ => { None }, // Normal operation, does not expand
    };

    Operation { label, operator, nzp, operands, src_lines, separators, whitespace, comments, newlines, expanded, }
}
