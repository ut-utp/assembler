use crate::ir::{ir5_expand_pseudo_ops, ir2_parse_line_syntax, ir4_parse_ambiguous_tokens};
use lc3_isa::{Addr, Word, Instruction};
use crate::lexer::Token;
use crate::analysis::symbol_table::{SymbolTable, SymbolTableError};
use crate::error::ParseError;

/// `complete` will store as much data as possible
/// relating to the source *and* what it will be assembled to.
/// This will allow querying for the source assembled to a memory location,
/// the addresses corresponding to labels, and whatever is required in the future
/// to provide a nice development environment.

pub type Label<'input> = ir5_expand_pseudo_ops::Label<'input>;
pub type Immediate<'input, Addr> = ir5_expand_pseudo_ops::Immediate<'input, Addr>;

pub struct Program<'input> {
    pub objects: Vec<Object<'input>>
}

pub struct Object<'input> {
    pub origin_src: Operation<'input>,
    pub origin: Immediate<'input, Addr>,
    pub content: ObjectContent<'input>,
    pub symbol_table: Result<SymbolTable<'input>, SymbolTableError>
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

    pub instruction_or_values: Result<InstructionOrValues, Vec<ParseError>>,
}

pub enum InstructionOrValues {
    Instruction(Instruction, Word),
    Values(Vec<Word>),
}

impl<'input> Operation<'input> {

    pub fn num_memory_locations_occupied(&self) -> Result<usize, Vec<ParseError>> {
        use InstructionOrValues::*;
        match &self.instruction_or_values {
            Ok(Instruction(_, _)) => Ok(1),
            Ok(Values(values)) => Ok(values.len()),
            Err(errors) => Err(errors.clone())
        }
    }

}

pub type Operands<'input> = ir5_expand_pseudo_ops::Operands<'input>;
pub type ConditionCodes = ir5_expand_pseudo_ops::ConditionCodes;
pub type Separator<'input> = ir5_expand_pseudo_ops::Separator<'input>;

