use crate::lexer::Lexer;
use crate::ir::ir1_parse_lines::parse_simple_lines;
use crate::ir::ir2_check_line_syntax::parse_lines;
use crate::ir::ir3_group_lines_and_objects::parse_unvalidated_file;
use crate::ir::ir4_validate_ambiguous_tokens::{File, CstParser};

pub fn parse(tokens: Lexer, leniency: LeniencyLevel) -> File {
    let ir1 = parse_simple_lines(tokens);
    let ir2 = parse_lines(ir1);
    let ir3 = parse_unvalidated_file(ir2);
    CstParser { leniency }.parse_cst(ir3)
}

// TODO: impl Default?
pub enum LeniencyLevel {
    Lenient,
    Strict,
}

impl LeniencyLevel {
    pub fn long_labels_allowed(&self) -> bool {
        match self {
            LeniencyLevel::Lenient => true,
            LeniencyLevel::Strict => false
        }
    }
}
