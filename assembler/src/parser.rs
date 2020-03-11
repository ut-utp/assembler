use crate::cst::{File, CstParser};
use crate::lexer::Lexer;
use crate::ir1_simple_lines::parse_simple_lines;
use crate::ir2_lines::parse_lines;
use crate::ir3_unvalidated_objects::parse_unvalidated_file;

pub fn parse(tokens: Lexer, leniency: LeniencyLevel) -> File {
    let ir1 = parse_simple_lines(tokens);
    let ir2 = parse_lines(ir1);
    let ir3 = parse_unvalidated_file(ir2);
    CstParser { leniency }.parse_cst(ir3)
}

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