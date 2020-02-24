use crate::cst::{File, parse_cst};
use crate::lexer::Lexer;
use crate::ir1_simple_lines::parse_simple_lines;
use crate::ir2_lines::parse_lines;
use crate::ir3_unvalidated_objects::parse_unvalidated_file;

pub fn parse(tokens: Lexer) -> File {
    let ir1 = parse_simple_lines(tokens);
    let ir2 = parse_lines(ir1);
    let ir3 = parse_unvalidated_file(ir2);
    parse_cst(ir3)
}