use crate::lexer::Lexer;
use crate::ir::ir1_parse_lines::parse_lines;
use crate::ir::ir2_parse_line_syntax::parse_line_syntax;
use crate::ir::ir3_parse_objects::parse_objects;
use crate::ir::ir4_parse_ambiguous_tokens;
use crate::ir::ir4_parse_ambiguous_tokens::AmbiguousTokenParser;
use crate::complete;
use crate::ir::ir5_expand_pseudo_ops::expand_pseudo_ops;
use crate::complete::construct_all_instructions;

pub fn parse(tokens: Lexer, leniency: LeniencyLevel) -> complete::Program {
    let ir1 = parse_lines(tokens);
    let ir2 = parse_line_syntax(ir1);
    let ir3 = parse_objects(ir2);
    let ir4 = AmbiguousTokenParser { leniency }.parse_ambiguous_tokens(ir3);
    let ir5 = expand_pseudo_ops(ir4);
    construct_all_instructions(ir5)
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
