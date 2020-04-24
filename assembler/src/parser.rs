use crate::lexer::Lexer;
use crate::ir::ir1_parse_lines::parse_lines;
use crate::ir::ir2_parse_line_syntax::parse_line_syntax;
use crate::ir::ir3_parse_objects::parse_objects;
use crate::ir::ir4_parse_ambiguous_tokens;
use crate::ir::ir4_parse_ambiguous_tokens::AmbiguousTokenParser;

pub fn parse(tokens: Lexer, leniency: LeniencyLevel) -> ir4_parse_ambiguous_tokens::File {
    let ir1 = parse_lines(tokens);
    let ir2 = parse_line_syntax(ir1);
    let ir3 = parse_objects(ir2);
    AmbiguousTokenParser { leniency }.parse_ambiguous_tokens(ir3)
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
