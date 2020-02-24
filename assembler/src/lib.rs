
// TODO: docs
// TODO: denys
// TODO: docs URL

pub mod error;
pub mod lexer;
pub mod parser;
pub mod ir1_simple_lines;
pub mod ir2_lines;
pub mod ir3_unvalidated_objects;
pub mod cst;
pub mod expanded;

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use crate::parser::parse;

    #[test]
    fn test_parse() {
        let src = ".ORIG x3000;\nADD R0, R0, 0\n";
        let tokens = Lexer::new(src);
        let cst = parse(tokens);
    }

}
