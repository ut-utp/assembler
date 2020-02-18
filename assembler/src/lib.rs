
// TODO: docs
// TODO: denys
// TODO: docs URL

mod error;
mod lexer;
pub mod ir1_simple_lines;
pub mod ir2_lines;
pub mod ir3_unvalidated_objects;
pub mod cst;
mod expanded;

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use crate::ir1_simple_lines::parse_simple_lines;
    use crate::ir2_lines::parse_lines;
    use crate::ir3_unvalidated_objects::parse_unvalidated_file;
    use crate::cst::parse_cst;

    #[test]
    fn test_parse() {
        let string = ".ORIG x3000;\nADD R0, R0, R0\n";
        let mut lexer = Lexer::new(string);
        let ir1 = parse_simple_lines(&mut lexer);
        let ir2 = parse_lines(&ir1);
        let ir3 = parse_unvalidated_file(&ir2);
        let cst = parse_cst(&ir3);
    }

}
