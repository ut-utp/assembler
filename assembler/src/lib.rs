
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

pub mod assembler;


#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use crate::parser::parse;
    use crate::parser::LeniencyLevel::Lenient;

    #[test]
    fn simple() {
        let src = ".ORIG x3000;\nLABEL ADD R0, R0, #0\n.end";
        let tokens = Lexer::new(src);
        let cst = parse(tokens, Lenient);
        println!("{:?}", cst);
    }

}
