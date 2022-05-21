
// TODO: docs
// TODO: denys
// TODO: docs URL

extern crate core;

pub mod lexer;
pub mod parser;
pub mod assembler;
pub mod linker;
pub mod analysis;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Copy, Clone)]
pub enum LeniencyLevel {
    Lenient,
    Strict
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let src = ".ORIG x3000;\nLABEL ADD R0, R0, #7000\n.end";
        let (tokens, lex_errs) = lexer::lex(src, LeniencyLevel::Lenient);
        println!("{:?}", tokens);
        println!("{:?}", lex_errs);

        let parse_results = tokens.map(|ts| parser::parse(src, ts, LeniencyLevel::Strict));
        if let Some((program, parse_errs)) = parse_results {
            println!("{:?}", program);
            println!("{:?}", parse_errs);
        }
    }
}
