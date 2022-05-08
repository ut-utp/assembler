mod lexer;
mod parser;

use chumsky::prelude::*;
use chumsky::Stream;
use itertools::Itertools;
use lc3_isa::{Reg, SignedWord, Word};

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum Token {
    Opcode(Opcode),
    Register(Reg),
    NumberLiteral(LiteralValue),
    StringLiteral(String),
    Label(String),

    Newline,
    Comma,

    Comment,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum LiteralValue {
    Word(Word),
    SignedWord(SignedWord),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ConditionCodes {
    n: bool,
    z: bool,
    p: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Opcode {
    Add,
    And,
    Br(ConditionCodes),
    Jmp,
    Jsr,
    Jsrr,
    Ld,
    Ldi,
    Ldr,
    Lea,
    Not,
    Ret,
    Rti,
    St,
    Sti,
    Str,
    Trap,

    // Pseudo-ops
    Orig,
    Fill,
    Blkw,
    Stringz,
    End,

    // Named TRAP routines
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let src = ".ORIG x3000;\nLABEL ADD R0, R0, #7000\n.end";
        let (tokens, lex_errs) = lexer::lex(src);
        println!("{:?}", tokens);
        println!("{:?}", lex_errs);

        let parse_results = tokens.map(|ts| parser::parse(src, ts));
        if let Some((program, parse_errs)) = parse_results {
            println!("{:?}", program);
            println!("{:?}", parse_errs);
        }
    }
}