use regex::Regex;
use crate::error::LexError;

// TODO: retain whitespace? Would make grammar unnecessarily complex, but may be necessary to fully recreate input

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'input> {
    pub src: &'input str,
    pub start: usize,
    pub end: usize,
    pub ty: TokenType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Insignificant Whitespace
    Whitespace,

    Op(Op),

    // String Literals
    // Numeric literals starting with x can't be disambiguated from labels,
    // so we'll do that later based on position.
    String,

    // Comments
    Comment,

    // Punctuation
    Comma,
    Newline,

    // Chunk of non-whitespace, non-comma, non-semicolon text.
    // Used as a catch-all for tokens that need to be disambiguated at parse-time,
    // for example, labels and hex literals which may both start with 'x'.
    // In more general terms: labels and operands.
    Word,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Opcode(Opcode),
    NamedTrap(NamedTrap),
    PseudoOp(PseudoOp),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    Add,
    And,
    Br,
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PseudoOp {
    Orig,
    Fill,
    Blkw,
    Stringz,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NamedTrap {
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,
}

pub struct Lexer<'input> {
    src: &'input str,
    patterns: Vec<(Regex, TokenType)>,
    cur_pos: usize,
}

use TokenType::*;
use Opcode::*;
use NamedTrap::*;
use PseudoOp::*;

impl<'input> Lexer<'input> {

    // The lexer tries to find these patterns in this order.
    // Registering a pattern will automatically append some stuff to the regex.
    // Notably, it will add ^ to the beginning to ensure that it grabs tokens
    // from the beginning of the slice it's examining, so don't use ^.
    const PATTERNS: [(&'static str, TokenType); 34] = [
        (r"[^\S\r\n]+", Whitespace),

        (r"ADD",  Op(Op::Opcode(Add))),
        (r"AND",  Op(Op::Opcode(And))),
        (r"BR",   Op(Op::Opcode(Br))),
        (r"JMP",  Op(Op::Opcode(Jmp))),
        (r"JSRR", Op(Op::Opcode(Jsrr))),
        (r"JSR",  Op(Op::Opcode(Jsr))),
        (r"LDI",  Op(Op::Opcode(Ldi))),
        (r"LDR",  Op(Op::Opcode(Ldr))),
        (r"LD",   Op(Op::Opcode(Ld))),
        (r"LEA",  Op(Op::Opcode(Lea))),
        (r"NOT",  Op(Op::Opcode(Not))),
        (r"RET",  Op(Op::Opcode(Ret))),
        (r"RTI",  Op(Op::Opcode(Rti))),
        (r"STI",  Op(Op::Opcode(Sti))),
        (r"STR",  Op(Op::Opcode(Str))),
        (r"ST",   Op(Op::Opcode(St))),
        (r"TRAP", Op(Op::Opcode(Trap))),

        (r"GETC",  Op(Op::NamedTrap(Getc))),
        (r"OUT",   Op(Op::NamedTrap(Out))),
        (r"PUTS",  Op(Op::NamedTrap(Puts))),
        (r"IN",    Op(Op::NamedTrap(In))),
        (r"PUTSP", Op(Op::NamedTrap(Putsp))),
        (r"HALT",  Op(Op::NamedTrap(Halt))),

        (r".ORIG",    Op(Op::PseudoOp(Orig))),
        (r".FILL",    Op(Op::PseudoOp(Fill))),
        (r".BLKW",    Op(Op::PseudoOp(Blkw))),
        (r".STRINGZ", Op(Op::PseudoOp(Stringz))),
        (r".END",     Op(Op::PseudoOp(End))),

        (r#""([^"\\]|\\.)*""#, String),// quotes with any number of non-quote/backslash chars *or* arbitrary chars escaped with backslashes in between.

        (r";.*", Comment), // semicolon followed by any number of chars that aren't newlines.

        (r",",            Comma),
        (r"(\r\n|\r|\n)", Newline),

        (r"[^\s,;]+", Word), // At least one non-whitespace, non-comma, non-semicolon character.
    ];

    pub fn new(src: &'input str) -> Lexer<'input> {
        let mut this = Lexer {
            src,
            patterns: Vec::new(),
            cur_pos: 0,
        };

        for (pattern, token_type) in Self::PATTERNS.iter() {
            this.register_pattern(pattern, *token_type);
        }

        this
    }

    fn register_pattern(&mut self, pattern: &str, token_type: TokenType)
    {
        assert!(!pattern.starts_with("^"));
        let pattern = format!("^(?i){}", pattern);
        let regex = Regex::new(pattern.as_str()).expect("Invalid regex");
        self.patterns.push((regex, token_type))
    }

    fn tail(&self) -> &'input str {
        &self.src[self.cur_pos..]
    }

}


impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cur_pos;
        if self.src.len() <= start {
            return None;
        }

        for (pattern, token_type) in &self.patterns {
            if let Some(found) = pattern.find(self.tail()) {
                self.cur_pos += found.end();
                let token = Token {
                    src: found.as_str(),
                    start,
                    end: self.cur_pos,
                    ty: *token_type,
                };
                return Some(token);
            }
        }

        unreachable!("The lexer could not recognize some character pattern you provided. Please contact the maintainers."); // TODO: handle gracefully?
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_simple() { // TODO: actually assert some stuff
        let input = ".ORIG x3000\nTEST add R0, R0, R0; Tokenize me, cap'n!\nBRnzp TEST\nHALT\n.END";
        let lexer = Lexer::new(input);
        for item in lexer {
            println!("{:?}", item);
        }
    }
}
