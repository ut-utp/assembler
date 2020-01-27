use lc3_isa::{Addr, SignedWord};
use crate::error::ParseError;
use crate::lexer::Token;

pub struct File<'input> {
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<Token<'input>>,
}

impl<'input> File<'input> {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            ignored: Vec::new(),
        }
    }
}

pub struct Object<'input> {
    pub orig: Operation<'input>,
    pub instructions: Vec<Operation<'input>>,
    pub ignored: Vec<Token<'input>>,
}

impl<'input> Object<'input> {
    pub fn new(orig: Operation<'input>) -> Self {
        Self {
            orig,
            instructions: Vec::new(),
            ignored: Vec::new(),
        }
    }
}

type Label<'input> = Token<'input>;
type Separator<'input> = Token<'input>;

// Different from lc3_isa::Instruction in that offsets from labels aren't computed.
// Also covers pseudo-ops.
pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub opcode_src: Token<'input>,
    pub operands: Operands<'input>,
    pub separators_src: Vec<Separator<'input>>,
}

#[derive(Debug, PartialEq)]
pub struct Reg<'input> {
    pub src: Token<'input>,
    pub reg: Result<lc3_isa::Reg, ParseError>,
}

#[derive(Debug, PartialEq)]
pub enum Sr2OrImm5<'input> {
    Sr2(Reg<'input>),
    Imm5(Immediate<'input, SignedWord>),
}

#[derive(Debug, PartialEq)]
pub struct Immediate<'input, T> {
    pub src: Token<'input>,
    pub value: T,
}

#[derive(Debug, PartialEq)]
pub enum Operands<'input> {
    Add { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Sr2OrImm5<'input> },
    And { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Sr2OrImm5<'input> },
    Br { nzp_src: Token<'input>, n: bool, z: bool, p: bool, label: Label<'input> },
    Jmp { base: Reg<'input> },
    Jsr { label: Label<'input> },
    Jsrr { base: Reg<'input> },
    Ld { dr: Reg<'input>, label: Label<'input> },
    Ldi { dr: Reg<'input>, label: Label<'input> },
    Ldr { dr: Reg<'input>, base: Reg<'input>, offset6: Immediate<'input, SignedWord> },
    Lea { dr: Reg<'input>, label: Label<'input> },
    Not { dr: Reg<'input>, sr: Reg<'input> },
    Ret, 
    Rti, 
    St { sr: Reg<'input>, label: Label<'input> },
    Sti { sr: Reg<'input>, label: Label<'input> },
    Str { sr: Reg<'input>, base: Reg<'input>, offset6: Immediate<'input, SignedWord> },
    Trap { trapvec: u8 },
    
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,

    Orig { origin: Immediate<'input, Addr> },
    Fill { value: Immediate<'input, SignedWord> },
    Blkw { size_src: Token<'input>, size: Addr }, // Addr used here to signify a number of locations. Max is number of possible Addrs.
    Stringz { string: Token<'input> },
    End,
    
    Invalid,
}
