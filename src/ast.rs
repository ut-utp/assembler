use lc3_isa::{Addr, SignedWord};
use crate::error::ParseError;

pub struct Object<'input> {
    orig: Operation<'input>,
    instructions: Vec<Operation<'input>>
}

type Label<'input> = &'input str;
type Separator<'input> = &'input str;

// Different from lc3_isa::Instruction in that offsets from labels aren't computed.
// Also covers pseudo-ops.
pub struct Operation<'input> {
    opcode_src: &'input str,
    operands: Operands<'input>,
    separators_src: Vec<Option<&'input str>>,
}

pub struct Reg<'input> {
    pub src: &'input str,
    pub reg: Result<lc3_isa::Reg, ParseError>,
}

pub enum Sr2OrImm5<'input> {
    Sr2(Reg<'input>),
    Imm5(Immediate<'input, SignedWord>),
}

pub struct Immediate<'input, T> {
    pub src: &'input str,
    pub value: Result<T, ParseError>,
}

pub enum Operands<'input> {
    Add { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Sr2OrImm5<'input> },
    And { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Sr2OrImm5<'input> },
    Br { nzp_src: &'input str, n: bool, z: bool, p: bool, label: Label<'input> },
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
    Blkw { size_src: &'input str, size: Addr }, // Addr used here to signify a number of locations. Max is number of possible Addrs.
    Stringz { string: &'input str },
    End,
}
