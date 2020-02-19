use lc3_isa::{Addr, SignedWord};
use crate::error::ParseError;
use crate::lexer::Token;
use crate::ir2_lines::Line;

pub struct File<'input> {
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<Line<'input>>,
}

pub struct Object<'input> {
    pub operations: Vec<Operation<'input>>,
    pub empty_lines: Vec<Line<'input>>,
    pub hanging_labels: Vec<Line<'input>>,
    pub invalid_lines: Vec<Line<'input>>,
}

pub type Label<'input> = Checked<'input, &'input str>;
pub type Separator<'input> = Token<'input>;

// Different from lc3_isa::Instruction in that offsets from labels aren't computed.
// Also covers pseudo-ops.
pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub operator: Token<'input>,
    pub operands: Operands<'input>,

    pub separators: Vec<Separator<'input>>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,
}

pub struct Checked<'input, T> {
    pub src: Token<'input>,
    pub value: Result<T, ParseError>,
}

pub type Reg<'input> = Checked<'input, lc3_isa::Reg>;
pub type Immediate<'input, T> = Checked<'input, T>;

pub enum Sr2OrImm5<'input> {
    Sr2(Reg<'input>),
    Imm5(Immediate<'input, SignedWord>),
}

pub struct ConditionCodes {
    pub n: bool,
    pub z: bool,
    pub p: bool,
}


pub enum Operands<'input> {
    Add { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Result<Sr2OrImm5<'input>, ParseError> },
    And { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Result<Sr2OrImm5<'input>, ParseError> },
    Br { nzp_src: Option<Token<'input>>, nzp: Result<ConditionCodes, ParseError>, label: Label<'input> },
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
    Trap { trap_vec: Immediate<'input, u8> },

    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,

    Orig { origin: Immediate<'input, Addr> },
    Fill { value: Immediate<'input, SignedWord> },
    Blkw { size_src: Token<'input>, size: Immediate<'input, Addr> }, // Addr used here to signify a number of locations. Max is number of possible Addrs.
    Stringz { string: Token<'input> },
    End,
}
