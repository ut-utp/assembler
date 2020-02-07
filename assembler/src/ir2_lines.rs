use crate::lexer::Token;

pub type Lines<'input> = Vec<Line<'input>>;

pub struct Line<'input> {
    pub content: LineContent<'input>,
    pub whitespace: Vec<Token<'input>>, // Only includes whitespace around operation
    pub comment: Option<Token<'input>>,
    pub newline: Option<Token<'input>>,
}

pub type Label<'input> = Token<'input>;

pub enum LineContent<'input> {
    Valid(Option<Label<'input>>, Option<OperationTokens<'input>>),
    Invalid(Vec<Token<'input>>)
}

pub struct OperationTokens<'input> {
    pub operator: Token<'input>,
    pub operands: OperandTokens<'input>,
    pub separators: Vec<Token<'input>>, // To include internal whitespace, but not surrounding
}

pub enum OperandTokens<'input> {
    Add { dr: Token<'input>, sr1: Token<'input>, sr2_or_imm5: Token<'input> },
    And { dr: Token<'input>, sr1: Token<'input>, sr2_or_imm5: Token<'input> },
    Br { nzp: Token<'input>, label: Label<'input> },
    Jmp { base: Token<'input> },
    Jsr { label: Label<'input> },
    Jsrr { base: Token<'input> },
    Ld { dr: Token<'input>, label: Label<'input>, },
    Ldi { dr: Token<'input>, label: Label<'input>, },
    Ldr { dr: Token<'input>, base: Token<'input>, offset6: Token<'input> },
    Lea { dr: Token<'input>, label: Label<'input> },
    Not { dr: Token<'input>, sr: Token<'input> },
    Ret,
    Rti,
    St { sr: Token<'input>, label: Label<'input> },
    Sti { sr: Token<'input>, label: Label<'input> },
    Str { sr: Token<'input>, base: Token<'input>, offset6: Token<'input> },
    Trap { trap_vec: Token<'input> },

    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,

    Orig { origin: Token<'input> },
    Fill { value: Token<'input> },
    Blkw { size: Token<'input> },
    Stringz { string: Token<'input> },
    End,
}
