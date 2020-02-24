use lc3_isa::{Addr, SignedWord, check_signed_imm};
use crate::error::ParseError;
use crate::lexer::Token;
use crate::ir2_lines::{Line, OperationTokens, OperandTokens};
use crate::ir3_unvalidated_objects::{UnvalidatedFile, UnvalidatedObject, UnvalidatedLine, UnvalidatedObjectContent};
use std::convert::TryInto;
use num_traits::Num;
use std::string::ToString;

#[derive(Clone)]
pub struct File<'input> {
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<Line<'input>>,
}

#[derive(Clone)]
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
#[derive(Clone)]
pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub operator: Token<'input>,
    pub operands: Operands<'input>,

    pub separators: Vec<Separator<'input>>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,
}

#[derive(Clone)]
pub struct Checked<'input, T> {
    pub src: Token<'input>,
    pub value: Result<T, ParseError>,
}

pub type Reg<'input> = Checked<'input, lc3_isa::Reg>;
pub type Immediate<'input, T> = Checked<'input, T>;

#[derive(Clone)]
pub enum Sr2OrImm5<'input> {
    Sr2(Reg<'input>),
    Imm5(Immediate<'input, SignedWord>),
}

#[derive(Clone)]
pub struct ConditionCodes {
    pub n: bool,
    pub z: bool,
    pub p: bool,
}

#[derive(Clone)]
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

pub fn parse_cst<'a, 'input>(file: &'a UnvalidatedFile<'input>) -> File<'input> {
    let UnvalidatedFile { objects: mut old_objects, ignored } = file.clone();
    let objects = old_objects.drain(..).map(validate_object).collect();
    File { objects, ignored }
}

fn validate_object(object: UnvalidatedObject) -> Object {
    let UnvalidatedObjectContent { operations: mut old_operations, empty_lines, hanging_labels, invalid_lines } = object.content.clone();
    let operations = old_operations.drain(..).map(validate_line).collect();
    Object { operations, empty_lines, hanging_labels, invalid_lines }
}

fn validate_line(line: UnvalidatedLine) -> Operation {
    let UnvalidatedLine {
        label,
        operation: OperationTokens {
            operator,
            operands,
            separators,
        },
        whitespace,
        comments,
        newlines
    } = line.clone();

    Operation {
        label: label.map(validate_label),
        operator,
        operands: validate_operand_tokens(operands),
        separators,
        whitespace,
        comments,
        newlines
    }
}

fn validate_operand_tokens(operands: OperandTokens) -> Operands {
    match operands {
        OperandTokens::Add { dr, sr1, sr2_or_imm5 } =>
            Operands::Add {
                dr: validate_reg(dr),
                sr1: validate_reg(sr1),
                sr2_or_imm5: validate_sr2_or_imm5(sr2_or_imm5)
            },
        OperandTokens::And { dr, sr1, sr2_or_imm5 } =>
            Operands::And {
                dr: validate_reg(dr),
                sr1: validate_reg(sr1),
                sr2_or_imm5: validate_sr2_or_imm5(sr2_or_imm5)
            },
        OperandTokens::Br { nzp, label } => {
            let (nzp_src, nzp) = validate_condition_codes(nzp);
            Operands::Br {
                nzp_src,
                nzp,
                label: validate_label(label),
            }
        },
        OperandTokens::Jmp { base } => Operands::Jmp { base: validate_reg(base) },
        OperandTokens::Jsr { label } => Operands::Jsr { label: validate_label(label) },
        OperandTokens::Jsrr { base } => Operands::Jsrr { base: validate_reg(base) },
        OperandTokens::Ld { dr, label } => Operands::Ld { dr: validate_reg(dr), label: validate_label(label) },
        OperandTokens::Ldi { dr, label } => Operands::Ldi { dr: validate_reg(dr), label: validate_label(label) },
        OperandTokens::Ldr { dr, base, offset6 } =>
            Operands::Ldr {
                dr: validate_reg(dr),
                base: validate_reg(base),
                offset6: validate_signed_immediate(offset6, 6),
            },
        OperandTokens::Lea { dr, label } => Operands::Lea { dr: validate_reg(dr), label: validate_label(label) },
        OperandTokens::Not { dr, sr } => Operands::Not { dr: validate_reg(dr), sr: validate_reg(sr) },
        OperandTokens::Ret => Operands::Ret,
        OperandTokens::Rti => Operands::Rti,
        OperandTokens::St { sr, label } => Operands::St { sr: validate_reg(sr), label: validate_label(label) },
        OperandTokens::Sti { sr, label } => Operands::Sti { sr: validate_reg(sr), label: validate_label(label) },
        OperandTokens::Str { sr, base, offset6 } =>
            Operands::Str {
                sr: validate_reg(sr),
                base: validate_reg(base),
                offset6: validate_signed_immediate(offset6, 6),
            },
        OperandTokens::Trap { trap_vec } => Operands::Trap { trap_vec: validate_numeric_immediate(trap_vec) },

        OperandTokens::Getc => Operands::Getc,
        OperandTokens::Out => Operands::Out,
        OperandTokens::Puts => Operands::Puts,
        OperandTokens::In => Operands::In,
        OperandTokens::Putsp => Operands::Putsp,
        OperandTokens::Halt => Operands::Halt,

        OperandTokens::Orig { origin } => Operands::Orig { origin: validate_numeric_immediate(origin) },
        OperandTokens::Fill { value } => Operands::Fill { value: validate_numeric_immediate(value) },
        OperandTokens::Blkw { size } => Operands::Blkw { size_src: size, size: validate_blkw_immediate(size) },
        OperandTokens::Stringz { string } => Operands::Stringz { string }, // TODO: validate ASCII?
        OperandTokens::End => Operands::End,
    }
}

fn validate_sr2_or_imm5(src: Token) -> Result<Sr2OrImm5, ParseError> {
    let reg = validate_reg(src);
    let imm5 = validate_signed_immediate(src, 5);
    if let Reg { value: Ok(_), .. } = reg {
        Ok(Sr2OrImm5::Sr2(reg))
    } else if let Immediate { value: Ok(_), .. } = imm5 {
        Ok(Sr2OrImm5::Imm5(imm5))
    } else {
        Err(ParseError("Invalid as SR2 and as Imm5.".to_string()))
    }
}

fn validate_reg(src: Token) -> Reg {
    let value = if let Some("r") | Some("R") = src.src.get(..=0) {
        src.src.get(1..)
            .filter(|s| s.len() == 1)
            .and_then(|s| s.parse::<u8>().ok())
            .and_then(|i| i.try_into().ok())
            .ok_or(ParseError("Invalid register: didn't follow R with only 0-7".to_string()))
    } else {
        Err(ParseError("Invalid register: didn't start with R".to_string()))
    };
    Reg { src, value }
}

fn validate_numeric_immediate<T: Num>(src: Token) -> Immediate<T> {
    let value = src.src.get(..=0).and_then(|src_head| {
        let radix = match src_head {
            "b" => Some(2),
            "#" => Some(10),
            "x" => Some(16),
            _ => None
        };
        if let (Some(radix), Some(src_tail)) = (radix, src.src.get(1..)) {
            T::from_str_radix(src_tail, radix).ok()
        } else {
            None
        }
    }).ok_or(ParseError("Invalid numeric immediate.".to_string())); // TODO: make error message good.

    Immediate { src, value }
}

fn validate_signed_immediate(src: Token, num_bits: u32) -> Immediate<SignedWord> {
    let Immediate { src, value } = validate_numeric_immediate(src);
    let value = value.ok()
        .filter(|&i| check_signed_imm(i, num_bits))
        .ok_or(ParseError("Invalid signed word immediate".to_string()));
    Immediate { src, value }
}

fn validate_label(src: Token) -> Label {
    let label = src.src;

    let valid_length = (1..=20).contains(&label.len());

    let mut chars = label.chars();
    let first_char_alphabetic = chars.next().filter(|c| c.is_alphabetic()).is_some();
    let other_chars_alphanumeric = chars.all(char::is_alphanumeric);

    let valid = valid_length && first_char_alphabetic && other_chars_alphanumeric;
    let value = if valid {
        Ok(label)
    } else {
        Err(ParseError("Invalid label.".to_string()))
    };

    Label { src, value }
}

fn validate_condition_codes(src: Option<Token>) -> (Option<Token>, Result<ConditionCodes, ParseError>) {
    let value = if let Some(token) = src {
        validate_condition_codes_str(token.src)
    } else {
        Ok(ConditionCodes { n: true, z: true, p: true })
    };
    (src, value)
}

fn validate_condition_codes_str(src: &str) -> Result<ConditionCodes, ParseError> {
    let mut n = false;
    let mut z = false;
    let mut p = false;
    for c in src.chars() {
        match c {
            // TODO: prettify with macro or non-iterative solution
            'n' | 'N' => {
                if n { return Err(ParseError("Duplicate condition code n.".to_string())); }
                n = true;
            },
            'z' | 'Z' => {
                if z { return Err(ParseError("Duplicate condition code z.".to_string())); }
                z = true;
            },
            'p' | 'P' => {
                if p { return Err(ParseError("Duplicate condition code p.".to_string())); }
                p = true;
            },
            _ => { return Err(ParseError("Invalid condition codes.".to_string())) },
        }
    }
    Ok(ConditionCodes { n, z, p })
}

fn validate_blkw_immediate(src: Token) -> Immediate<Addr> {
    Immediate {
        src,
        value: src.src.parse().map_err(|_| ParseError("Invalid BLKW immediate.".to_string()))
    }
}
