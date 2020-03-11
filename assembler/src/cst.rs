use lc3_isa::{Addr, SignedWord, check_signed_imm, Word};
use crate::error::{ParseError, InvalidLabelReason, InvalidRegReason, InvalidImmediateReason};
use crate::lexer::Token;
use crate::ir2_lines::{Line, OperationTokens, OperandTokens};
use crate::ir3_unvalidated_objects::{UnvalidatedFile, UnvalidatedObject, UnvalidatedLine, UnvalidatedObjectContent};
use std::convert::TryInto;
use num_traits::Num;
use std::string::ToString;
use crate::parser::LeniencyLevel;

#[derive(Clone, Debug)]
pub struct File<'input> {
    pub objects: Vec<Object<'input>>,
    pub ignored: Vec<Line<'input>>,
}

#[derive(Clone, Debug)]
pub struct Object<'input> {
    pub origin_src: Operation<'input>,
    pub origin: Immediate<'input, Addr>,
    pub content: ObjectContent<'input>,
}

#[derive(Clone, Debug)]
pub struct ObjectContent<'input> {
    pub operations: Vec<Operation<'input>>,
    pub empty_lines: Vec<Line<'input>>,
    pub hanging_labels: Vec<Line<'input>>,
    pub invalid_lines: Vec<Line<'input>>,
}

pub type Label<'input> = Checked<'input, &'input str>;
pub type Separator<'input> = Token<'input>;

// Different from lc3_isa::Instruction in that offsets from labels aren't computed.
// Also covers pseudo-ops.
#[derive(Clone, Debug)]
pub struct Operation<'input> {
    pub label: Option<Label<'input>>,
    pub operator: Token<'input>,
    pub nzp: Result<Option<ConditionCodes>, ParseError>,
    pub operands: Operands<'input>,

    pub separators: Vec<Separator<'input>>,
    pub whitespace: Vec<Token<'input>>,
    pub comments: Vec<Token<'input>>,
    pub newlines: Vec<Token<'input>>,
}

#[derive(Clone, Debug)]
pub struct Checked<'input, T> {
    pub src: Token<'input>,
    pub value: Result<T, ParseError>,
}

impl<'input, T> Checked<'input, T> {
    pub fn unwrap(self) -> T {
        self.value.unwrap()
    }
    
    pub fn extract_error_into(self, errors: &mut Vec<ParseError>) {
        if let Err(error) = self.value {
            errors.push(error);
        }
    }
}

pub type Reg<'input> = Checked<'input, lc3_isa::Reg>;
pub type Immediate<'input, T> = Checked<'input, T>;

#[derive(Clone, Debug)]
pub enum Sr2OrImm5<'input> {
    Sr2(Reg<'input>),
    Imm5(Immediate<'input, SignedWord>),
}

#[derive(Clone, Debug)]
pub enum ImmOrLabel<'input> {
    Imm(Immediate<'input, SignedWord>),
    Label(Label<'input>),
}

#[derive(Clone, Debug)]
pub struct ConditionCodes {
    pub n: bool,
    pub z: bool,
    pub p: bool,
}

type PCOffset<'input> = Checked<'input, ImmOrLabel<'input>>;

#[derive(Clone, Debug)]
pub enum Operands<'input> {
    Add { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Checked<'input, Sr2OrImm5<'input>> },
    And { dr: Reg<'input>, sr1: Reg<'input>, sr2_or_imm5: Checked<'input, Sr2OrImm5<'input>> },
    Br { pc_offset9: PCOffset<'input> },
    Jmp { base: Reg<'input> },
    Jsr { pc_offset11: PCOffset<'input> },
    Jsrr { base: Reg<'input> },
    Ld { dr: Reg<'input>, pc_offset9: PCOffset<'input> },
    Ldi { dr: Reg<'input>, pc_offset9: PCOffset<'input> },
    Ldr { dr: Reg<'input>, base: Reg<'input>, offset6: Immediate<'input, SignedWord> },
    Lea { dr: Reg<'input>, pc_offset9: PCOffset<'input> },
    Not { dr: Reg<'input>, sr: Reg<'input> },
    Ret,
    Rti,
    St { sr: Reg<'input>, pc_offset9: PCOffset<'input> },
    Sti { sr: Reg<'input>, pc_offset9: PCOffset<'input> },
    Str { sr: Reg<'input>, base: Reg<'input>, offset6: Immediate<'input, SignedWord> },
    Trap { trap_vec: Immediate<'input, u8> },

    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,

    Orig { origin: Immediate<'input, Addr> },
    Fill { value: Immediate<'input, Word> },
    Blkw { size_src: Token<'input>, size: Immediate<'input, Addr> }, // Addr used here to signify a number of locations. Max is number of possible Addrs.
    Stringz { string: Checked<'input, String> },
    End,
}

pub struct CstParser {
    pub leniency: LeniencyLevel,
}

impl CstParser {

    pub fn parse_cst<'input>(&self, file: UnvalidatedFile<'input>) -> File<'input> {
        let UnvalidatedFile { objects, ignored } = file;
        File {
            objects: objects.into_iter().map(|o| self.validate_object(o)).collect(),
            ignored
        }
    }

    fn validate_object<'input>(&self, object: UnvalidatedObject<'input>) -> Object<'input> {
        let UnvalidatedObject { origin_src, origin, content } = object;
        let UnvalidatedObjectContent { operations, empty_lines, hanging_labels, invalid_lines } = content;
        Object {
            origin_src: self.validate_line(origin_src),
            origin: self.validate_numeric_immediate(origin),
            content: ObjectContent {
                operations: operations.into_iter().map(|o| self.validate_line(o)).collect(),
                empty_lines,
                hanging_labels,
                invalid_lines
            }
        }
    }

    fn validate_line<'input>(&self, line: UnvalidatedLine<'input>) -> Operation<'input> {
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
            label: label.map(|l| self.validate_label(l)),
            operator,
            nzp: self.validate_condition_codes(&operator),
            operands: self.validate_operand_tokens(operands),
            separators,
            whitespace,
            comments,
            newlines
        }
    }

    fn validate_operand_tokens<'input>(&self, operands: OperandTokens<'input>) -> Operands<'input> {
        match operands {
            OperandTokens::Add { dr, sr1, sr2_or_imm5 } =>
                Operands::Add {
                    dr: self.validate_reg(dr),
                    sr1: self.validate_reg(sr1),
                    sr2_or_imm5: self.validate_sr2_or_imm5(sr2_or_imm5)
                },
            OperandTokens::And { dr, sr1, sr2_or_imm5 } =>
                Operands::And {
                    dr: self.validate_reg(dr),
                    sr1: self.validate_reg(sr1),
                    sr2_or_imm5: self.validate_sr2_or_imm5(sr2_or_imm5)
                },
            OperandTokens::Br { label } => {
                Operands::Br { pc_offset9: self.validate_imm_or_label(label, 9), }
            },
            OperandTokens::Jmp { base } => Operands::Jmp { base: self.validate_reg(base) },
            OperandTokens::Jsr { label } => Operands::Jsr { pc_offset11: self.validate_imm_or_label(label, 11) },
            OperandTokens::Jsrr { base } => Operands::Jsrr { base: self.validate_reg(base) },
            OperandTokens::Ld { dr, label } => Operands::Ld { dr: self.validate_reg(dr), pc_offset9: self.validate_imm_or_label(label, 9) },
            OperandTokens::Ldi { dr, label } => Operands::Ldi { dr: self.validate_reg(dr), pc_offset9: self.validate_imm_or_label(label, 9) },
            OperandTokens::Ldr { dr, base, offset6 } =>
                Operands::Ldr {
                    dr: self.validate_reg(dr),
                    base: self.validate_reg(base),
                    offset6: self.validate_signed_immediate(offset6, 6),
                },
            OperandTokens::Lea { dr, label } => Operands::Lea { dr: self.validate_reg(dr), pc_offset9: self.validate_imm_or_label(label, 9) },
            OperandTokens::Not { dr, sr } => Operands::Not { dr: self.validate_reg(dr), sr: self.validate_reg(sr) },
            OperandTokens::Ret => Operands::Ret,
            OperandTokens::Rti => Operands::Rti,
            OperandTokens::St { sr, label } => Operands::St { sr: self.validate_reg(sr), pc_offset9: self.validate_imm_or_label(label, 9) },
            OperandTokens::Sti { sr, label } => Operands::Sti { sr: self.validate_reg(sr), pc_offset9: self.validate_imm_or_label(label, 9) },
            OperandTokens::Str { sr, base, offset6 } =>
                Operands::Str {
                    sr: self.validate_reg(sr),
                    base: self.validate_reg(base),
                    offset6: self.validate_signed_immediate(offset6, 6),
                },
            OperandTokens::Trap { trap_vec } => Operands::Trap { trap_vec: self.validate_numeric_immediate(trap_vec) },

            OperandTokens::Getc => Operands::Getc,
            OperandTokens::Out => Operands::Out,
            OperandTokens::Puts => Operands::Puts,
            OperandTokens::In => Operands::In,
            OperandTokens::Putsp => Operands::Putsp,
            OperandTokens::Halt => Operands::Halt,

            OperandTokens::Orig { origin } => Operands::Orig { origin: self.validate_numeric_immediate(origin) },
            OperandTokens::Fill { value } => Operands::Fill { value: self.validate_numeric_immediate(value) },
            OperandTokens::Blkw { size } => Operands::Blkw { size_src: size, size: self.validate_blkw_immediate(size) },
            OperandTokens::Stringz { string } => Operands::Stringz { string: self.validate_string(string) },
            OperandTokens::End => Operands::End,
        }
    }

    fn validate_sr2_or_imm5<'input>(&self, src: Token<'input>) -> Checked<'input, Sr2OrImm5<'input>> {
        let reg = self.validate_reg(src);
        let imm5 = self.validate_signed_immediate(src, 5);
        let value = if let Reg { value: Ok(_), .. } = reg {
            Ok(Sr2OrImm5::Sr2(reg))
        } else if let Immediate { value: Ok(_), .. } = imm5 {
            Ok(Sr2OrImm5::Imm5(imm5))
        } else {
            Err(ParseError::Misc("Invalid as register and as 5-bit immediate.".to_string()))
        };
        Checked { src, value }
    }

    fn validate_reg<'input>(&self, src: Token<'input>) -> Reg<'input> {
        let value = if let Some("r") | Some("R") = src.src.get(..=0) {
            src.src.get(1..)
                .filter(|s| s.len() == 1)
                .and_then(|s| s.parse::<u8>().ok())
                .and_then(|i| i.try_into().ok())
                .ok_or(ParseError::InvalidReg {
                    range: src.span,
                    reason: InvalidRegReason::Number,
                })
        } else {
            Err(ParseError::InvalidReg {
                range: src.span,
                reason: InvalidRegReason::FirstChar,
            })
        };
        Reg { src, value }
    }

    fn validate_numeric_immediate<'input, T: Num>(&self, src: Token<'input>) -> Immediate<'input, T> {
        let Token { src: str, span, .. } = src;
        let value = if let Some(str_head) = str.get(..=0) {
            let radix = match str_head {
                "b" => Some(2),
                "#" => Some(10),
                "x" => Some(16),
                _ => None
            };
            if let Some(radix) = radix {
                if let Some(src_tail) = src.src.get(1..) {
                    T::from_str_radix(src_tail, radix)
                        .map_err(|_| InvalidImmediateReason::Number { actual: src_tail.to_string() })
                } else {
                    Err(InvalidImmediateReason::NoNumber)
                }
            } else {
                Err(InvalidImmediateReason::RadixChar { actual: str_head.to_string() })
            }
        } else {
            Err(InvalidImmediateReason::NoChars)
        }.map_err(|reason| ParseError::InvalidImmediate {
            range: span,
            reason
        });

        Immediate { src, value }
    }

    fn validate_signed_immediate<'input>(&self, src: Token<'input>, num_bits: u32) -> Immediate<'input, SignedWord> {
        let Immediate { src, value } = self.validate_numeric_immediate(src);
        let value = value.ok()
            .filter(|&i| check_signed_imm(i, num_bits))
            .ok_or(ParseError::Misc("Invalid signed word immediate".to_string()));
        Immediate { src, value }
    }
    
    fn validate_imm_or_label<'input>(&self, src: Token<'input>, num_bits: u32) -> Checked<'input, ImmOrLabel<'input>>  {
        let label = self.validate_label(src);
        let imm = self.validate_signed_immediate(src, num_bits);
        let value = if let Label { value: Ok(_), .. } = label {
            Ok(ImmOrLabel::Label(label))
        } else if let Immediate { value: Ok(_), .. } = imm {
            Ok(ImmOrLabel::Imm(imm))
        } else {
            Err(ParseError::Misc("Invalid as label and as immediate.".to_string()))
        };
        Checked { src, value }
    }

    fn validate_label<'input>(&self, src: Token<'input>) -> Label<'input> {
        let label = src.src;

        let length = label.len();
        let valid_length = if self.leniency.long_labels_allowed() {
            length >= 1
        } else {
            (1..=20).contains(&length)  
        };

        let mut chars = label.chars();
        let first_char = chars.next();
        let first_char_alphabetic = first_char.filter(|c| c.is_alphabetic()).is_some();

        let mut other_chars = chars.collect::<Vec<_>>();
        other_chars.retain(|&c| !(c.is_alphanumeric() || c == '_'));
        let other_chars_alphanumeric = other_chars.len() == 0;

        let mut invalidation_reasons = Vec::new();
        if !valid_length {
            invalidation_reasons.push(InvalidLabelReason::Length { actual: length.clone() });
        }
        if !first_char_alphabetic {
            invalidation_reasons.push(InvalidLabelReason::FirstChar { actual: first_char });
        }
        if !other_chars_alphanumeric {
            invalidation_reasons.push(InvalidLabelReason::OtherChars { actual: other_chars.into_iter().collect::<String>() });
        }
        
        let value = if invalidation_reasons.len() == 0 {
            Ok(label)
        } else {
            Err(ParseError::InvalidLabel {
                range: src.span,
                reasons: invalidation_reasons,
            })
        };

        Label { src, value }
    }

    fn validate_condition_codes(&self, src: &Token) -> Result<Option<ConditionCodes>, ParseError> {
        let str = src.src;
        if str.to_uppercase().starts_with("BR") {
            let mut n = false;
            let mut z = false;
            let mut p = false;
            for c in str[2..].to_lowercase().chars() {
                match c {
                    // TODO: prettify with macro or non-iterative solution
                    'n' => {
                        if n { return Err(ParseError::Misc("Duplicate condition code n.".to_string())); }
                        n = true;
                    },
                    'z' => {
                        if z { return Err(ParseError::Misc("Duplicate condition code z.".to_string())); }
                        z = true;
                    },
                    'p' => {
                        if p { return Err(ParseError::Misc("Duplicate condition code p.".to_string())); }
                        p = true;
                    },
                    _ => { return Err(ParseError::Misc("Invalid condition codes.".to_string())) },
                }
            }
            Ok(Some(ConditionCodes { n, z, p }))
        } else {
            Ok(None)
        }
    }

    fn validate_blkw_immediate<'input>(&self, src: Token<'input>) -> Immediate<'input, Addr> {
        Immediate {
            src,
            value: src.src.parse().map_err(|_| ParseError::Misc("Invalid BLKW immediate.".to_string()))
        }
    }
    
    fn validate_string<'input>(&self, src: Token<'input>) -> Checked<'input, String> {
        let mut string = src.src.to_string();
        // remove start and end quote
        string.pop();
        string.remove(0);
        // remove escape characters
        string = string
            .replace(r#"\""#, r#"""#)
            .replace(r#"\\"#, r#"\"#);
        let value = Ok(string);
        Checked { src, value }
    }
}

