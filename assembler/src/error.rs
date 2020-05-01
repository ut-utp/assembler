use std::fmt::{Display, Formatter, Result};
use crate::lexer::Span;
use annotate_snippets::snippet::{Snippet, Annotation, Slice, SourceAnnotation, AnnotationType};

use ParseError::*;
use itertools::Itertools;
use crate::ir::ir4_parse_ambiguous_tokens;
use crate::ir::ir4_parse_ambiguous_tokens::{Object, ObjectContent, Operation, Operands};
use lc3_isa::SignedWord;
use crate::ir::ir2_parse_line_syntax::LineContent::Invalid;
use annotate_snippets::display_list::FormatOptions;
use crate::analysis::memory_placement::MemoryPlacementError;
use crate::analysis::symbol_table::SymbolTableError;
use crate::complete::ConstructInstructionError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    Unknown,
}

impl LexError {

    pub fn message(&self) -> String {
        match self {
            LexError::Unknown => "encountered unknown token when lexing",
        }.to_string()
    }

    pub fn annotations(&self) -> Vec<SourceAnnotation> {
        match self {
            LexError::Unknown => vec![],
        }
    }

    pub fn should_show(&self) -> bool {
        match self {
            LexError::Unknown => true,
        }
    }
}


#[derive(Debug, Clone)]
pub enum ParseError {
    NoObjects,
    InvalidReg {
        range: Span,
        reason: InvalidRegReason
    },
    InvalidLabel { 
        range: Span,
        reasons: Vec<InvalidLabelReason>,
    },
    InvalidImmediate {
        range: Span,
        reason: InvalidImmediateReason
    },
    HangingLabel {
        range: Span,
    },
    InvalidLine {
        range: Option<Span>,
    },
    InvalidRegOrImm5 {
        range: Span,
        invalid_reg_reason: InvalidRegReason,
        invalid_imm5_reason: InvalidImmediateReason,
    },
    InvalidLabelOrImmediate {
        range: Span,
        invalid_label_reasons: Vec<InvalidLabelReason>,
        invalid_immediate_reason: InvalidImmediateReason,
    },
    Misc(String),
}

pub enum Error {
    Lex(LexError),
    Parse(ParseError),
    MemoryPlacement(MemoryPlacementError),
    SymbolTable(SymbolTableError),
    ConstructInstruction(ConstructInstructionError),
}

impl Error {
    pub fn message(&self) -> String {
        use Error::*;
        match self {
            Lex(error)                  => error.message(),
            Parse(error)                => error.message(),
            MemoryPlacement(error)      => error.message(),
            SymbolTable(error)          => error.message(),
            ConstructInstruction(error) => error.message(),
        }
    }

    pub fn annotations(&self) -> Vec<SourceAnnotation> {
        use Error::*;
        match self {
            Lex(error)                  => error.annotations(),
            Parse(error)                => error.annotations(),
            MemoryPlacement(error)      => error.annotations(),
            SymbolTable(error)          => error.annotations(),
            ConstructInstruction(error) => error.annotations(),
        }
    }

    pub fn should_show(&self) -> bool {
        use Error::*;
        match self {
            Lex(error)                  => error.should_show(),
            Parse(error)                => error.should_show(),
            MemoryPlacement(error)      => error.should_show(),
            SymbolTable(error)          => error.should_show(),
            ConstructInstruction(error) => error.should_show(),
        }
    }

}

// TODO: write macro for these From impls
impl From<LexError> for Error {
    fn from(error: LexError) -> Self {
        Error::Lex(error)
    }
}

impl From<&LexError> for Error {
    fn from(error: &LexError) -> Self {
        Error::Lex(error.clone())
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Self {
        Error::Parse(error)
    }
}

impl From<&ParseError> for Error {
    fn from(error: &ParseError) -> Self {
        Error::Parse(error.clone())
    }
}

impl From<MemoryPlacementError> for Error {
    fn from(error: MemoryPlacementError) -> Self {
        Error::MemoryPlacement(error)
    }
}

impl From<&MemoryPlacementError> for Error {
    fn from(error: &MemoryPlacementError) -> Self {
        Error::MemoryPlacement(error.clone())
    }
}

impl From<SymbolTableError> for Error {
    fn from(error: SymbolTableError) -> Self {
        Error::SymbolTable(error)
    }
}

impl From<&SymbolTableError> for Error {
    fn from(error: &SymbolTableError) -> Self {
        Error::SymbolTable(error.clone())
    }
}

impl From<ConstructInstructionError> for Error {
    fn from(error: ConstructInstructionError) -> Self {
        Error::ConstructInstruction(error)
    }
}

impl From<&ConstructInstructionError> for Error {
    fn from(error: &ConstructInstructionError) -> Self {
        Error::ConstructInstruction(error.clone())
    }
}

#[derive(Debug, Clone)]
pub enum InvalidRegReason {
    FirstChar,
    Number,
}

impl Display for InvalidRegReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use InvalidRegReason::*;
        match self {
            FirstChar => { write!(f, "didn't start with R") }
            Number => { write!(f, "didn't follow R with only 0-7") }
        }
    }
}

#[derive(Debug, Clone)]
pub enum InvalidImmediateReason {
    NoChars,
    RadixChar { actual: String },
    NoNumber,
    Number { actual: String },
    OutOfRange { value: SignedWord, num_bits: u32 },
}

impl Display for InvalidImmediateReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use InvalidImmediateReason::*;
        match self {
            NoChars => { write!(f, "didn't have any characters") }
            NoNumber => { write!(f, "didn't follow radix sign with number") }
            RadixChar { actual } => { write!(f, "didn't use valid radix sign (was: {})", actual) }
            Number { actual } => { write!(f, "couldn't parse number (was: {})", actual) }
            OutOfRange { value, num_bits } => { write!(f, "value {} can't be represented in {} bits", value, num_bits)}
        }
    }
}

#[derive(Debug, Clone)]
pub enum InvalidLabelReason {
    Length { actual: usize },
    FirstChar { actual: Option<char> },
    OtherChars { actual: String },
}

impl Display for InvalidLabelReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use InvalidLabelReason::*;
        match self {
            Length { actual } => { write!(f, "not between 1-20 chars (was: {})", actual) } 
            FirstChar { actual } => { write!(f, "first char not alphabetic (was: {:?})", actual) },
            OtherChars { actual } => { write!(f, "other chars not alphanumeric or underscores (bad chars: {})", actual) },
        }
    }
}

impl ParseError {
    pub fn message(&self) -> String {
        match self {
            InvalidLabel { reasons, .. } => {
                format!("invalid label, reasons -- {}", reasons.iter().map(InvalidLabelReason::to_string).join(", "))
            },
            InvalidReg { reason, .. } => {
                format!("invalid register, {}", reason)
            }
            Misc(message) => message.clone(),
            HangingLabel { .. } => { format!("hanging label") }
            InvalidLine  { .. } => { format!("invalid line")  }
            InvalidImmediate { reason, .. } => { format!("invalid immediate, {}", reason) }
            InvalidRegOrImm5 { invalid_reg_reason, invalid_imm5_reason, .. } => {
                format!("invalid register or 5-bit immediate,\n\
                         invalid as register because: {}\n\
                         invalid as immediate because: {}",
                        invalid_reg_reason, invalid_imm5_reason)
            }
            InvalidLabelOrImmediate { invalid_label_reasons, invalid_immediate_reason, .. } => {
                format!("invalid label or immediate,\n\
                         invalid as label because: {}\n\
                         invalid as immediate because: {}",
                        invalid_label_reasons.iter().map(InvalidLabelReason::to_string).join(", "),
                        invalid_immediate_reason)
            }
            NoObjects => {
                format!("no objects (.ORIG/.END blocks) found in file")
            }
        }
    }
    
    pub fn annotations(&self) -> Vec<SourceAnnotation> {
        let mut annotations = Vec::new();
        
        macro_rules! push_annotation {
            ($range:expr, $label:expr) => {
                annotations.push(
                    SourceAnnotation {
                        range: $range.clone(),
                        label: $label,
                        annotation_type: AnnotationType::Error,
                    }
                );
            }
        }
        match self {
            InvalidLabel { range, .. } => { push_annotation!(range, "invalid label here"); },
            InvalidReg   { range, .. } => { push_annotation!(range, "invalid reg here");   },
            HangingLabel { range }     => { push_annotation!(range, "hanging label here"); },
            InvalidLine { range } => {
                if let Some(range) = range {
                    push_annotation!(range, "invalid line here");
                }
            }
            InvalidImmediate { range, .. } => { push_annotation!(range, "invalid immediate here"); }
            InvalidRegOrImm5 { range, .. } => { push_annotation!(range, "invalid register or immediate here"); }
            InvalidLabelOrImmediate { range, .. } => { push_annotation!(range, "invalid label or immediate here"); }
            NoObjects
            | Misc(_) => {},
        }
        annotations
    }

    pub fn should_show(&self) -> bool {
        true
    }

}

