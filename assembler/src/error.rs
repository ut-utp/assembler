use std::fmt::{Display, Formatter, Result};
use crate::lexer::Span;
use annotate_snippets::snippet::{Snippet, Annotation, Slice, SourceAnnotation, AnnotationType};

use ParseError::*;
use itertools::Itertools;
use crate::cst;
use crate::cst::{Object, ObjectContent, Operation, Operands};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    Unknown,
}


#[derive(Debug, Clone)]
pub enum ParseError {
    InvalidReg {
        range: Span,
        reason: InvalidRegReason
    },
    InvalidLabel { 
        range: Span,
        reasons: Vec<InvalidLabelReason>,
    },
    HangingLabel {
        range: Span,
    },
    InvalidLine {
        range: Option<Span>,
    },
    Misc(String),
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

impl<'input> ParseError {
    fn message(&self) -> String {
        match self {
            InvalidLabel { reasons, .. } => {
                format!("invalid label, reasons -- {}", reasons.iter().map(InvalidLabelReason::to_string).join(", "))
            },
            InvalidReg { reason, .. } => {
                format!("invalid register, {}", reason.to_string())
            }
            Misc(message) => message.clone(),
            HangingLabel { .. } => { format!("hanging label") }
            InvalidLine  { .. } => { format!("invalid line")  }
        }
    }
    
    fn annotations(&self) -> Vec<SourceAnnotation> {
        let mut annotations = Vec::new();
        
        macro_rules! push_annotation {
            ($range:expr, $label:expr) => {
                annotations.push(
                    SourceAnnotation {
                        range: $range.clone(),
                        label: $label.to_string(),
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
            Misc(_) => {},
        }
        annotations
    }
    
    fn slices(&self, source: String, origin: Option<String>) -> Vec<Slice> {
        let mut slices = Vec::new();
        match self {
            Misc(_) => {},
            _ => {
                slices.push(
                    Slice {
                        source,
                        origin,
                        line_start: 1,
                        fold: true,
                        annotations: self.annotations(),
                    }
                );
            },
        }
        slices
    }

    pub fn create_snippet(&self, source: String, origin: Option<String>) -> Snippet {
        Snippet {
            title: Some(Annotation {
                label: Some(self.message()),
                id: None,
                annotation_type: AnnotationType::Error
            }),
            footer: vec![],
            slices: self.slices(source, origin),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryError(pub String);

pub fn extract_file_errors(cst: cst::File) -> Vec<ParseError> {
    let mut errors = Vec::new();

    let cst::File { objects, .. } = cst;
    if objects.len() == 0 {
        errors.push(ParseError::Misc("File contained no objects.".to_string()));
    }

    for object in objects {
        errors.extend(extract_object_errors(object))
    }

    errors
}

fn extract_object_errors(object: Object) -> Vec<ParseError> {
    let mut errors = Vec::new();

    let Object { origin, content, .. } = object;

    origin.extract_error_into(&mut errors);
    errors.extend(extract_object_content_errors(content));

    errors
}

fn extract_object_content_errors(object_content: ObjectContent) -> Vec<ParseError> {
    let mut errors = Vec::new();

    let ObjectContent { operations, hanging_labels, invalid_lines, .. } = object_content;

    for operation in operations {
        errors.extend(extract_operation_errors(operation));
    }

    for hanging_label in hanging_labels {
        let range = hanging_label.span().unwrap();
        errors.push(ParseError::HangingLabel { range });
    }

    for invalid_line in invalid_lines {
        let range = invalid_line.span();
        errors.push(ParseError::InvalidLine { range });
    }

    errors
}

fn extract_operation_errors(operation: Operation) -> Vec<ParseError> {
    let mut errors = Vec::new();

    let Operation { label, operands, nzp, .. } = operation;

    if let Some(label) = label {
        label.extract_error_into(&mut errors);
    }

    errors.extend(extract_operands_errors(operands));
    
    if let Err(error) = nzp {
        errors.push(error);
    }

    errors
}

fn extract_operands_errors(operands: Operands) -> Vec<ParseError> {
    use Operands::*;

    let mut errors = Vec::new();
    match operands {
        Add { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(&mut errors);
            sr1.extract_error_into(&mut errors);
            sr2_or_imm5.extract_error_into(&mut errors);
        },
        And { dr, sr1, sr2_or_imm5 } => {
            dr.extract_error_into(&mut errors);
            sr1.extract_error_into(&mut errors);
            sr2_or_imm5.extract_error_into(&mut errors);
        },
        Br { pc_offset9 } => {
            pc_offset9.extract_error_into(&mut errors);
        },
        Jmp { base } => {
            base.extract_error_into(&mut errors);
        },
        Jsr { pc_offset11 } => {
            pc_offset11.extract_error_into(&mut errors);
        },
        Jsrr { base } => {
            base.extract_error_into(&mut errors);
        },
        Ld { dr, pc_offset9 } => {
            dr.extract_error_into(&mut errors);
            pc_offset9.extract_error_into(&mut errors);
        },
        Ldi { dr, pc_offset9 } => {
            dr.extract_error_into(&mut errors);
            pc_offset9.extract_error_into(&mut errors);
        },
        Ldr { dr, base, offset6 } => {
            dr.extract_error_into(&mut errors);
            base.extract_error_into(&mut errors);
            offset6.extract_error_into(&mut errors);
        },
        Lea { dr, pc_offset9 } => {
            dr.extract_error_into(&mut errors);
            pc_offset9.extract_error_into(&mut errors);
        },
        Not { dr, sr } => {
            dr.extract_error_into(&mut errors);
            sr.extract_error_into(&mut errors);
        },
        St { sr, pc_offset9 } => {
            sr.extract_error_into(&mut errors);
            pc_offset9.extract_error_into(&mut errors);
        }
        Sti { sr, pc_offset9 } => {
            sr.extract_error_into(&mut errors);
            pc_offset9.extract_error_into(&mut errors);
        }
        Str { sr, base, offset6 } => {
            sr.extract_error_into(&mut errors);
            base.extract_error_into(&mut errors);
            offset6.extract_error_into(&mut errors);
        }
        Trap { trap_vec } => {
            trap_vec.extract_error_into(&mut errors);
        }
        Orig { origin } => {
            origin.extract_error_into(&mut errors);
        }
        Fill { value } => {
            value.extract_error_into(&mut errors);
        }
        Blkw { size, .. } => {
            size.extract_error_into(&mut errors);
        }
        Stringz { .. } => {}

        // Putting these in instead of _ to avoid forgetting to change
        Ret
        | Rti
        | Getc
        | Out
        | Puts
        | In
        | Putsp
        | Halt
        | End => {}
    };

    errors
}
