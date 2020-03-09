use std::fmt::{Display, Formatter, Result};
use crate::lexer::{Token, Span};
use annotate_snippets::snippet::{Snippet, Annotation, Slice, SourceAnnotation, AnnotationType};

use ParseError::*;
use itertools::Itertools;
use annotate_snippets::display_list::DisplayMarkType::AnnotationStart;

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
        }
    }
    
    fn annotations(&self) -> Vec<SourceAnnotation> {
        let mut annotations = Vec::new();
        match self {
            InvalidLabel { range, .. } => {
                annotations.push(
                    SourceAnnotation {
                        range: range.clone(),
                        label: "invalid label here".to_string(),
                        annotation_type: AnnotationType::Error,
                    }
                );
            },
            InvalidReg { range, .. } => {
                annotations.push(
                    SourceAnnotation {
                        range: range.clone(),
                        label: "invalid reg here".to_string(),
                        annotation_type: AnnotationType::Error,
                    }
                )
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
                        line_start: 0,
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
