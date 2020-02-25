use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError(pub String);

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryError(pub String);
