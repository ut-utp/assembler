#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryError(pub String);
