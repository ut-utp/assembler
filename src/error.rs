#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
}
